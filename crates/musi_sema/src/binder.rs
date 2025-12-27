use musi_ast::{
    AstArena, CondId, CondKind, Expr, ExprId, ExprKind, Ident, LitKind, OptExprId, OptTyExprId,
    PatId, PatKind, Prog, StmtId, StmtKind, TyExpr, TyExprId, TyExprKind,
};
use musi_basic::diagnostic::{Diagnostic, DiagnosticBag};
use musi_basic::error::IntoMusiError;
use musi_basic::interner::Interner;
use musi_basic::span::Span;
use musi_lex::token::TokenKind;

use crate::builtins::Builtins;
use crate::error::SemaErrorKind;
use crate::model::SemanticModel;
use crate::symbol::{SymbolKind, SymbolTable};
use crate::ty_repr::{FloatWidth, IntWidth, TyRepr, TyReprKind};
use crate::unifier::Unifier;

#[must_use]
pub fn bind(
    arena: &AstArena,
    interner: &Interner,
    prog: &Prog,
    builtins: &Builtins,
) -> (SemanticModel, DiagnosticBag) {
    let mut binder = Binder::new(arena, interner, builtins);
    binder.bind_prog(prog);
    binder.finish()
}

struct Binder<'a> {
    arena: &'a AstArena,
    interner: &'a Interner,
    model: SemanticModel,
    symbols: SymbolTable,
    unifier: Unifier,
    diags: DiagnosticBag,
    in_loop: bool,
    in_fn: bool,
}

impl<'a> Binder<'a> {
    fn new(arena: &'a AstArena, interner: &'a Interner, builtins: &Builtins) -> Self {
        let model = SemanticModel::new(arena.exprs.len(), arena.pats.len(), arena.ty_exprs.len());
        let mut symbols = SymbolTable::new();
        builtins.register(&mut symbols);

        Self {
            arena,
            interner,
            model,
            symbols,
            unifier: Unifier::new(),
            diags: DiagnosticBag::default(),
            in_loop: false,
            in_fn: false,
        }
    }

    fn finish(mut self) -> (SemanticModel, DiagnosticBag) {
        self.finalize_types();
        (self.model, self.diags)
    }

    fn bind_prog(&mut self, prog: &Prog) {
        for stmt_id in &prog.stmts {
            self.bind_stmt(*stmt_id);
        }
    }

    fn bind_stmt(&mut self, stmt_id: StmtId) {
        let stmt = self.arena.stmts.get(stmt_id);
        match &stmt.kind {
            StmtKind::Expr(expr_id) => {
                let _ = self.bind_expr(*expr_id);
            }
        }
    }

    fn bind_expr(&mut self, expr_id: ExprId) -> TyRepr {
        let expr = self.arena.exprs.get(expr_id);
        let ty = self.bind_expr_inner(expr);
        self.model.set_expr_type(expr_id, ty.clone());
        ty
    }

    fn bind_expr_inner(&mut self, expr: &Expr) -> TyRepr {
        match &expr.kind {
            ExprKind::Lit(lit) => Self::bind_lit(lit),
            ExprKind::Ident(ident) => self.bind_ident_expr(*ident, expr.id, expr.span),
            ExprKind::Tuple(elems) => self.bind_tuple(elems),
            ExprKind::Array(elems) => self.bind_array(elems),
            ExprKind::Block { stmts, expr: tail } => self.bind_block(stmts, *tail),
            ExprKind::Bind { pat, ty, init, .. } => self.bind_val(*pat, *ty, *init),
            ExprKind::If {
                cond,
                then_br,
                else_br,
            } => self.bind_if(*cond, *then_br, *else_br),
            ExprKind::While { cond, body } => self.bind_while(*cond, *body),
            ExprKind::For { pat, iter, body } => self.bind_for(*pat, *iter, *body),
            ExprKind::Return(opt) => self.bind_return(*opt, expr.span),
            ExprKind::Break(opt) => self.bind_break(*opt, expr.span),
            ExprKind::Cycle => self.bind_cycle(expr.span),
            ExprKind::Call { callee, args } => self.bind_call(*callee, args),
            ExprKind::Binary { op, lhs, rhs } => self.bind_binary(*op, *lhs, *rhs),
            ExprKind::Unary { op, operand } => self.bind_unary(*op, *operand, expr.span),
            ExprKind::Assign { target, value } => self.bind_assign(*target, *value),
            ExprKind::Field { base, field } => self.bind_field(*base, *field, expr.span),
            ExprKind::Index { base, index } => self.bind_index(*base, *index, expr.span),
            ExprKind::Fn { body, .. } => self.bind_fn_expr(*body),
            ExprKind::RecordDef { .. } | ExprKind::ChoiceDef { .. } | ExprKind::Alias { .. } => {
                TyRepr::unit()
            }
            ExprKind::Defer(inner) => {
                let _ = self.bind_expr(*inner);
                TyRepr::unit()
            }
            _ => TyRepr::any(),
        }
    }

    const fn bind_lit(lit: &LitKind) -> TyRepr {
        match lit {
            LitKind::Int(n) => {
                if *n < 0 {
                    TyRepr::int(IntWidth::I32)
                } else {
                    TyRepr::nat(IntWidth::I32)
                }
            }
            LitKind::Real(_) => TyRepr::float(FloatWidth::F64),
            LitKind::String(_) | LitKind::Template(_) => TyRepr::string(),
            LitKind::Rune(_) => TyRepr::rune(),
            LitKind::Bool(_) => TyRepr::bool(),
        }
    }

    fn bind_ident_expr(&mut self, ident: Ident, expr_id: ExprId, span: Span) -> TyRepr {
        if let Some(sym_id) = self.symbols.lookup(ident) {
            self.model.set_expr_symbol(expr_id, sym_id);
            if let Some(sym) = self.symbols.get(sym_id) {
                return sym.ty.clone();
            }
        }
        let name = self.lookup_name(ident);
        self.error(SemaErrorKind::UndefinedIdent(name), span);
        TyRepr::error()
    }

    fn bind_tuple(&mut self, elems: &[ExprId]) -> TyRepr {
        let types: Vec<_> = elems.iter().map(|e| self.bind_expr(*e)).collect();
        TyRepr::tuple(types)
    }

    fn bind_array(&mut self, elems: &[ExprId]) -> TyRepr {
        if elems.is_empty() {
            return TyRepr::array(self.unifier.fresh_var(), Some(0));
        }
        let first_ty = self.bind_expr(elems[0]);
        for elem_id in &elems[1..] {
            let elem_ty = self.bind_expr(*elem_id);
            if let Err(err) = self.unifier.unify(&first_ty, &elem_ty) {
                let span = self.arena.exprs.get(*elem_id).span;
                self.error(err, span);
            }
        }
        TyRepr::array(first_ty, Some(elems.len()))
    }

    fn bind_block(&mut self, stmts: &[StmtId], tail: OptExprId) -> TyRepr {
        let _ = self.symbols.push_scope();
        for stmt_id in stmts {
            self.bind_stmt(*stmt_id);
        }
        let result = tail.map_or(TyRepr::unit(), |e| self.bind_expr(e));
        self.symbols.pop_scope();
        result
    }

    fn bind_val(&mut self, pat_id: PatId, ty_ann: OptTyExprId, init_id: ExprId) -> TyRepr {
        let init_ty = self.bind_expr(init_id);
        let decl_ty = ty_ann.map(|id| self.resolve_ty_expr(id));

        let binding_ty = if let Some(decl) = decl_ty {
            if let Err(err) = self.unifier.unify(&decl, &init_ty) {
                let span = self.arena.exprs.get(init_id).span;
                self.error(err, span);
            }
            decl
        } else {
            init_ty
        };

        self.bind_pat(pat_id, &binding_ty);
        TyRepr::unit()
    }

    fn bind_if(&mut self, cond_id: CondId, then_id: ExprId, else_id: OptExprId) -> TyRepr {
        let cond = self.arena.conds.get(cond_id);
        match &cond.kind {
            CondKind::Expr(expr_id) => {
                let cond_ty = self.bind_expr(*expr_id);
                if let Err(err) = self.unifier.unify(&cond_ty, &TyRepr::bool()) {
                    let span = self.arena.exprs.get(*expr_id).span;
                    self.error(err, span);
                }
            }
            CondKind::Case { pat, init, extra } => {
                let init_ty = self.bind_expr(*init);
                self.bind_pat(*pat, &init_ty);
                for extra_id in extra {
                    let ty = self.bind_expr(*extra_id);
                    if let Err(err) = self.unifier.unify(&ty, &TyRepr::bool()) {
                        let span = self.arena.exprs.get(*extra_id).span;
                        self.error(err, span);
                    }
                }
            }
        }

        let then_ty = self.bind_expr(then_id);
        if let Some(else_id) = else_id {
            let else_ty = self.bind_expr(else_id);
            if let Err(err) = self.unifier.unify(&then_ty, &else_ty) {
                let span = self.arena.exprs.get(else_id).span;
                self.error(err, span);
            }
            then_ty
        } else {
            TyRepr::unit()
        }
    }

    fn bind_while(&mut self, cond_id: CondId, body_id: ExprId) -> TyRepr {
        let cond = self.arena.conds.get(cond_id);
        if let CondKind::Expr(expr_id) = &cond.kind {
            let cond_ty = self.bind_expr(*expr_id);
            if let Err(err) = self.unifier.unify(&cond_ty, &TyRepr::bool()) {
                let span = self.arena.exprs.get(*expr_id).span;
                self.error(err, span);
            }
        }
        let prev = self.in_loop;
        self.in_loop = true;
        let _ = self.bind_expr(body_id);
        self.in_loop = prev;
        TyRepr::unit()
    }

    fn bind_for(&mut self, pat_id: PatId, iter_id: ExprId, body_id: ExprId) -> TyRepr {
        let iter_ty = self.bind_expr(iter_id);
        let elem_ty = match &iter_ty.kind {
            TyReprKind::Array { elem, .. } => (**elem).clone(),
            _ => self.unifier.fresh_var(),
        };

        let _ = self.symbols.push_scope();
        self.bind_pat(pat_id, &elem_ty);

        let prev = self.in_loop;
        self.in_loop = true;
        let _ = self.bind_expr(body_id);
        self.in_loop = prev;

        self.symbols.pop_scope();
        TyRepr::unit()
    }

    fn bind_return(&mut self, opt: OptExprId, span: Span) -> TyRepr {
        if !self.in_fn {
            self.error(SemaErrorKind::ReturnOutsideFn, span);
        }
        if let Some(id) = opt {
            let _ = self.bind_expr(id);
        }
        TyRepr::never()
    }

    fn bind_break(&mut self, opt: OptExprId, span: Span) -> TyRepr {
        if !self.in_loop {
            self.error(SemaErrorKind::BreakOutsideLoop, span);
        }
        if let Some(id) = opt {
            let _ = self.bind_expr(id);
        }
        TyRepr::never()
    }

    fn bind_cycle(&mut self, span: Span) -> TyRepr {
        if !self.in_loop {
            self.error(SemaErrorKind::CycleOutsideLoop, span);
        }
        TyRepr::never()
    }

    fn bind_call(&mut self, callee_id: ExprId, args: &[ExprId]) -> TyRepr {
        let callee_ty = self.bind_expr(callee_id);
        let arg_tys: Vec<_> = args.iter().map(|a| self.bind_expr(*a)).collect();

        match &callee_ty.kind {
            TyReprKind::Fn { params, ret } => {
                if params.len() != arg_tys.len() {
                    let span = self.arena.exprs.get(callee_id).span;
                    self.error(
                        SemaErrorKind::ArityMismatch {
                            expected: params.len(),
                            got: arg_tys.len(),
                        },
                        span,
                    );
                    return (**ret).clone();
                }
                for (param, arg) in params.iter().zip(arg_tys.iter()) {
                    if let Err(err) = self.unifier.unify(param, arg) {
                        let span = self.arena.exprs.get(callee_id).span;
                        self.error(err, span);
                    }
                }
                (**ret).clone()
            }
            TyReprKind::Any | TyReprKind::Error => callee_ty,
            _ => {
                let span = self.arena.exprs.get(callee_id).span;
                self.error(
                    SemaErrorKind::NotCallable {
                        callee: format!("{callee_ty}"),
                        ty: format!("{callee_ty}"),
                    },
                    span,
                );
                TyRepr::error()
            }
        }
    }

    fn bind_binary(&mut self, op: TokenKind, lhs_id: ExprId, rhs_id: ExprId) -> TyRepr {
        let lhs_ty = self.bind_expr(lhs_id);
        let rhs_ty = self.bind_expr(rhs_id);

        match op {
            TokenKind::Eq
            | TokenKind::SlashEq
            | TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::Gt
            | TokenKind::GtEq => TyRepr::bool(),

            TokenKind::KwAnd | TokenKind::KwOr => {
                if let Err(err) = self.unifier.unify(&lhs_ty, &TyRepr::bool()) {
                    let span = self.arena.exprs.get(lhs_id).span;
                    self.error(err, span);
                }
                if let Err(err) = self.unifier.unify(&rhs_ty, &TyRepr::bool()) {
                    let span = self.arena.exprs.get(rhs_id).span;
                    self.error(err, span);
                }
                TyRepr::bool()
            }

            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent => {
                if let Err(err) = self.unifier.unify(&lhs_ty, &rhs_ty) {
                    let span = self.arena.exprs.get(rhs_id).span;
                    self.error(err, span);
                }
                lhs_ty
            }

            _ => TyRepr::any(),
        }
    }

    fn bind_unary(&mut self, op: TokenKind, operand_id: ExprId, span: Span) -> TyRepr {
        let operand_ty = self.bind_expr(operand_id);
        match op {
            TokenKind::KwNot => {
                if let Err(err) = self.unifier.unify(&operand_ty, &TyRepr::bool()) {
                    self.error(err, span);
                }
                TyRepr::bool()
            }
            TokenKind::Minus => operand_ty,
            TokenKind::At => TyRepr::ptr(operand_ty),
            _ => TyRepr::any(),
        }
    }

    fn bind_assign(&mut self, target_id: ExprId, value_id: ExprId) -> TyRepr {
        let target_ty = self.bind_expr(target_id);
        let value_ty = self.bind_expr(value_id);
        if let Err(err) = self.unifier.unify(&target_ty, &value_ty) {
            let span = self.arena.exprs.get(value_id).span;
            self.error(err, span);
        }
        TyRepr::unit()
    }

    fn bind_field(&mut self, base_id: ExprId, field: Ident, span: Span) -> TyRepr {
        let base_ty = self.bind_expr(base_id);
        match &base_ty.kind {
            TyReprKind::Any | TyReprKind::Error => base_ty,
            _ => {
                let field_name = self.lookup_name(field);
                self.error(
                    SemaErrorKind::NoSuchField {
                        ty: format!("{base_ty}"),
                        field: field_name,
                    },
                    span,
                );
                TyRepr::error()
            }
        }
    }

    fn bind_index(&mut self, base_id: ExprId, index_id: ExprId, span: Span) -> TyRepr {
        let base_ty = self.bind_expr(base_id);
        let _ = self.bind_expr(index_id);
        match &base_ty.kind {
            TyReprKind::Array { elem, .. } => (**elem).clone(),
            TyReprKind::Any | TyReprKind::Error => base_ty,
            _ => {
                self.error(SemaErrorKind::NotIndexable(format!("{base_ty}")), span);
                TyRepr::error()
            }
        }
    }

    fn bind_fn_expr(&mut self, body_id: ExprId) -> TyRepr {
        let _ = self.symbols.push_scope();
        let prev = self.in_fn;
        self.in_fn = true;
        let ret_ty = self.bind_expr(body_id);
        self.in_fn = prev;
        self.symbols.pop_scope();
        TyRepr::func(vec![], ret_ty)
    }

    fn bind_pat(&mut self, pat_id: PatId, expected: &TyRepr) {
        let pat = self.arena.pats.get(pat_id);
        self.model.set_pat_type(pat_id, expected.clone());

        match &pat.kind {
            PatKind::Ident(ident) => {
                match self
                    .symbols
                    .define(*ident, SymbolKind::Local, expected.clone(), pat.span)
                {
                    Ok(sym_id) => self.model.set_pat_symbol(pat_id, sym_id),
                    Err(_prev) => {
                        let name = self.lookup_name(*ident);
                        self.error(SemaErrorKind::DuplicateDef(name), pat.span);
                    }
                }
            }
            PatKind::Tuple(pats) => {
                if let TyReprKind::Tuple(elem_tys) = &expected.kind {
                    for (sub_pat, elem_ty) in pats.iter().zip(elem_tys.iter()) {
                        self.bind_pat(*sub_pat, elem_ty);
                    }
                }
            }
            _ => {}
        }
    }

    fn resolve_ty_expr(&mut self, ty_id: TyExprId) -> TyRepr {
        let ty_expr = self.arena.ty_exprs.get(ty_id);
        let resolved = self.resolve_ty_expr_inner(ty_expr);
        self.model.set_ty_expr_type(ty_id, resolved.clone());
        resolved
    }

    fn resolve_ty_expr_inner(&mut self, ty_expr: &TyExpr) -> TyRepr {
        match &ty_expr.kind {
            TyExprKind::Ident(ident) => {
                if let Some(sym_id) = self.symbols.lookup(*ident)
                    && let Some(sym) = self.symbols.get(sym_id)
                    && matches!(sym.kind, SymbolKind::Builtin | SymbolKind::Type)
                {
                    return sym.ty.clone();
                }
                let name = self.lookup_name(*ident);
                self.error(SemaErrorKind::UndefinedType(name), ty_expr.span);
                TyRepr::error()
            }
            TyExprKind::Optional(inner) => TyRepr::optional(self.resolve_ty_expr(*inner)),
            TyExprKind::Ptr(inner) => TyRepr::ptr(self.resolve_ty_expr(*inner)),
            TyExprKind::Array { size, elem } => {
                let elem_ty = self.resolve_ty_expr(*elem);
                TyRepr::array(
                    elem_ty,
                    size.map(|s| usize::try_from(s).expect("size overflow")),
                )
            }
            TyExprKind::Tuple(elems) => {
                TyRepr::tuple(elems.iter().map(|e| self.resolve_ty_expr(*e)).collect())
            }
            TyExprKind::Fn { param, ret } => TyRepr::func(
                vec![self.resolve_ty_expr(*param)],
                self.resolve_ty_expr(*ret),
            ),
            TyExprKind::App { base, args } => {
                if let Some(sym_id) = self.symbols.lookup(*base) {
                    let arg_tys: Vec<_> = args.iter().map(|a| self.resolve_ty_expr(*a)).collect();
                    return TyRepr::new(TyReprKind::Named {
                        symbol: sym_id,
                        args: arg_tys,
                    });
                }
                let name = self.lookup_name(*base);
                self.error(SemaErrorKind::UndefinedType(name), ty_expr.span);
                TyRepr::error()
            }
        }
    }

    fn finalize_types(&mut self) {
        for idx in 0..self.arena.exprs.len() {
            let expr_id = ExprId::new(u32::try_from(idx).expect("index overflow"));
            if let Some(ty) = self.model.type_of_expr(expr_id).cloned() {
                let finalized = self.unifier.finalize(&ty);
                self.model.set_expr_type(expr_id, finalized);
            }
        }
    }

    fn error(&mut self, kind: SemaErrorKind, span: Span) {
        let diag = Diagnostic::from(kind.into_musi_error(span));
        self.diags.add(diag);
    }

    fn lookup_name(&self, ident: Ident) -> String {
        self.interner
            .lookup(ident)
            .unwrap_or("<unknown>")
            .to_owned()
    }
}
