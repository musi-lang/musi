use musi_ast::{
    AstArena, CondId, CondKind, Expr, ExprId, ExprKind, FnSig, Ident, LitKind, PatId, PatKind,
    Prog, StmtId, StmtKind, TyExpr, TyExprId, TyExprKind,
};
use musi_basic::diagnostic::{Diagnostic, DiagnosticBag};
use musi_basic::error::IntoMusiError;
use musi_basic::interner::Interner;
use musi_basic::span::Span;
use musi_lex::token::TokenKind;

use crate::builtins::Builtins;
use crate::error::SemaErrorKind;
use crate::semantic::SemanticModel;
use crate::symbol::{SymbolKind, SymbolTable};
use crate::ty_repr::{FloatWidth, IntWidth, TyRepr, TyReprKind};
use crate::unifier::Unifier;

#[must_use]
pub fn bind(
    arena: &AstArena,
    interner: &Interner,
    prog: &Prog,
    builtins: &Builtins,
) -> (SemanticModel, SymbolTable, DiagnosticBag) {
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

    fn finish(mut self) -> (SemanticModel, SymbolTable, DiagnosticBag) {
        self.finalize_types();
        (self.model, self.symbols, self.diags)
    }

    fn bind_prog(&mut self, prog: &Prog) {
        for stmt_id in &prog.stmts {
            self.bind_stmt(*stmt_id);
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
            ExprKind::Lit(lit) => Self::bind_expr_lit(lit),
            ExprKind::Ident(ident) => self.bind_expr_ident(*ident, expr.id, expr.span),
            ExprKind::Tuple(elems) => self.bind_expr_tuple(elems),
            ExprKind::Array(elems) => self.bind_expr_array(elems),
            ExprKind::Block { stmts, expr: tail } => self.bind_expr_block(stmts, *tail),
            ExprKind::Bind {
                pat,
                ty,
                init,
                mutable,
                ..
            } => self.bind_expr_bind(*pat, *ty, *init, *mutable),
            ExprKind::If {
                cond,
                then_br,
                else_br,
            } => self.bind_expr_if(*cond, *then_br, *else_br),
            ExprKind::While { cond, body } => self.bind_expr_while(*cond, *body),
            ExprKind::For { pat, iter, body } => self.bind_expr_for(*pat, *iter, *body),
            ExprKind::Return(opt) => self.bind_expr_return(*opt, expr.span),
            ExprKind::Break(opt) => self.bind_expr_break(*opt, expr.span),
            ExprKind::Cycle => self.bind_expr_cycle(expr.span),
            ExprKind::Call { callee, args } => self.bind_expr_call(*callee, args),
            ExprKind::Binary { op, lhs, rhs } => self.bind_expr_binary(*op, *lhs, *rhs),
            ExprKind::Unary { op, operand } => self.bind_expr_unary(*op, *operand, expr.span),
            ExprKind::Assign { target, value } => self.bind_expr_assign(*target, *value),
            ExprKind::Field { base, field } => self.bind_expr_field(*base, *field, expr.span),
            ExprKind::Index { base, index } => self.bind_expr_index(*base, *index, expr.span),
            ExprKind::Fn { sig, body, .. } => self.bind_expr_fn(sig, *body),
            ExprKind::RecordDef {
                name,
                fields,
                ty_params,
                ..
            } => self.bind_expr_record_def(*name, fields, ty_params),
            ExprKind::ChoiceDef {
                name,
                cases,
                ty_params,
                ..
            } => self.bind_expr_choice_def(*name, cases, ty_params),
            ExprKind::Alias { name, ty, .. } => self.bind_expr_alias(*name, *ty),
            ExprKind::Defer(inner) => {
                let _ = self.bind_expr(*inner);
                TyRepr::unit()
            }
            _ => TyRepr::any(),
        }
    }

    const fn bind_expr_lit(lit: &LitKind) -> TyRepr {
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

    fn bind_expr_ident(&mut self, ident: Ident, expr_id: ExprId, span: Span) -> TyRepr {
        if let Some(sym_id) = self.symbols.lookup(ident) {
            self.model.set_expr_symbol(expr_id, sym_id);
            if let Some(sym) = self.symbols.get(sym_id) {
                return sym.ty.clone();
            }
        }
        let name = self.interner.resolve(ident.id);
        self.error(SemaErrorKind::UndefinedIdent(name.to_owned()), span);
        TyRepr::error()
    }

    fn bind_expr_tuple(&mut self, elems: &[ExprId]) -> TyRepr {
        let types: Vec<_> = elems.iter().map(|e| self.bind_expr(*e)).collect();
        TyRepr::tuple(types)
    }

    fn bind_expr_array(&mut self, elems: &[ExprId]) -> TyRepr {
        if elems.is_empty() {
            return TyRepr::array(self.unifier.fresh_var(), Some(0));
        }
        let first_ty = self.bind_expr(elems[0]);
        for elem_id in &elems[1..] {
            let elem_ty = self.bind_expr(*elem_id);
            let span = self.arena.exprs.get(*elem_id).span;
            self.unify_or_err(&first_ty, &elem_ty, span);
        }
        TyRepr::array(first_ty, Some(elems.len()))
    }

    fn bind_expr_block(&mut self, stmts: &[StmtId], tail: Option<ExprId>) -> TyRepr {
        let _ = self.symbols.push_scope();
        for stmt_id in stmts {
            self.bind_stmt(*stmt_id);
        }
        let result = tail.map_or(TyRepr::unit(), |e| self.bind_expr(e));
        self.symbols.pop_scope();
        result
    }

    fn bind_expr_bind(
        &mut self,
        pat_id: PatId,
        ty_ann: Option<TyExprId>,
        init_id: ExprId,
        mutable: bool,
    ) -> TyRepr {
        let init_ty = self.bind_expr(init_id);
        let decl_ty = ty_ann.map(|id| self.resolve_ty_expr(id));

        let binding_ty = if let Some(decl) = decl_ty {
            let span = self.arena.exprs.get(init_id).span;
            self.unify_or_err(&decl, &init_ty, span);
            decl
        } else {
            init_ty
        };

        // If init is a type definition or function, use appropriate symbol kind
        let kind = match self.arena.exprs.get(init_id).kind {
            ExprKind::RecordDef { .. } | ExprKind::ChoiceDef { .. } | ExprKind::Alias { .. } => {
                SymbolKind::Type
            }
            ExprKind::Fn { .. } => SymbolKind::Fn,
            _ => {
                if matches!(binding_ty.kind, TyReprKind::Fn(..)) {
                    SymbolKind::Fn
                } else {
                    SymbolKind::Local
                }
            }
        };

        self.bind_pat_with_kind(pat_id, &binding_ty, mutable, kind);
        TyRepr::unit()
    }

    fn bind_expr_if(
        &mut self,
        cond_id: CondId,
        then_id: ExprId,
        else_id: Option<ExprId>,
    ) -> TyRepr {
        let cond = self.arena.conds.get(cond_id);
        match &cond.kind {
            CondKind::Expr(expr_id) => {
                let cond_ty = self.bind_expr(*expr_id);
                let span = self.arena.exprs.get(*expr_id).span;
                self.unify_or_err(&cond_ty, &TyRepr::bool(), span);
            }
            CondKind::Case { pat, init, extra } => {
                let init_ty = self.bind_expr(*init);
                self.bind_pat(*pat, &init_ty, false);
                for extra_id in extra {
                    let ty = self.bind_expr(*extra_id);
                    let span = self.arena.exprs.get(*extra_id).span;
                    self.unify_or_err(&ty, &TyRepr::bool(), span);
                }
            }
        }

        let then_ty = self.bind_expr(then_id);
        if let Some(else_id) = else_id {
            let else_ty = self.bind_expr(else_id);
            let span = self.arena.exprs.get(else_id).span;
            self.unify_or_err(&then_ty, &else_ty, span);
            then_ty
        } else {
            TyRepr::unit()
        }
    }

    fn bind_expr_while(&mut self, cond_id: CondId, body_id: ExprId) -> TyRepr {
        let cond = self.arena.conds.get(cond_id);
        if let CondKind::Expr(expr_id) = &cond.kind {
            let cond_ty = self.bind_expr(*expr_id);
            let span = self.arena.exprs.get(*expr_id).span;
            self.unify_or_err(&cond_ty, &TyRepr::bool(), span);
        }
        let prev = self.in_loop;
        self.in_loop = true;
        let _ = self.bind_expr(body_id);
        self.in_loop = prev;
        TyRepr::unit()
    }

    fn bind_expr_for(&mut self, pat_id: PatId, iter_id: ExprId, body_id: ExprId) -> TyRepr {
        let iter_ty = self.bind_expr(iter_id);
        let elem_ty = match &iter_ty.kind {
            TyReprKind::Array(elem, ..) => (**elem).clone(),
            _ => self.unifier.fresh_var(),
        };

        let _ = self.symbols.push_scope();
        self.bind_pat(pat_id, &elem_ty, false);

        let prev = self.in_loop;
        self.in_loop = true;
        let _ = self.bind_expr(body_id);
        self.in_loop = prev;

        self.symbols.pop_scope();
        TyRepr::unit()
    }

    fn bind_expr_return(&mut self, opt: Option<ExprId>, span: Span) -> TyRepr {
        if !self.in_fn {
            self.error(SemaErrorKind::ReturnOutsideFn, span);
        }
        if let Some(id) = opt {
            let _ = self.bind_expr(id);
        }
        TyRepr::never()
    }

    fn bind_expr_break(&mut self, opt: Option<ExprId>, span: Span) -> TyRepr {
        if !self.in_loop {
            self.error(SemaErrorKind::BreakOutsideLoop, span);
        }
        if let Some(id) = opt {
            let _ = self.bind_expr(id);
        }
        TyRepr::never()
    }

    fn bind_expr_cycle(&mut self, span: Span) -> TyRepr {
        if !self.in_loop {
            self.error(SemaErrorKind::CycleOutsideLoop, span);
        }
        TyRepr::never()
    }

    fn bind_expr_call(&mut self, callee_id: ExprId, args: &[ExprId]) -> TyRepr {
        let callee_ty = self.bind_expr(callee_id);
        let arg_tys: Vec<_> = args.iter().map(|a| self.bind_expr(*a)).collect();

        match &callee_ty.kind {
            TyReprKind::Fn(params, ret) => {
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
                    let span = self.arena.exprs.get(callee_id).span;
                    self.unify_or_err(param, arg, span);
                }
                (**ret).clone()
            }
            TyReprKind::Any | TyReprKind::Unknown => callee_ty,
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

    fn bind_expr_binary(&mut self, op: TokenKind, lhs_id: ExprId, rhs_id: ExprId) -> TyRepr {
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
                let lhs_span = self.arena.exprs.get(lhs_id).span;
                self.unify_or_err(&lhs_ty, &TyRepr::bool(), lhs_span);

                let rhs_span = self.arena.exprs.get(rhs_id).span;
                self.unify_or_err(&rhs_ty, &TyRepr::bool(), rhs_span);

                TyRepr::bool()
            }

            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent => {
                let span = self.arena.exprs.get(rhs_id).span;
                self.unify_or_err(&lhs_ty, &rhs_ty, span);
                lhs_ty
            }

            _ => TyRepr::any(),
        }
    }

    fn bind_expr_unary(&mut self, op: TokenKind, operand_id: ExprId, span: Span) -> TyRepr {
        let operand_ty = self.bind_expr(operand_id);
        match op {
            TokenKind::KwNot => {
                self.unify_or_err(&operand_ty, &TyRepr::bool(), span);
                TyRepr::bool()
            }
            TokenKind::Minus => operand_ty,
            TokenKind::At => TyRepr::ptr(operand_ty),
            _ => TyRepr::any(),
        }
    }

    fn bind_expr_assign(&mut self, target_id: ExprId, value_id: ExprId) -> TyRepr {
        let target_ty = self.bind_expr(target_id);

        if let ExprKind::Ident(ident) = self.arena.exprs.get(target_id).kind
            && let Some(sym_id) = self.symbols.lookup(ident)
            && let Some(sym) = self.symbols.get(sym_id)
            && !sym.mutable
        {
            let name = self.interner.resolve(ident.id);
            let span = self.arena.exprs.get(target_id).span;
            self.error(SemaErrorKind::AssignmentToImmutable(name.to_owned()), span);
        }

        let value_ty = self.bind_expr(value_id);
        let span = self.arena.exprs.get(value_id).span;
        self.unify_or_err(&target_ty, &value_ty, span);
        TyRepr::unit()
    }

    fn bind_expr_field(&mut self, base_id: ExprId, field: Ident, span: Span) -> TyRepr {
        let base_ty = self.bind_expr(base_id);
        match &base_ty.kind {
            TyReprKind::Any | TyReprKind::Unknown => base_ty,
            _ => {
                let field_name = self.interner.resolve(field.id);
                self.error(
                    SemaErrorKind::NoSuchField {
                        ty: format!("{base_ty}"),
                        field: field_name.to_owned(),
                    },
                    span,
                );
                TyRepr::error()
            }
        }
    }

    fn bind_expr_index(&mut self, base_id: ExprId, index_id: ExprId, span: Span) -> TyRepr {
        let base_ty = self.bind_expr(base_id);
        let _ = self.bind_expr(index_id);
        match &base_ty.kind {
            TyReprKind::Array(elem, ..) => (**elem).clone(),
            TyReprKind::Any | TyReprKind::Unknown => base_ty,
            _ => {
                self.error(SemaErrorKind::NotIndexable(format!("{base_ty}")), span);
                TyRepr::error()
            }
        }
    }

    fn bind_expr_fn(&mut self, sig: &FnSig, body_id: ExprId) -> TyRepr {
        let mut param_tys = vec![];
        for param in &sig.params {
            let ty = if let Some(id) = param.ty {
                self.resolve_ty_expr(id)
            } else {
                self.unifier.fresh_var()
            };
            param_tys.push(ty);
        }

        let _ = self.symbols.push_scope();

        for (param, ty) in sig.params.iter().zip(param_tys.iter()) {
            if self
                .symbols
                .define(
                    param.name,
                    SymbolKind::Local,
                    ty.clone(),
                    param.name.span,
                    param.mutable,
                )
                .is_ok()
            {
            } else {
                let name = self.interner.resolve(param.name.id);
                self.error(
                    SemaErrorKind::DuplicateDef(name.to_owned()),
                    param.name.span,
                );
            }
        }

        let prev = self.in_fn;
        self.in_fn = true;
        let body_ty = self.bind_expr(body_id);
        self.in_fn = prev;

        if let Some(ret_ty_expr) = sig.ret {
            let ret_ty = self.resolve_ty_expr(ret_ty_expr);
            self.unify_or_err(&ret_ty, &body_ty, self.arena.exprs.get(body_id).span);
        }

        self.symbols.pop_scope();
        TyRepr::func(param_tys, body_ty)
    }

    fn bind_expr_record_def(
        &mut self,
        name: Option<Ident>,
        fields: &musi_ast::Fields,
        _ty_params: &musi_ast::Idents,
    ) -> TyRepr {
        let ty = if let Some(ident) = name {
            let sym_id = self
                .symbols
                .define(
                    ident,
                    SymbolKind::Type,
                    TyRepr::unit(),
                    Span::default(),
                    false,
                )
                .expect("duplicate type def");
            TyRepr::named(sym_id, vec![])
        } else {
            TyRepr::unit()
        };

        for field in fields {
            if let Some(ty_expr) = field.ty {
                let _ = self.resolve_ty_expr(ty_expr);
            }
        }

        ty
    }

    fn bind_expr_choice_def(
        &mut self,
        name: Option<Ident>,
        cases: &[musi_ast::ChoiceCase],
        _ty_params: &musi_ast::Idents,
    ) -> TyRepr {
        let ty = if let Some(ident) = name {
            let sym_id = self
                .symbols
                .define(
                    ident,
                    SymbolKind::Type,
                    TyRepr::unit(),
                    Span::default(),
                    false,
                )
                .expect("duplicate type def");
            TyRepr::named(sym_id, vec![])
        } else {
            TyRepr::unit()
        };

        for case in cases {
            let _ = self
                .symbols
                .define(
                    case.name,
                    SymbolKind::Variant,
                    TyRepr::unit(),
                    Span::default(),
                    false,
                )
                .ok();
        }

        ty
    }

    fn bind_expr_alias(&mut self, name: Ident, ty_expr: TyExprId) -> TyRepr {
        let ty = self.resolve_ty_expr(ty_expr);
        let _ = self
            .symbols
            .define(name, SymbolKind::Type, ty.clone(), Span::default(), false)
            .ok();
        ty
    }

    fn bind_pat(&mut self, pat_id: PatId, expected: &TyRepr, mutable: bool) {
        self.bind_pat_with_kind(pat_id, expected, mutable, SymbolKind::Local);
    }

    fn bind_pat_with_kind(
        &mut self,
        pat_id: PatId,
        expected: &TyRepr,
        mutable: bool,
        kind: SymbolKind,
    ) {
        let pat = self.arena.pats.get(pat_id);
        self.model.set_pat_type(pat_id, expected.clone());

        match &pat.kind {
            PatKind::Ident(ident) => {
                match self
                    .symbols
                    .define(*ident, kind, expected.clone(), ident.span, mutable)
                {
                    Ok(sym_id) => self.model.set_pat_symbol(pat_id, sym_id),
                    Err(_prev) => {
                        let name = self.interner.resolve(ident.id);
                        self.error(SemaErrorKind::DuplicateDef(name.to_owned()), ident.span);
                    }
                }
            }
            PatKind::Tuple(pats) => {
                if let TyReprKind::Tuple(elem_tys) = &expected.kind {
                    for (sub_pat, elem_ty) in pats.iter().zip(elem_tys.iter()) {
                        self.bind_pat_with_kind(*sub_pat, elem_ty, mutable, kind);
                    }
                }
            }
            PatKind::Choice { name, args, .. } => {
                if let Some(sym_id) = self.symbols.lookup(*name) {
                    self.model.set_pat_symbol(pat_id, sym_id);
                }
                for arg in args {
                    self.bind_pat(*arg, &TyRepr::any(), false);
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
                    self.model.set_ty_expr_symbol(ty_expr.id, sym_id);
                    return sym.ty.clone();
                }
                let name = self.interner.resolve(ident.id);
                self.error(SemaErrorKind::UndefinedType(name.to_owned()), ident.span);
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
                    return TyRepr::named(sym_id, arg_tys);
                }
                let name = self.interner.resolve(base.id);
                self.error(SemaErrorKind::UndefinedType(name.to_owned()), base.span);
                TyRepr::error()
            }
        }
    }

    fn error(&mut self, kind: SemaErrorKind, span: Span) {
        let diag = Diagnostic::from(kind.into_musi_error(span));
        self.diags.add(diag);
    }

    fn unify_or_err(&mut self, a: &TyRepr, b: &TyRepr, span: Span) {
        if let Err(err) = self.unifier.unify(a, b) {
            self.error(err, span);
        }
    }
}

#[cfg(test)]
mod tests;
