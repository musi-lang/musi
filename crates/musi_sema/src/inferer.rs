use musi_ast::{
    AstArena, ChoiceCase, CondId, CondKind, ExprId, ExprKind, Field, FnSig, LitKind, MatchCase,
    PatId, PatKind, StmtId, StmtKind, TyExprId, TyExprKind,
};
use musi_core::{Interner, MusiResult, Span, Symbol, TokenKind};

use crate::errors;
use crate::table::UnificationTable;
use crate::ty::{TyArena, TyId, TyKind};
use crate::ty_env::TyEnv;
use crate::unifier::Unifier;

pub struct Inferer<'a> {
    ast: &'a AstArena,
    interner: &'a Interner,
    ty_arena: &'a mut TyArena,
    env: &'a mut TyEnv,
    table: &'a mut UnificationTable,
}

impl<'a> Inferer<'a> {
    #[must_use]
    pub const fn new(
        ast: &'a AstArena,
        interner: &'a Interner,
        ty_arena: &'a mut TyArena,
        env: &'a mut TyEnv,
        table: &'a mut UnificationTable,
    ) -> Self {
        Self {
            ast,
            interner,
            ty_arena,
            env,
            table,
        }
    }

    /// # Errors
    /// Returns error if type inference fails.
    pub fn infer_expr(&mut self, expr_id: ExprId) -> MusiResult<TyId> {
        let expr = self.ast.exprs.get(expr_id);
        let span = expr.span;

        tracing::debug!(?expr.kind, ?span, "infer_expr");

        match &expr.kind.clone() {
            ExprKind::Lit(lit) => Ok(self.infer_lit(lit, span)),
            ExprKind::Ident(sym) => self.infer_ident(*sym, span),
            ExprKind::Tuple(elems) => self.infer_tuple(elems, span),
            ExprKind::Array(elems) => self.infer_array(elems, span),
            ExprKind::Block { stmts, expr } => self.infer_block(stmts, *expr, span),
            ExprKind::Binary { op, lhs, rhs } => self.infer_binary(*op, *lhs, *rhs, span),
            ExprKind::Unary { op, operand } => self.infer_unary(*op, *operand, span),
            ExprKind::Call { callee, args } => self.infer_call(*callee, args, span),
            ExprKind::If {
                cond,
                then_br,
                else_br,
            } => self.infer_if(*cond, *then_br, *else_br, span),
            ExprKind::Binding {
                mutable,
                pat,
                ty,
                init,
                ..
            } => self.infer_binding(*mutable, *pat, *ty, *init, span),
            ExprKind::Fn { sig, body, .. } => self.infer_fn(sig, *body, span),
            ExprKind::Field { base, field } => self.infer_field(*base, *field, span),
            ExprKind::Index { base, index } => self.infer_index(*base, *index, span),
            ExprKind::Assign { target, value } => self.infer_assign(*target, *value, span),
            ExprKind::Return(val) => self.infer_return(*val, span),
            ExprKind::Break(_) | ExprKind::Cycle => Ok(self.alloc_ty(TyKind::Never, span)),
            ExprKind::While { body, .. } | ExprKind::For { body, .. } => {
                let _ = self.infer_expr(*body)?;
                Ok(self.alloc_ty(TyKind::Unit, span))
            }
            ExprKind::Match { scrutinee, cases } => self.infer_match(*scrutinee, cases, span),
            ExprKind::Record { base, fields } => self.infer_record(*base, fields, span),
            ExprKind::Deref(inner) => self.infer_deref(*inner, span),
            ExprKind::Propagate(inner) => self.infer_propagate(*inner, span),
            ExprKind::Force(inner) => self.infer_force(*inner, span),
            ExprKind::Range { start, end, .. } => self.infer_range(*start, *end, span),
            ExprKind::RecordDef { name, fields, .. } => {
                Ok(self.infer_record_def(*name, fields, span))
            }
            ExprKind::ChoiceDef { name, cases, .. } => {
                Ok(self.infer_choice_def(*name, cases, span))
            }
            ExprKind::TypeDef { name, .. } => Ok(self.infer_type_def(*name, span)),
            ExprKind::Defer(inner) | ExprKind::Unsafe(inner) => self.infer_expr(*inner),
            ExprKind::Import(_) => Ok(self.alloc_ty(TyKind::Unit, span)),
        }
    }

    fn infer_lit(&mut self, lit: &LitKind, span: Span) -> TyId {
        let kind = match lit {
            LitKind::Int(_) => TyKind::Int,
            LitKind::Real(_) => TyKind::Real,
            LitKind::String(_) | LitKind::Template(_) => TyKind::String,
            LitKind::Rune(_) => TyKind::Rune,
            LitKind::Bool(_) => TyKind::Bool,
        };
        self.alloc_ty(kind, span)
    }

    fn infer_ident(&self, sym: Symbol, span: Span) -> MusiResult<TyId> {
        self.env
            .lookup_value(sym)
            .map(|entry| entry.ty)
            .ok_or_else(|| errors::undefined_variable(sym, span))
    }

    fn infer_tuple(&mut self, elems: &[ExprId], span: Span) -> MusiResult<TyId> {
        let elem_tys: Vec<TyId> = elems
            .iter()
            .map(|e| self.infer_expr(*e))
            .collect::<MusiResult<Vec<_>>>()?;
        Ok(self.alloc_ty(TyKind::Tuple(elem_tys), span))
    }

    fn infer_array(&mut self, elems: &[ExprId], span: Span) -> MusiResult<TyId> {
        let elem_ty = if elems.is_empty() {
            self.fresh_var(span)
        } else {
            let first = self.infer_expr(elems[0])?;
            for elem in &elems[1..] {
                let ty = self.infer_expr(*elem)?;
                self.unify(first, ty, span)?;
            }
            first
        };
        Ok(self.alloc_ty(TyKind::Array(elem_ty), span))
    }

    fn infer_block(
        &mut self,
        stmts: &[StmtId],
        tail: Option<ExprId>,
        span: Span,
    ) -> MusiResult<TyId> {
        self.env.enter_scope();
        for stmt_id in stmts {
            let stmt = self.ast.stmts.get(*stmt_id);
            let StmtKind::Expr(expr_id) = &stmt.kind;
            let _ = self.infer_expr(*expr_id)?;
        }
        let block_ty = match tail {
            Some(expr) => self.infer_expr(expr)?,
            None => self.alloc_ty(TyKind::Unit, span),
        };
        self.env.exit_scope(span)?;
        Ok(block_ty)
    }

    fn infer_binary(
        &mut self,
        op: TokenKind,
        lhs: ExprId,
        rhs: ExprId,
        span: Span,
    ) -> MusiResult<TyId> {
        let lhs_ty = self.infer_expr(lhs)?;
        let rhs_ty = self.infer_expr(rhs)?;
        self.unify(lhs_ty, rhs_ty, span)?;
        Ok(self.result_type_for_binary_op(op, lhs_ty, span))
    }

    fn result_type_for_binary_op(&mut self, op: TokenKind, operand_ty: TyId, span: Span) -> TyId {
        match op {
            TokenKind::Lt
            | TokenKind::LtEq
            | TokenKind::Gt
            | TokenKind::GtEq
            | TokenKind::Eq
            | TokenKind::SlashEq
            | TokenKind::KwAnd
            | TokenKind::KwOr => self.alloc_ty(TyKind::Bool, span),
            _ => operand_ty,
        }
    }

    fn infer_unary(&mut self, op: TokenKind, operand: ExprId, span: Span) -> MusiResult<TyId> {
        let operand_ty = self.infer_expr(operand)?;
        match op {
            TokenKind::KwNot => Ok(self.alloc_ty(TyKind::Bool, span)),
            _ => Ok(operand_ty),
        }
    }

    fn infer_call(&mut self, callee: ExprId, args: &[ExprId], span: Span) -> MusiResult<TyId> {
        let callee_ty = self.infer_expr(callee)?;
        let arg_tys: Vec<TyId> = args
            .iter()
            .map(|a| self.infer_expr(*a))
            .collect::<MusiResult<Vec<_>>>()?;

        let callee_kind = &self.ty_arena.get(callee_ty).kind.clone();
        if let TyKind::Fn { params, ret } = callee_kind {
            if params.len() != arg_tys.len() {
                return Err(errors::arity_mismatch(params.len(), arg_tys.len(), span));
            }
            for (param_ty, arg_ty) in params.iter().zip(&arg_tys) {
                self.unify(*param_ty, *arg_ty, span)?;
            }
            Ok(*ret)
        } else {
            let ret_ty = self.fresh_var(span);
            let fn_ty = self.alloc_ty(
                TyKind::Fn {
                    params: arg_tys,
                    ret: ret_ty,
                },
                span,
            );
            self.unify(callee_ty, fn_ty, span)?;
            Ok(ret_ty)
        }
    }

    fn infer_if(
        &mut self,
        cond: CondId,
        then_br: ExprId,
        else_br: Option<ExprId>,
        span: Span,
    ) -> MusiResult<TyId> {
        let _ = self.infer_cond(cond)?;
        let then_ty = self.infer_expr(then_br)?;
        match else_br {
            Some(else_expr) => {
                let else_ty = self.infer_expr(else_expr)?;
                self.unify(then_ty, else_ty, span)?;
                Ok(then_ty)
            }
            None => Ok(self.alloc_ty(TyKind::Unit, span)),
        }
    }

    fn infer_cond(&mut self, cond_id: CondId) -> MusiResult<TyId> {
        let cond = self.ast.conds.get(cond_id);
        match &cond.kind.clone() {
            CondKind::Expr(expr_id) => {
                let ty = self.infer_expr(*expr_id)?;
                let bool_ty = self.alloc_ty(TyKind::Bool, Span::DUMMY);
                self.unify(ty, bool_ty, Span::DUMMY)?;
                Ok(ty)
            }
            CondKind::Case { pat, init, .. } => {
                let init_ty = self.infer_expr(*init)?;
                self.bind_pattern(*pat, init_ty)?;
                Ok(init_ty)
            }
        }
    }

    fn infer_binding(
        &mut self,
        mutable: bool,
        pat: PatId,
        ty_ann: Option<TyExprId>,
        init: ExprId,
        span: Span,
    ) -> MusiResult<TyId> {
        let expected_ty = match ty_ann {
            Some(ty_expr_id) => self.lower_ty_expr(ty_expr_id),
            None => self.fresh_var(span),
        };
        let init_ty = self.infer_expr(init)?;
        self.unify(init_ty, expected_ty, span)?;
        self.bind_pattern(pat, expected_ty)?;
        let pat_node = self.ast.pats.get(pat);
        if let PatKind::Ident(sym) = &pat_node.kind {
            self.env.bind_value(*sym, expected_ty, mutable, span);
        }
        Ok(self.alloc_ty(TyKind::Unit, span))
    }

    fn bind_pattern(&mut self, pat_id: PatId, ty: TyId) -> MusiResult<()> {
        let pat = self.ast.pats.get(pat_id);
        let span = pat.span;
        match &pat.kind.clone() {
            PatKind::Ident(sym) => {
                self.env.bind_value(*sym, ty, false, span);
                Ok(())
            }
            PatKind::Wild | PatKind::Lit(_) => Ok(()),
            PatKind::Tuple(pats) => self.bind_tuple_pat(pats, ty, span),
            PatKind::Array(pats) | PatKind::Cons(pats) => self.bind_seq_pat(pats, ty),
            PatKind::Record { fields, .. } => {
                self.bind_record_pat(fields, ty, span);
                Ok(())
            }
            PatKind::Variant { args, .. } => self.bind_variant_pat(args, ty, span),
            PatKind::Or(alts) => self.bind_or_pat(alts, ty, span),
            PatKind::As { inner, binding } => {
                self.env.bind_value(*binding, ty, false, span);
                self.bind_pattern(*inner, ty)
            }
        }
    }

    fn bind_tuple_pat(&mut self, pats: &[PatId], ty: TyId, span: Span) -> MusiResult<()> {
        let ty_kind = &self.ty_arena.get(ty).kind.clone();
        if let TyKind::Tuple(elem_tys) = ty_kind {
            if pats.len() != elem_tys.len() {
                return Err(errors::pattern_arity_mismatch(
                    elem_tys.len(),
                    pats.len(),
                    span,
                ));
            }
            for (p, t) in pats.iter().zip(elem_tys) {
                self.bind_pattern(*p, *t)?;
            }
        }
        Ok(())
    }

    fn bind_seq_pat(&mut self, pats: &[PatId], ty: TyId) -> MusiResult<()> {
        let ty_kind = &self.ty_arena.get(ty).kind.clone();
        if let TyKind::Array(elem_ty) = ty_kind {
            for p in pats {
                self.bind_pattern(*p, *elem_ty)?;
            }
        }
        Ok(())
    }

    fn bind_record_pat(&mut self, fields: &[Symbol], ty: TyId, span: Span) {
        let ty_kind = &self.ty_arena.get(ty).kind.clone();
        if let TyKind::Record { fields: field_tys } = ty_kind {
            for field_name in fields {
                let field_ty = match field_tys.iter().find(|(n, _)| n == field_name) {
                    Some((_, t)) => *t,
                    None => self.fresh_var(span),
                };
                self.env.bind_value(*field_name, field_ty, false, span);
            }
        }
    }

    fn bind_variant_pat(&mut self, args: &[PatId], ty: TyId, span: Span) -> MusiResult<()> {
        for arg in args {
            let arg_ty = self.fresh_var(span);
            self.bind_pattern(*arg, arg_ty)?;
        }
        let _ = ty;
        Ok(())
    }

    fn bind_or_pat(&mut self, alts: &[PatId], ty: TyId, _span: Span) -> MusiResult<()> {
        for alt in alts {
            self.bind_pattern(*alt, ty)?;
        }
        Ok(())
    }

    fn infer_fn(&mut self, sig: &FnSig, body: ExprId, span: Span) -> MusiResult<TyId> {
        self.env.enter_scope();
        let param_tys: Vec<TyId> = sig
            .params
            .iter()
            .map(|p| {
                let ty = self.fresh_var(span);
                self.env.bind_value(p.name, ty, p.mutable, span);
                ty
            })
            .collect();

        let body_ty = self.infer_expr(body)?;
        self.env.exit_scope(span)?;

        let fn_ty = self.alloc_ty(
            TyKind::Fn {
                params: param_tys,
                ret: body_ty,
            },
            span,
        );
        if let Some(name) = sig.name {
            self.env.bind_value(name, fn_ty, false, span);
        }
        Ok(fn_ty)
    }

    fn infer_field(&mut self, base: ExprId, field: Symbol, span: Span) -> MusiResult<TyId> {
        let base_ty = self.infer_expr(base)?;
        let base_kind = &self.ty_arena.get(base_ty).kind.clone();
        if let TyKind::Record { fields } = base_kind {
            for (name, ty) in fields {
                if *name == field {
                    return Ok(*ty);
                }
            }
            Err(errors::unknown_field(field, base_kind, span))
        } else {
            Err(errors::field_on_non_record(span))
        }
    }

    fn infer_index(&mut self, base: ExprId, index: ExprId, span: Span) -> MusiResult<TyId> {
        let base_ty = self.infer_expr(base)?;
        let index_ty = self.infer_expr(index)?;
        let int_ty = self.alloc_ty(TyKind::Int, span);
        self.unify(index_ty, int_ty, span)?;

        let base_kind = &self.ty_arena.get(base_ty).kind.clone();
        if let TyKind::Array(elem_ty) = base_kind {
            Ok(*elem_ty)
        } else {
            let elem_ty = self.fresh_var(span);
            let arr_ty = self.alloc_ty(TyKind::Array(elem_ty), span);
            self.unify(base_ty, arr_ty, span)?;
            Ok(elem_ty)
        }
    }

    fn infer_assign(&mut self, target: ExprId, value: ExprId, span: Span) -> MusiResult<TyId> {
        let target_ty = self.infer_expr(target)?;
        let value_ty = self.infer_expr(value)?;
        self.unify(target_ty, value_ty, span)?;
        Ok(self.alloc_ty(TyKind::Unit, span))
    }

    fn infer_return(&mut self, val: Option<ExprId>, span: Span) -> MusiResult<TyId> {
        if let Some(expr) = val {
            let _ = self.infer_expr(expr)?;
        }
        Ok(self.alloc_ty(TyKind::Never, span))
    }

    fn infer_match(
        &mut self,
        scrutinee: ExprId,
        cases: &[MatchCase],
        span: Span,
    ) -> MusiResult<TyId> {
        let scrutinee_ty = self.infer_expr(scrutinee)?;
        let mut result_ty: Option<TyId> = None;

        for case in cases {
            self.env.enter_scope();
            self.bind_pattern(case.pat, scrutinee_ty)?;
            if let Some(guard) = case.guard {
                let guard_ty = self.infer_expr(guard)?;
                let bool_ty = self.alloc_ty(TyKind::Bool, span);
                self.unify(guard_ty, bool_ty, span)?;
            }
            let body_ty = self.infer_expr(case.body)?;
            self.env.exit_scope(span)?;

            match result_ty {
                Some(prev) => self.unify(prev, body_ty, span)?,
                None => result_ty = Some(body_ty),
            }
        }
        result_ty.ok_or_else(|| errors::empty_match(span))
    }

    fn infer_record(
        &mut self,
        base: Option<ExprId>,
        fields: &[Field],
        span: Span,
    ) -> MusiResult<TyId> {
        let mut field_tys = Vec::new();
        if let Some(base_expr) = base {
            let base_ty = self.infer_expr(base_expr)?;
            let base_kind = &self.ty_arena.get(base_ty).kind.clone();
            if let TyKind::Record {
                fields: base_fields,
            } = base_kind
            {
                field_tys.extend(base_fields.clone());
            }
        }
        for field in fields {
            let field_ty = match field.init {
                Some(init) => self.infer_expr(init)?,
                None => self.fresh_var(span),
            };
            if let Some(pos) = field_tys.iter().position(|(n, _)| *n == field.name) {
                field_tys[pos] = (field.name, field_ty);
            } else {
                field_tys.push((field.name, field_ty));
            }
        }
        Ok(self.alloc_ty(TyKind::Record { fields: field_tys }, span))
    }

    fn infer_deref(&mut self, inner: ExprId, span: Span) -> MusiResult<TyId> {
        let inner_ty = self.infer_expr(inner)?;
        let inner_kind = &self.ty_arena.get(inner_ty).kind.clone();
        if let TyKind::Ptr(pointee) = inner_kind {
            Ok(*pointee)
        } else {
            let pointee = self.fresh_var(span);
            let ptr_ty = self.alloc_ty(TyKind::Ptr(pointee), span);
            self.unify(inner_ty, ptr_ty, span)?;
            Ok(pointee)
        }
    }

    fn infer_propagate(&mut self, inner: ExprId, span: Span) -> MusiResult<TyId> {
        self.unwrap_optional(inner, span)
    }

    fn infer_force(&mut self, inner: ExprId, span: Span) -> MusiResult<TyId> {
        self.unwrap_optional(inner, span)
    }

    fn unwrap_optional(&mut self, inner: ExprId, span: Span) -> MusiResult<TyId> {
        let inner_ty = self.infer_expr(inner)?;
        let inner_kind = &self.ty_arena.get(inner_ty).kind.clone();
        if let TyKind::Optional(elem) = inner_kind {
            Ok(*elem)
        } else {
            let elem = self.fresh_var(span);
            let opt_ty = self.alloc_ty(TyKind::Optional(elem), span);
            self.unify(inner_ty, opt_ty, span)?;
            Ok(elem)
        }
    }

    fn infer_range(&mut self, start: ExprId, end: Option<ExprId>, span: Span) -> MusiResult<TyId> {
        let start_ty = self.infer_expr(start)?;
        if let Some(end_expr) = end {
            let end_ty = self.infer_expr(end_expr)?;
            self.unify(start_ty, end_ty, span)?;
        }
        Ok(self.alloc_ty(TyKind::Range(start_ty), span))
    }

    fn infer_record_def(&mut self, name: Option<Symbol>, fields: &[Field], span: Span) -> TyId {
        let field_tys: Vec<(Symbol, TyId)> = fields
            .iter()
            .map(|f| (f.name, self.fresh_var(span)))
            .collect();
        let record_ty = self.alloc_ty(TyKind::Record { fields: field_tys }, span);
        if let Some(sym) = name {
            self.env.bind_ty(sym, record_ty);
        }
        self.alloc_ty(TyKind::Unit, span)
    }

    fn infer_choice_def(
        &mut self,
        name: Option<Symbol>,
        _cases: &[ChoiceCase],
        span: Span,
    ) -> TyId {
        if let Some(sym) = name {
            let choice_ty = self.alloc_ty(TyKind::Named(sym), span);
            self.env.bind_ty(sym, choice_ty);
        }
        self.alloc_ty(TyKind::Unit, span)
    }

    fn infer_type_def(&mut self, name: Symbol, span: Span) -> TyId {
        let alias_ty = self.alloc_ty(TyKind::Named(name), span);
        self.env.bind_ty(name, alias_ty);
        self.alloc_ty(TyKind::Unit, span)
    }

    fn alloc_ty(&mut self, kind: TyKind, span: Span) -> TyId {
        self.ty_arena.alloc(kind, span)
    }

    fn fresh_var(&mut self, span: Span) -> TyId {
        let var_key = self.table.new_key();
        self.alloc_ty(TyKind::Var(var_key), span)
    }

    fn unify(&mut self, t1: TyId, t2: TyId, span: Span) -> MusiResult<()> {
        let mut unifier = Unifier::new(self.ty_arena, self.table);
        unifier.unify(t1, t2, span)
    }

    /// # Errors
    /// Returns error if the expression does not match the expected type.
    pub fn check_expr(&mut self, expr_id: ExprId, expected: TyId) -> MusiResult<TyId> {
        let inferred = self.infer_expr(expr_id)?;
        let span = self.ast.exprs.get(expr_id).span;
        self.unify(inferred, expected, span)?;
        Ok(expected)
    }

    fn lower_ty_expr(&mut self, ty_expr_id: TyExprId) -> TyId {
        let ty_expr = self.ast.ty_exprs.get(ty_expr_id);
        let span = ty_expr.span;
        match &ty_expr.kind.clone() {
            TyExprKind::Ident(sym) => self.lower_ty_ident(*sym, span),
            TyExprKind::App { base, args } => self.lower_ty_app(*base, args, span),
            TyExprKind::Optional(inner) => {
                let inner_ty = self.lower_ty_expr(*inner);
                self.alloc_ty(TyKind::Optional(inner_ty), span)
            }
            TyExprKind::Array { elem, .. } => {
                let elem_ty = self.lower_ty_expr(*elem);
                self.alloc_ty(TyKind::Array(elem_ty), span)
            }
            TyExprKind::Ptr(inner) => {
                let inner_ty = self.lower_ty_expr(*inner);
                self.alloc_ty(TyKind::Ptr(inner_ty), span)
            }
            TyExprKind::Fn { param, ret } => {
                let param_ty = self.lower_ty_expr(*param);
                let ret_ty = self.lower_ty_expr(*ret);
                self.alloc_ty(
                    TyKind::Fn {
                        params: vec![param_ty],
                        ret: ret_ty,
                    },
                    span,
                )
            }
            TyExprKind::Tuple(elems) => {
                let elem_tys: Vec<TyId> = elems.iter().map(|e| self.lower_ty_expr(*e)).collect();
                self.alloc_ty(TyKind::Tuple(elem_tys), span)
            }
        }
    }

    fn lower_ty_ident(&mut self, sym: Symbol, span: Span) -> TyId {
        let name = self.interner.resolve(sym.id);
        match name {
            "Int" => self.alloc_ty(TyKind::Int, span),
            "Real" => self.alloc_ty(TyKind::Real, span),
            "String" => self.alloc_ty(TyKind::String, span),
            "Rune" => self.alloc_ty(TyKind::Rune, span),
            "Bool" => self.alloc_ty(TyKind::Bool, span),
            "Unit" => self.alloc_ty(TyKind::Unit, span),
            "Any" => self.alloc_ty(TyKind::Any, span),
            "Never" => self.alloc_ty(TyKind::Never, span),
            _ => self
                .env
                .lookup_ty(sym)
                .unwrap_or_else(|| self.alloc_ty(TyKind::Named(sym), span)),
        }
    }

    fn lower_ty_app(&mut self, _base: Symbol, _args: &[TyExprId], span: Span) -> TyId {
        self.fresh_var(span)
    }
}

#[cfg(test)]
mod tests;
