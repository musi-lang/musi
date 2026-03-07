//! Type inference methods for expressions, statements, and declarations.

use musi_ast::{
    ArrayItem, AstArenas, BinOp, Cond, ElifBranch, Expr, FieldInit, LitValue, MatchArm,
    PostfixOp, PrefixOp,
};
use musi_shared::{Idx, Span};

use crate::types::{PrimTy, Type, TypeVarId};

use super::{instantiate, FnDefNode, TypeChecker};

impl TypeChecker<'_> {
    pub(super) fn infer_expr(&mut self, idx: Idx<Expr>, ctx: &AstArenas) -> Type {
        let expr = ctx.exprs.get(idx);
        match expr {
            Expr::Lit { value, .. } => Self::infer_lit(value),
            Expr::Unit { .. }
            | Expr::Choice { .. }
            | Expr::Import { .. }
            | Expr::Export { .. }
            | Expr::Record { name: None, .. } => Type::Prim(PrimTy::Unit),
            Expr::Record { name: Some(sym), fields, .. } => {
                use std::collections::HashMap;
                use musi_shared::Symbol;
                if let Some(def_id) = self.find_type_def(*sym) {
                    let field_map: HashMap<Symbol, Type> = fields
                        .iter()
                        .filter_map(|f| f.ty.as_ref().map(|ty| (f.name, self.resolve_ty(ty))))
                        .collect();
                    let _ = self.record_fields.insert(def_id, field_map);
                }
                Type::Prim(PrimTy::Unit)
            }
            Expr::Ident { span, .. } => self.infer_ident(idx, *span),
            Expr::Paren { inner, .. } => self.infer(*inner, ctx),
            Expr::Tuple { elements, .. } => self.infer_tuple(ctx.expr_lists.get_slice(*elements), ctx),
            Expr::Block { stmts, tail, .. } => self.infer_block(ctx.expr_lists.get_slice(*stmts), tail.as_ref().copied(), ctx),
            Expr::Array { items, .. } => self.infer_array_expr(items, ctx),
            Expr::AnonRec { fields, .. } => self.infer_anon_rec_expr(fields, ctx),
            Expr::Prefix { op, operand, span } => self.infer_prefix_op(*op, *operand, *span, ctx),
            Expr::Binary { op, lhs, rhs, span } => self.infer_binary(*op, *lhs, *rhs, *span, ctx),
            Expr::Assign { target, value, span } => self.infer_assign(*target, *value, *span, ctx),
            Expr::Postfix { base, op, span } => self.infer_postfix(*base, op, *span, ctx),
            Expr::Bind { pat, ty: ann_ty, init, span, .. } =>
                self.infer_bind_expr(pat, ann_ty.as_ref(), init.as_ref().copied(), *span, ctx),
            Expr::FnDef { name, ty_params, params, ret_ty, body, span, .. } => {
                let node = FnDefNode {
                    name: *name,
                    ty_params,
                    params,
                    ret_ty: ret_ty.as_ref(),
                    body: body.as_ref().copied(),
                    span: *span,
                };
                self.infer_fn_def(node, ctx)
            }
            Expr::Lambda { ty_params, params, ret_ty, body, .. } =>
                self.infer_lambda_expr(ty_params, params, ret_ty.as_ref(), *body, ctx),
            Expr::DotPrefix { args, .. } => {
                for &a in ctx.expr_lists.get_slice(*args) {
                    let _ty = self.infer(a, ctx);
                }
                Type::Var(self.unify_table.fresh())
            }
            other => self.infer_expr_stmt(other, ctx),
        }
    }

    fn infer_expr_stmt(&mut self, expr: &Expr, ctx: &AstArenas) -> Type {
        match expr {
            Expr::If { cond, then_body, elif_chains, else_body, span } =>
                self.infer_if(cond, *then_body, elif_chains, else_body.as_ref().copied(), *span, ctx),
            Expr::Match { scrutinee, arms, .. } =>
                self.infer_match_expr(*scrutinee, arms, ctx),
            Expr::While { cond, guard, body, span } =>
                self.infer_while_expr(cond, guard.as_ref().copied(), *body, *span, ctx),
            Expr::Loop { body, post_cond, .. } =>
                self.infer_loop_expr(*body, post_cond.as_deref(), ctx),
            Expr::For { iter, body, guard, .. } =>
                self.infer_for_expr(*iter, *body, guard.as_ref().copied(), ctx),
            Expr::Label { body, .. } => self.infer(*body, ctx),
            Expr::Return { value, .. } | Expr::Break { value, .. } => {
                if let Some(&v) = value.as_ref() {
                    let _ty = self.infer(v, ctx);
                }
                Type::Var(self.unify_table.fresh())
            }
            Expr::Cycle { guard, .. } => {
                if let Some(&g) = guard.as_ref() {
                    let _ty = self.infer(g, ctx);
                }
                Type::Var(self.unify_table.fresh())
            }
            Expr::Defer { body, .. } => {
                let _ty = self.infer(*body, ctx);
                Type::Prim(PrimTy::Unit)
            }
            Expr::Using { init, body, .. } => {
                let _init_ty = self.infer(*init, ctx);
                self.infer(*body, ctx)
            }
            Expr::ClassDef { .. } | Expr::GivenDef { .. } => Type::Prim(PrimTy::Unit),
            _ => Type::Error,
        }
    }

    const fn infer_lit(value: &LitValue) -> Type {
        match value {
            LitValue::Int(_) => Type::Prim(PrimTy::Int),
            LitValue::Float(_) => Type::Prim(PrimTy::Float),
            LitValue::Str(_) => Type::Prim(PrimTy::String),
            LitValue::Char(_) => Type::Prim(PrimTy::Rune),
        }
    }

    fn infer_ident(&mut self, idx: Idx<Expr>, span: Span) -> Type {
        match self.expr_defs.get(&idx) {
            Some(&def_id) => self.def_type(def_id, span),
            None => Type::Error, // already reported by resolver
        }
    }

    fn infer_tuple(&mut self, elements: &[Idx<Expr>], ctx: &AstArenas) -> Type {
        let elems: Vec<Type> = elements.iter().map(|&e| self.infer(e, ctx)).collect();
        Type::Tuple(elems)
    }

    fn infer_block(
        &mut self,
        stmts: &[Idx<Expr>],
        tail: Option<Idx<Expr>>,
        ctx: &AstArenas,
    ) -> Type {
        for &stmt in stmts {
            let _ty = self.infer(stmt, ctx);
        }
        tail.map_or(Type::Prim(PrimTy::Unit), |t| self.infer(t, ctx))
    }

    fn infer_array_expr(&mut self, items: &[ArrayItem], ctx: &AstArenas) -> Type {
        let elem_ty = self.unify_table.fresh();
        for &item in items {
            let item_idx = match item {
                ArrayItem::Single(i) | ArrayItem::Spread(i) => i,
            };
            let item_ty = self.infer(item_idx, ctx);
            let span = Self::expr_span(item_idx, ctx);
            let unified = self.unify(Type::Var(elem_ty), item_ty, span);
            let _prev = self.expr_types.insert(item_idx, unified);
        }
        Type::Array(Box::new(Type::Var(elem_ty)), None)
    }

    fn infer_field_inits(&mut self, fields: &[FieldInit], ctx: &AstArenas) {
        for field in fields {
            match field {
                FieldInit::Named { value, .. } => {
                    let _ty = self.infer(*value, ctx);
                }
                FieldInit::Spread { expr: e, .. } => {
                    let _ty = self.infer(*e, ctx);
                }
            }
        }
    }

    fn infer_anon_rec_expr(&mut self, fields: &[FieldInit], ctx: &AstArenas) -> Type {
        self.infer_field_inits(fields, ctx);
        Type::Var(self.unify_table.fresh())
    }

    fn infer_prefix_op(
        &mut self,
        op: PrefixOp,
        operand: Idx<Expr>,
        span: Span,
        ctx: &AstArenas,
    ) -> Type {
        let operand_ty = self.infer(operand, ctx);
        match op {
            PrefixOp::Neg => self.unify(Type::Prim(PrimTy::Int), operand_ty, span),
            PrefixOp::Not => self.unify(Type::Prim(PrimTy::Bool), operand_ty, span),
            PrefixOp::BitNot | PrefixOp::Deref | PrefixOp::AddrOf => operand_ty,
        }
    }

    fn infer_assign(
        &mut self,
        target: Idx<Expr>,
        value: Idx<Expr>,
        span: Span,
        ctx: &AstArenas,
    ) -> Type {
        let target_ty = self.infer(target, ctx);
        let value_ty = self.infer(value, ctx);
        let _unified = self.unify(target_ty, value_ty, span);
        Type::Prim(PrimTy::Unit)
    }

    fn infer_bind_expr(
        &mut self,
        pat: &musi_ast::Pat,
        ann_ty: Option<&musi_ast::Ty>,
        init: Option<Idx<Expr>>,
        span: Span,
        ctx: &AstArenas,
    ) -> Type {
        let ann = ann_ty.map(|t| self.resolve_ty(t));

        let init_ty = init.map(|e| self.infer(e, ctx));
        let binding_ty = match (ann, init_ty) {
            (Some(a), Some(i)) => {
                let _u = self.unify(a.clone(), i, span);
                a
            }
            (Some(a), None) => a,
            (None, Some(i)) => i,
            (None, None) => {
                let v = self.unify_table.fresh();
                Type::Var(v)
            }
        };

        self.set_pat_type(pat, binding_ty);

        Type::Prim(PrimTy::Unit)
    }

    fn infer_lambda_expr(
        &mut self,
        ty_params: &[musi_ast::TyParam],
        params: &[musi_ast::Param],
        ret_ty: Option<&musi_ast::Ty>,
        body: Idx<Expr>,
        ctx: &AstArenas,
    ) -> Type {
        let _ty_param_vars = self.push_ty_param_scope(ty_params);
        let param_types = self.collect_param_types(params);

        let ret_var = self.unify_table.fresh();
        let body_ty = self.infer(body, ctx);
        let body_span = Self::expr_span(body, ctx);
        let _u = self.unify(Type::Var(ret_var), body_ty, body_span);
        let ret_type = self.unify_table.resolve(Type::Var(ret_var));
        if let Some(ann) = ret_ty {
            let ann_type = self.resolve_ty(ann);
            let _u = self.unify(ann_type, ret_type.clone(), body_span);
        }

        let _frame = self.ty_scope.pop();

        Type::Arrow(param_types, Box::new(ret_type))
    }

    fn infer_match_expr(
        &mut self,
        scrutinee: Idx<Expr>,
        arms: &[MatchArm],
        ctx: &AstArenas,
    ) -> Type {
        let _scrut_ty = self.infer(scrutinee, ctx);
        let result_var = self.unify_table.fresh();
        for arm in arms {
            let arm_ty = self.infer_match_arm(arm, ctx);
            let _u = self.unify(Type::Var(result_var), arm_ty, Span::default());
        }
        self.unify_table.resolve(Type::Var(result_var))
    }

    fn infer_while_expr(
        &mut self,
        cond: &Cond,
        guard: Option<Idx<Expr>>,
        body: Idx<Expr>,
        span: Span,
        ctx: &AstArenas,
    ) -> Type {
        self.check_cond(cond, span, ctx);
        self.check_guard(guard, ctx);
        let _body_ty = self.infer(body, ctx);
        Type::Prim(PrimTy::Unit)
    }

    fn infer_loop_expr(
        &mut self,
        body: Idx<Expr>,
        post_cond: Option<&Cond>,
        ctx: &AstArenas,
    ) -> Type {
        let _body_ty = self.infer(body, ctx);
        if let Some(pc) = post_cond {
            let fake_span = Span::default();
            self.check_cond(pc, fake_span, ctx);
        }
        Type::Prim(PrimTy::Unit)
    }

    fn infer_for_expr(
        &mut self,
        iter: Idx<Expr>,
        body: Idx<Expr>,
        guard: Option<Idx<Expr>>,
        ctx: &AstArenas,
    ) -> Type {
        let _iter_ty = self.infer(iter, ctx);
        self.check_guard(guard, ctx);
        let _body_ty = self.infer(body, ctx);
        Type::Prim(PrimTy::Unit)
    }

    fn infer_fn_def(&mut self, node: FnDefNode<'_>, ctx: &AstArenas) -> Type {
        let ty_param_vars = self.push_ty_param_scope(node.ty_params);
        let param_types = self.collect_param_types(node.params);

        let ret_var = self.unify_table.fresh();
        let expected_ret = node.ret_ty.map(|t| self.resolve_ty(t));

        let preliminary_ty = Type::Arrow(param_types.clone(), Box::new(Type::Var(ret_var)));
        let def_id = self.find_def_by_name(node.name);
        if let Some(id) = def_id {
            self.set_def_type(id, preliminary_ty);
        }

        if let Some(body_idx) = node.body {
            let body_ty = self.infer(body_idx, ctx);
            let body_span = Self::expr_span(body_idx, ctx);
            let _u = self.unify(Type::Var(ret_var), body_ty, body_span);
        }

        if let Some(ann_ret) = expected_ret {
            let actual_ret = self.unify_table.resolve(Type::Var(ret_var));
            let _u = self.unify(ann_ret, actual_ret, node.span);
        }

        let _frame = self.ty_scope.pop();

        let scheme_vars: Vec<TypeVarId> = ty_param_vars
            .into_iter()
            .filter(|&v| self.unify_table.is_free(v))
            .collect();

        let final_ret = self.unify_table.resolve(Type::Var(ret_var));
        let final_ty = Type::Arrow(param_types, Box::new(final_ret));
        if let Some(id) = def_id {
            let idx = usize::try_from(id.0).expect("DefId in range");
            let info = self.defs.get_mut(idx).expect("DefId is valid");
            info.ty = Some(final_ty);
            info.scheme_vars = scheme_vars;
        }

        Type::Prim(PrimTy::Unit)
    }

    fn infer_binary(
        &mut self,
        op: BinOp,
        lhs: Idx<Expr>,
        rhs: Idx<Expr>,
        span: Span,
        ctx: &AstArenas,
    ) -> Type {
        let lhs_ty = self.infer(lhs, ctx);
        let rhs_ty = self.infer(rhs, ctx);

        match op {
            // Arithmetic + bitwise: both sides must agree, result same type.
            BinOp::Add
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Rem
            | BinOp::BitOr
            | BinOp::BitXor
            | BinOp::BitAnd
            | BinOp::Shl
            | BinOp::Shr => self.unify(lhs_ty, rhs_ty, span),

            // Logical: both sides must be Bool, result is Bool.
            BinOp::And | BinOp::Or | BinOp::Xor => {
                let _u = self.unify(Type::Prim(PrimTy::Bool), lhs_ty, span);
                let _v = self.unify(Type::Prim(PrimTy::Bool), rhs_ty, span);
                Type::Prim(PrimTy::Bool)
            }

            // Comparison: both sides must agree, result is Bool.
            BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                let _u = self.unify(lhs_ty, rhs_ty, span);
                Type::Prim(PrimTy::Bool)
            }

            BinOp::In => {
                // Element-in-collection; result is Bool.
                Type::Prim(PrimTy::Bool)
            }

            // Range: both sides Int, result is an array/range.
            BinOp::Range | BinOp::RangeExcl => {
                let _u = self.unify(lhs_ty, rhs_ty, span);
                // For now, return a fresh var (range type not yet modelled).
                Type::Var(self.unify_table.fresh())
            }

            BinOp::Cons => {
                // Cons: result type is an array of the lhs element type.
                Type::Array(Box::new(lhs_ty), None)
            }

            BinOp::NilCoalesce => {
                // lhs ?? rhs: lhs is Option[T], rhs is T, result is T.
                Type::Var(self.unify_table.fresh())
            }
        }
    }

    fn infer_postfix(
        &mut self,
        base: Idx<Expr>,
        op: &PostfixOp,
        span: Span,
        ctx: &AstArenas,
    ) -> Type {
        match op {
            PostfixOp::Call { args, .. } => {
                let callee_ty = self.infer(base, ctx);
                self.infer_call(callee_ty, base, ctx.expr_lists.get_slice(*args), span, ctx)
            }

            PostfixOp::Index {
                args,
                span: idx_span,
            } => {
                let _base_ty = self.infer(base, ctx);
                for &a in ctx.expr_lists.get_slice(*args) {
                    let _ty = self.infer(a, ctx);
                }
                let _ = idx_span;
                Type::Var(self.unify_table.fresh())
            }

            PostfixOp::Field { name, .. } => {
                let base_ty = self.infer(base, ctx);
                let resolved = self.unify_table.resolve(base_ty);
                if let Type::Named(def_id, _) = &resolved
                    && let Some(fields) = self.record_fields.get(def_id)
                        && let Some(field_ty) = fields.get(name) {
                            return field_ty.clone();
                        }
                Type::Var(self.unify_table.fresh())
            }

            PostfixOp::RecDot { fields, .. } => {
                let base_ty = self.infer(base, ctx);
                self.infer_field_inits(fields, ctx);
                base_ty
            }

            PostfixOp::OptField { .. } => {
                let _base_ty = self.infer(base, ctx);
                Type::Var(self.unify_table.fresh())
            }

            PostfixOp::As { ty, .. } => {
                let _base_ty = self.infer(base, ctx);
                self.resolve_ty(ty)
            }
        }
    }

    fn infer_args(&mut self, args: &[Idx<Expr>], ctx: &AstArenas) {
        for &a in args {
            let _ty = self.infer(a, ctx);
        }
    }

    fn infer_call(
        &mut self,
        callee_ty: Type,
        base: Idx<Expr>,
        args: &[Idx<Expr>],
        span: Span,
        ctx: &AstArenas,
    ) -> Type {
        let resolved_callee = self.unify_table.resolve(callee_ty);
        match resolved_callee {
            Type::Arrow(param_tys, ret_ty) => {
                let (param_tys_inst, ret_ty_inst) =
                    if let Some(def_id) = self.expr_defs.get(&base).copied() {
                        let idx = usize::try_from(def_id.0).expect("DefId in range");
                        let scheme_vars = self
                            .defs
                            .get(idx)
                            .map(|d| d.scheme_vars.clone())
                            .unwrap_or_default();
                        if scheme_vars.is_empty() {
                            (param_tys, *ret_ty)
                        } else {
                            let fresh: Vec<TypeVarId> = scheme_vars
                                .iter()
                                .map(|_| self.unify_table.fresh())
                                .collect();
                            let inst_params = param_tys
                                .iter()
                                .map(|p| instantiate(p, &scheme_vars, &fresh))
                                .collect();
                            let inst_ret = instantiate(&ret_ty, &scheme_vars, &fresh);
                            (inst_params, inst_ret)
                        }
                    } else {
                        (param_tys, *ret_ty)
                    };

                if args.len() != param_tys_inst.len() {
                    let _d = self.diags.error(
                        format!(
                            "function expects {} argument(s), found {}",
                            param_tys_inst.len(),
                            args.len()
                        ),
                        span,
                        self.file_id,
                    );
                    self.infer_args(args, ctx);
                    return Type::Error;
                }

                let mut all_ok = true;
                for (&arg_idx, param_ty) in args.iter().zip(param_tys_inst.iter()) {
                    let arg_ty = self.infer(arg_idx, ctx);
                    let arg_span = Self::expr_span(arg_idx, ctx);
                    let u = self.unify(param_ty.clone(), arg_ty, arg_span);
                    if u == Type::Error {
                        all_ok = false;
                    }
                }

                if all_ok {
                    self.unify_table.resolve(ret_ty_inst)
                } else {
                    Type::Error
                }
            }

            Type::Var(v) => {
                let arg_types: Vec<Type> = args.iter().map(|&a| self.infer(a, ctx)).collect();
                let ret_var = self.unify_table.fresh();
                let arrow = Type::Arrow(arg_types, Box::new(Type::Var(ret_var)));
                let _u = self.unify(Type::Var(v), arrow, span);
                self.unify_table.resolve(Type::Var(ret_var))
            }

            Type::Error => {
                self.infer_args(args, ctx);
                Type::Error
            }

            other => {
                let _d = self.diags.error(
                    format!("cannot call a value of type `{other}`"),
                    span,
                    self.file_id,
                );
                self.infer_args(args, ctx);
                Type::Error
            }
        }
    }

    fn infer_if(
        &mut self,
        cond: &Cond,
        then_body: Idx<Expr>,
        elif_chains: &[ElifBranch],
        else_body: Option<Idx<Expr>>,
        span: Span,
        ctx: &AstArenas,
    ) -> Type {
        self.check_cond(cond, span, ctx);
        let then_ty = self.infer(then_body, ctx);
        let result_var = self.unify_table.fresh();
        let then_span = Self::expr_span(then_body, ctx);
        let _u = self.unify_branch_type(result_var, then_ty, then_span);

        for chain in elif_chains {
            self.check_cond(&chain.cond, span, ctx);
            let elif_ty = self.infer(chain.body, ctx);
            let elif_span = Self::expr_span(chain.body, ctx);
            let _u = self.unify_branch_type(result_var, elif_ty, elif_span);
        }

        match else_body {
            Some(eb) => {
                let else_ty = self.infer(eb, ctx);
                let else_span = Self::expr_span(eb, ctx);
                self.unify_branch_type(result_var, else_ty, else_span)
            }
            None => self.unify_branch_type(result_var, Type::Prim(PrimTy::Unit), span),
        }
    }

    fn unify_branch_type(&mut self, result_var: TypeVarId, branch_ty: Type, span: Span) -> Type {
        self.unify(Type::Var(result_var), branch_ty, span)
    }

    fn check_guard(&mut self, guard: Option<Idx<Expr>>, ctx: &AstArenas) {
        if let Some(g) = guard {
            let gty = self.infer(g, ctx);
            let gspan = Self::expr_span(g, ctx);
            let _u = self.unify(Type::Prim(PrimTy::Bool), gty, gspan);
        }
    }

    fn check_cond(&mut self, cond: &Cond, span: Span, ctx: &AstArenas) {
        match cond {
            Cond::Expr(e) => {
                let ty = self.infer(*e, ctx);
                let _u = self.unify(Type::Prim(PrimTy::Bool), ty, span);
            }
            Cond::Case { init, .. } => {
                let _ty = self.infer(*init, ctx);
            }
        }
    }

    fn infer_match_arm(&mut self, arm: &MatchArm, ctx: &AstArenas) -> Type {
        self.infer(arm.body, ctx)
    }
}
