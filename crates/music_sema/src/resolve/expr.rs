//! Pass 2: expression resolution.

use music_ast::expr::{Arg, ArrayElem, Expr, LetFields, MatchArm, Param, PwGuard, RecField};
use music_ast::ty::{Constraint, TyParam};
use music_ast::{ExprIdx, TyIdx};
use music_shared::{Span, Symbol};

use crate::def::DefKind;
use crate::error::SemaError;

use super::{Resolver, binding_def_kind};

impl Resolver<'_> {
    /// Pass 2: resolve all name references in an expression.
    pub(super) fn resolve_expr(&mut self, expr_idx: ExprIdx) {
        match self.ast.exprs[expr_idx].clone() {
            Expr::Name { name, span } => self.resolve_name(expr_idx, name, span),
            Expr::Lit { .. } | Expr::Error { .. } | Expr::Import { .. } | Expr::Export { .. } => {}
            Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => self.resolve_expr(inner),
            Expr::Choice { body, .. } => self.resolve_expr_choice(body),
            Expr::Tuple { elems, .. } | Expr::Variant { args: elems, .. } => {
                for &e in &elems {
                    self.resolve_expr(e);
                }
            }
            Expr::Block { stmts, tail, .. } => self.resolve_expr_block(&stmts, tail),
            Expr::BinOp { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::UnaryOp { operand, .. } => self.resolve_expr(operand),
            Expr::Field { object, .. } => self.resolve_expr(object),
            Expr::Index { object, index, .. } => {
                self.resolve_expr(object);
                self.resolve_expr(index);
            }
            Expr::Call { callee, args, .. } => self.resolve_expr_call(callee, &args),
            Expr::Update { base, fields, .. } => {
                self.resolve_expr(base);
                self.resolve_rec_fields(&fields);
            }
            Expr::Record { fields, .. } => self.resolve_rec_fields(&fields),
            Expr::Array { elems, .. } => {
                for elem in &elems {
                    match elem {
                        ArrayElem::Elem { expr, .. } | ArrayElem::Spread { expr, .. } => {
                            self.resolve_expr(*expr);
                        }
                    }
                }
            }
            Expr::Piecewise { arms, .. } => {
                for arm in &arms {
                    if let PwGuard::When { expr, .. } = arm.guard {
                        self.resolve_expr(expr);
                    }
                    self.resolve_expr(arm.result);
                }
            }
            Expr::Return { value, .. } => {
                if let Some(v) = value {
                    self.resolve_expr(v);
                }
            }
            Expr::Let { fields, body, .. } => self.resolve_expr_let(&fields, body),
            Expr::Binding { fields, .. } => self.resolve_expr_binding(&fields),
            Expr::Fn {
                params,
                ret_ty,
                body,
                ..
            } => self.resolve_expr_fn(&params, ret_ty, body),
            Expr::Match {
                scrutinee, arms, ..
            } => self.resolve_expr_match(scrutinee, &arms),
            Expr::Quantified {
                body,
                params,
                constraints,
                ..
            } => {
                self.resolve_expr_quantified(body, &params, &constraints);
            }
            Expr::Class {
                name,
                params,
                constraints,
                members,
                ..
            } => {
                self.resolve_expr_class(name, &params, &constraints, &members);
            }
            Expr::Given {
                target,
                params,
                constraints,
                members,
                ..
            } => {
                self.resolve_expr_given(&target, &params, &constraints, &members);
            }
            Expr::Effect {
                name,
                params,
                ops,
                exported,
                ..
            } => self.resolve_expr_effect(name, &params, &ops, exported),
            Expr::Foreign { decls, .. } => self.resolve_expr_foreign(&decls),
        }
    }

    pub(super) fn resolve_name(&mut self, expr_idx: ExprIdx, name: Symbol, span: Span) {
        if let Some(def_id) = self.scopes.lookup(self.current_scope, name) {
            let _prev = self.output.expr_defs.insert(expr_idx, def_id);
            self.defs.get_mut(def_id).use_count += 1;
        } else {
            self.report_undefined(name, span);
        }
    }

    pub(super) fn report_undefined(&mut self, name: Symbol, span: Span) {
        let name_str = self.interner.resolve(name);
        let _d = self.diags.report(
            &SemaError::UndefinedName {
                name: Box::from(name_str),
            },
            span,
            self.file_id,
        );
    }

    fn resolve_rec_fields(&mut self, fields: &[RecField]) {
        for field in fields {
            match field {
                RecField::Named { value, .. } => {
                    if let Some(v) = value {
                        self.resolve_expr(*v);
                    }
                }
                RecField::Spread { expr, .. } => {
                    self.resolve_expr(*expr);
                }
            }
        }
    }

    fn resolve_expr_block(&mut self, stmts: &[ExprIdx], tail: Option<ExprIdx>) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        for &stmt in stmts {
            let fields = match &self.ast.exprs[stmt] {
                Expr::Binding { fields, .. } | Expr::Let { fields, .. } => Some(fields.clone()),
                _ => None,
            };
            if let Some(fields) = fields {
                self.resolve_block_binding(&fields);
            } else {
                self.resolve_expr(stmt);
            }
        }
        if let Some(t) = tail {
            self.resolve_expr(t);
        }
        self.current_scope = parent;
    }

    /// Resolves a block-local binding: pre-defines function names for recursion,
    /// resolves the value/type, then defines non-function patterns in scope.
    fn resolve_block_binding(&mut self, fields: &LetFields) {
        use music_ast::pat::Pat;
        let is_fn_pat = matches!(&self.ast.pats[fields.pat], Pat::Variant { .. });

        // For function-like patterns, pre-define the name to enable recursion.
        if is_fn_pat {
            self.define_fn_name(fields.pat, binding_def_kind(fields.kind));
        }

        let parent_ty_scope = if fields.params.is_empty() {
            None
        } else {
            Some(self.enter_ty_param_scope(&fields.params, &fields.constraints))
        };
        let fn_pat_parent = self.enter_fn_pat_scope(fields.pat);
        if let Some(v) = fields.value {
            self.resolve_expr(v);
        }
        if let Some(ty) = fields.ty {
            self.resolve_ty(ty);
        }
        if let Some(p) = fn_pat_parent {
            self.current_scope = p;
        }
        if let Some(p) = parent_ty_scope {
            self.current_scope = p;
        }
        if !is_fn_pat {
            self.define_pat(fields.pat, binding_def_kind(fields.kind));
        }
    }

    fn resolve_expr_call(&mut self, callee: ExprIdx, args: &[Arg]) {
        self.resolve_expr(callee);
        for arg in args {
            if let Arg::Pos { expr, .. } = arg {
                self.resolve_expr(*expr);
            }
        }
    }

    fn resolve_expr_let(&mut self, fields: &LetFields, body: Option<ExprIdx>) {
        use music_ast::pat::Pat;
        let is_fn_pat = matches!(&self.ast.pats[fields.pat], Pat::Variant { .. });

        let parent_ty_scope = if fields.params.is_empty() {
            None
        } else {
            Some(self.enter_ty_param_scope(&fields.params, &fields.constraints))
        };

        // For function-like patterns, pre-define the name for recursion.
        if is_fn_pat {
            self.define_fn_name(fields.pat, binding_def_kind(fields.kind));
        }

        let fn_pat_parent = self.enter_fn_pat_scope(fields.pat);

        if let Some(v) = fields.value {
            self.resolve_expr(v);
        }
        if let Some(ty) = fields.ty {
            self.resolve_ty(ty);
        }

        if let Some(p) = fn_pat_parent {
            self.current_scope = p;
        }

        if let Some(body) = body {
            let parent = self.current_scope;
            self.current_scope = self.scopes.push_child(parent);
            if !is_fn_pat {
                self.define_pat(fields.pat, binding_def_kind(fields.kind));
            }
            self.resolve_expr(body);
            self.current_scope = parent;
        } else if !is_fn_pat {
            self.define_pat(fields.pat, binding_def_kind(fields.kind));
        }

        if let Some(p) = parent_ty_scope {
            self.current_scope = p;
        }
    }

    fn resolve_expr_binding(&mut self, fields: &LetFields) {
        let parent_ty_scope = if fields.params.is_empty() {
            None
        } else {
            Some(self.enter_ty_param_scope(&fields.params, &fields.constraints))
        };

        let fn_pat_parent = self.enter_fn_pat_scope(fields.pat);

        if let Some(v) = fields.value {
            self.resolve_expr(v);
        }
        if let Some(ty) = fields.ty {
            self.resolve_ty(ty);
        }

        if let Some(p) = fn_pat_parent {
            self.current_scope = p;
        }

        if let Some(p) = parent_ty_scope {
            self.current_scope = p;
        }
    }

    fn resolve_expr_fn(&mut self, params: &[Param], ret_ty: Option<TyIdx>, body: ExprIdx) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        for param in params {
            let id = self.defs.alloc(param.name, DefKind::Param, param.span);
            self.defs.get_mut(id).param_mode = Some(param.mode);
            self.define_in_scope(param.name, id, param.span);
            let _inserted = self.output.pat_defs.insert(param.span, id);
            if let Some(ty) = param.ty {
                self.resolve_ty(ty);
            }
        }
        if let Some(ret) = ret_ty {
            self.resolve_ty(ret);
        }
        self.resolve_expr(body);
        self.current_scope = parent;
    }

    fn resolve_expr_match(&mut self, scrutinee: ExprIdx, arms: &[MatchArm]) {
        self.resolve_expr(scrutinee);
        for arm in arms {
            let parent = self.current_scope;
            self.current_scope = self.scopes.push_child(parent);
            self.resolve_pat(arm.pat);
            if let Some(guard) = arm.guard {
                self.resolve_expr(guard);
            }
            self.resolve_expr(arm.result);
            self.current_scope = parent;
        }
    }

    fn resolve_expr_quantified(
        &mut self,
        body: ExprIdx,
        params: &[TyParam],
        constraints: &[Constraint],
    ) {
        let parent = self.enter_ty_param_scope(params, constraints);
        self.resolve_expr(body);
        self.current_scope = parent;
    }

    fn resolve_expr_choice(&mut self, body: TyIdx) {
        use music_ast::ty::Ty;
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);

        if let Ty::Sum { variants, .. } = &self.ast.tys[body] {
            for &variant_ty in variants {
                if let Ty::Named { name, .. } = &self.ast.tys[variant_ty] {
                    let id = self.defs.alloc(*name, DefKind::Variant, Span::DUMMY);
                    self.define_in_scope(*name, id, Span::DUMMY);
                }
            }
        }

        self.resolve_ty(body);
        self.current_scope = parent;
    }
}
