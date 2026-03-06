//! Bidirectional type checker with Hindley–Milner–style inference.
//!
//! Walks the AST in expression order, infers types bottom-up and checks them
//! top-down.  Unification is handled by a standard union-find table.
//!
//! The output is a side table mapping each `Idx<Expr>` to its [`Type`].  The
//! [`DefInfo`] entries for all resolved bindings are updated in place with the
//! inferred types.

use std::collections::HashMap;

use musi_ast::{
    ArrayItem, AstArenas, BinOp, Cond, ElifBranch, Expr, FieldInit, LitValue, MatchArm, Param,
    Pat, ParsedModule, PostfixOp, PrefixOp, Ty, TyParam,
};
use musi_shared::{DiagnosticBag, FileId, Idx, Interner, Span, Symbol};

use crate::def::{DefId, DefInfo, DefKind};
use crate::types::{PrimTy, Type, TypeVarId};

/// A union-find table for type unification variables.
pub struct UnifyTable {
    vars: Vec<Option<Type>>,
}

impl UnifyTable {
    #[must_use]
    pub const fn new() -> Self {
        Self { vars: Vec::new() }
    }

    /// Allocates a fresh, unbound unification variable.
    ///
    /// # Panics
    ///
    /// Panics if the number of type variables overflows `u32`.
    #[must_use]
    pub fn fresh(&mut self) -> TypeVarId {
        let id = TypeVarId(u32::try_from(self.vars.len()).expect("type var overflow"));
        self.vars.push(None);
        id
    }

    /// Returns `true` if `v` is not yet bound to any type.
    ///
    /// # Panics
    ///
    /// Panics if `v` is not a valid `TypeVarId` from this table.
    #[must_use]
    pub fn is_free(&self, v: TypeVarId) -> bool {
        let idx = usize::try_from(v.0).expect("TypeVarId in range");
        self.vars[idx].is_none()
    }

    /// Follows the chain of variable bindings until reaching a concrete type
    /// or a free variable.
    ///
    /// # Panics
    ///
    /// Panics if a `TypeVarId` is not valid for this table.
    #[must_use]
    pub fn resolve(&self, ty: Type) -> Type {
        match ty {
            Type::Var(v) => {
                let idx = usize::try_from(v.0).expect("TypeVarId in range");
                self.vars[idx].as_ref().map_or(Type::Var(v), |bound| self.resolve(bound.clone()))
            }
            other => other,
        }
    }

    fn bind(&mut self, v: TypeVarId, ty: Type) {
        let idx = usize::try_from(v.0).expect("TypeVarId in range");
        self.vars[idx] = Some(ty);
    }

    /// Returns `true` if `v` appears anywhere in `ty` (occurs check).
    fn occurs(&self, v: TypeVarId, ty: &Type) -> bool {
        match ty {
            Type::Var(w) => {
                if v == *w {
                    return true;
                }
                let idx = usize::try_from(w.0).expect("TypeVarId in range");
                self.vars[idx].as_ref().is_some_and(|bound| self.occurs(v, bound))
            }
            Type::Prim(_) | Type::Error => false,
            Type::Tuple(elems) => elems.iter().any(|t| self.occurs(v, t)),
            Type::Array(elem, _) => self.occurs(v, elem),
            Type::Arrow(params, ret) => {
                params.iter().any(|p| self.occurs(v, p)) || self.occurs(v, ret)
            }
            Type::Named(_, args) => args.iter().any(|a| self.occurs(v, a)),
        }
    }

    /// Unifies `a` and `b`, emitting a diagnostic on mismatch.
    ///
    /// Returns the unified type (or [`Type::Error`] on failure).
    pub fn unify(
        &mut self,
        a: Type,
        b: Type,
        span: Span,
        diags: &mut DiagnosticBag,
        file_id: FileId,
    ) -> Type {
        let a = self.resolve(a);
        let b = self.resolve(b);

        match (a, b) {
            // Both sides already resolved to the same type.
            (ref ra, ref rb) if ra == rb => ra.clone(),

            // Error propagates without emitting cascading diagnostics.
            (Type::Error, _) | (_, Type::Error) => Type::Error,

            // Bind a free variable (with occurs check).
            (Type::Var(v), t) | (t, Type::Var(v)) => {
                if self.occurs(v, &t) {
                    let _d = diags.error(
                        String::from("infinite type (occurs check failed)"),
                        span,
                        file_id,
                    );
                    Type::Error
                } else {
                    self.bind(v, t.clone());
                    t
                }
            }

            // Structural cases.
            (Type::Prim(pa), Type::Prim(pb)) => {
                let _d = diags.error(
                    format!("type mismatch: expected `{pa}`, found `{pb}`"),
                    span,
                    file_id,
                );
                Type::Error
            }

            (Type::Arrow(p1, r1), Type::Arrow(p2, r2)) => {
                if p1.len() != p2.len() {
                    let _d = diags.error(
                        format!(
                            "function arity mismatch: expected {} parameter(s), found {}",
                            p1.len(),
                            p2.len()
                        ),
                        span,
                        file_id,
                    );
                    return Type::Error;
                }
                let params: Vec<Type> = p1
                    .into_iter()
                    .zip(p2)
                    .map(|(pa, pb)| self.unify(pa, pb, span, diags, file_id))
                    .collect();
                let ret = Box::new(self.unify(*r1, *r2, span, diags, file_id));
                Type::Arrow(params, ret)
            }

            (Type::Named(d1, a1), Type::Named(d2, a2)) => {
                if d1 != d2 {
                    let _d = diags.error(
                        String::from("type mismatch: incompatible named types"),
                        span,
                        file_id,
                    );
                    return Type::Error;
                }
                if a1.len() != a2.len() {
                    let _d =
                        diags.error(String::from("type argument count mismatch"), span, file_id);
                    return Type::Error;
                }
                let args: Vec<Type> = a1
                    .into_iter()
                    .zip(a2)
                    .map(|(ta, tb)| self.unify(ta, tb, span, diags, file_id))
                    .collect();
                Type::Named(d1, args)
            }

            (Type::Tuple(a), Type::Tuple(b)) => {
                if a.len() != b.len() {
                    let _d = diags.error(
                        format!(
                            "tuple length mismatch: expected {}, found {}",
                            a.len(),
                            b.len()
                        ),
                        span,
                        file_id,
                    );
                    return Type::Error;
                }
                let elems: Vec<Type> = a
                    .into_iter()
                    .zip(b)
                    .map(|(ta, tb)| self.unify(ta, tb, span, diags, file_id))
                    .collect();
                Type::Tuple(elems)
            }

            (a, b) => {
                let _d = diags.error(
                    format!("type mismatch: expected `{a}`, found `{b}`"),
                    span,
                    file_id,
                );
                Type::Error
            }
        }
    }
}

impl Default for UnifyTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Substitutes `scheme_vars[i]` → `fresh_vars[i]` throughout `ty`.
fn instantiate(ty: &Type, scheme_vars: &[TypeVarId], fresh_vars: &[TypeVarId]) -> Type {
    match ty {
        Type::Var(v) => scheme_vars.iter().position(|sv| sv == v)
            .map_or(Type::Var(*v), |pos| Type::Var(fresh_vars[pos])),
        Type::Prim(p) => Type::Prim(*p),
        Type::Error => Type::Error,
        Type::Tuple(elems) => Type::Tuple(
            elems
                .iter()
                .map(|t| instantiate(t, scheme_vars, fresh_vars))
                .collect(),
        ),
        Type::Array(elem, size) => {
            Type::Array(Box::new(instantiate(elem, scheme_vars, fresh_vars)), *size)
        }
        Type::Arrow(params, ret) => Type::Arrow(
            params
                .iter()
                .map(|p| instantiate(p, scheme_vars, fresh_vars))
                .collect(),
            Box::new(instantiate(ret, scheme_vars, fresh_vars)),
        ),
        Type::Named(d, args) => Type::Named(
            *d,
            args.iter()
                .map(|a| instantiate(a, scheme_vars, fresh_vars))
                .collect(),
        ),
    }
}

/// A stack of type-parameter scopes.  `'T` in a generic function maps to a
/// [`TypeVarId`] that was allocated when the function was entered.
type TyScope = Vec<HashMap<Symbol, TypeVarId>>;

fn ty_scope_lookup(stack: &TyScope, name: Symbol) -> Option<TypeVarId> {
    stack
        .iter()
        .rev()
        .find_map(|frame| frame.get(&name).copied())
}

#[derive(Clone, Copy)]
struct FnDefNode<'a> {
    name: musi_shared::Symbol,
    ty_params: &'a [musi_ast::TyParam],
    params: &'a [musi_ast::Param],
    ret_ty: Option<&'a musi_ast::Ty>,
    body: Option<musi_shared::Idx<musi_ast::Expr>>,
    span: musi_shared::Span,
}

pub struct TypeChecker<'a> {
    interner: &'a Interner,
    file_id: FileId,
    diags: &'a mut DiagnosticBag,
    /// Unification table (shared across the entire analysis).
    pub unify_table: UnifyTable,
    /// Expression node → `DefId` (from the resolver).
    expr_defs: &'a HashMap<Idx<Expr>, DefId>,
    /// Pattern binding site span → `DefId` (from the resolver).
    pat_defs: &'a HashMap<Span, DefId>,
    /// All definitions (owned; types are written back here).
    pub defs: Vec<DefInfo>,
    /// The output side table: `Idx<Expr>` → inferred type.
    pub expr_types: HashMap<Idx<Expr>, Type>,
    /// Stack of type-parameter scopes.
    ty_scope: TyScope,
}

impl<'a> TypeChecker<'a> {
    #[must_use]
    pub fn new(
        interner: &'a Interner,
        file_id: FileId,
        diags: &'a mut DiagnosticBag,
        expr_defs: &'a HashMap<Idx<Expr>, DefId>,
        pat_defs: &'a HashMap<Span, DefId>,
        defs: Vec<DefInfo>,
    ) -> Self {
        Self {
            interner,
            file_id,
            diags,
            unify_table: UnifyTable::new(),
            expr_defs,
            pat_defs,
            defs,
            expr_types: HashMap::new(),
            ty_scope: Vec::new(),
        }
    }

    /// Type-checks an entire module, walking each top-level item in order.
    pub fn check_module(&mut self, module: &ParsedModule) {
        for &item_idx in &module.items {
            let ty = self.infer(item_idx, &module.ctx);
            let _prev = self.expr_types.insert(item_idx, ty);
        }
    }

    fn resolve_ty(&self, ty: &Ty) -> Type {
        match ty {
            Ty::Named { name, args, .. } => {
                let name_str = self.interner.resolve(*name);
                // Try built-in primitive types first.
                if let Some(prim) = PrimTy::from_name(name_str) {
                    if args.is_empty() {
                        return Type::Prim(prim);
                    }
                    // Primitive with type args -- not valid, but degrade gracefully.
                    return Type::Prim(prim);
                }
                // Look up user-defined type by name.
                let def_id = self.find_type_def(*name);
                def_id.map_or(Type::Error, |id| {
                    let resolved: Vec<Type> = args.iter().map(|a| self.resolve_ty(a)).collect();
                    Type::Named(id, resolved)
                })
            }
            Ty::Var { name, .. } => {
                ty_scope_lookup(&self.ty_scope, *name).map_or(Type::Error, Type::Var)
            }
            Ty::Arrow { params, ret, .. } => {
                let resolved_params: Vec<Type> =
                    params.iter().map(|p| self.resolve_ty(p)).collect();
                let resolved_ret = Box::new(self.resolve_ty(ret));
                Type::Arrow(resolved_params, resolved_ret)
            }
            Ty::Prod { elements, .. } => {
                let elems: Vec<Type> = elements.iter().map(|e| self.resolve_ty(e)).collect();
                Type::Tuple(elems)
            }
            Ty::Arr { element, .. } => Type::Array(Box::new(self.resolve_ty(element)), None),
            Ty::Error { .. } => Type::Error,
        }
    }

    /// Finds the [`DefId`] for a user-defined type by its interned name.
    fn find_type_def(&self, name: Symbol) -> Option<DefId> {
        self.defs
            .iter()
            .find(|d| d.name == name && d.kind == DefKind::Type)
            .map(|d| d.id)
    }

    /// Infers and records the type of the expression at `idx`.
    pub fn infer(&mut self, idx: Idx<Expr>, ctx: &AstArenas) -> Type {
        let ty = self.infer_expr(idx, ctx);
        let resolved = self.unify_table.resolve(ty);
        let _prev = self.expr_types.insert(idx, resolved.clone());
        resolved
    }

    fn infer_expr(&mut self, idx: Idx<Expr>, ctx: &AstArenas) -> Type {
        let expr = ctx.exprs.get(idx);
        match expr {
            Expr::Lit { value, .. } => Self::infer_lit(value),
            Expr::Unit { .. }
            | Expr::Record { .. }
            | Expr::Choice { .. }
            | Expr::Import { .. }
            | Expr::Export { .. } => Type::Prim(PrimTy::Unit),
            Expr::Ident { span, .. } => self.infer_ident(idx, *span),
            Expr::Paren { inner, .. } => self.infer(*inner, ctx),
            Expr::Tuple { elements, .. } => self.infer_tuple(elements, ctx),
            Expr::Block { stmts, tail, .. } => self.infer_block(stmts, tail.as_ref().copied(), ctx),
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
            let unified =
                self.unify_table
                    .unify(Type::Var(elem_ty), item_ty, span, self.diags, self.file_id);
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
        // We don't track anonymous record types in the type table yet.
        self.infer_field_inits(fields, ctx);
        Type::Error // unresolved named rec type -- Phase 8
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
            PrefixOp::Neg => {
                let expected = Type::Prim(PrimTy::Int);
                self.unify_table
                    .unify(expected, operand_ty, span, self.diags, self.file_id)
            }
            PrefixOp::Not => {
                let expected = Type::Prim(PrimTy::Bool);
                self.unify_table
                    .unify(expected, operand_ty, span, self.diags, self.file_id)
            }
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
        let _unified = self
            .unify_table
            .unify(target_ty, value_ty, span, self.diags, self.file_id);
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
                let _u = self
                    .unify_table
                    .unify(a.clone(), i, span, self.diags, self.file_id);
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

        let mut param_types: Vec<Type> = Vec::new();
        for p in params {
            let pty = match p.ty.as_ref() {
                Some(t) => self.resolve_ty(t),
                None => Type::Var(self.unify_table.fresh()),
            };
            param_types.push(pty);
        }
        self.set_param_types(params, &param_types);

        let ret_var = self.unify_table.fresh();
        let body_ty = self.infer(body, ctx);
        let body_span = Self::expr_span(body, ctx);
        let _u = self.unify_table.unify(
            Type::Var(ret_var),
            body_ty,
            body_span,
            self.diags,
            self.file_id,
        );
        let ret_type = self.unify_table.resolve(Type::Var(ret_var));
        if let Some(ann) = ret_ty {
            let ann_type = self.resolve_ty(ann);
            let _u = self.unify_table.unify(
                ann_type,
                ret_type.clone(),
                body_span,
                self.diags,
                self.file_id,
            );
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
            let _u = self.unify_table.unify(
                Type::Var(result_var),
                arm_ty,
                Span::default(),
                self.diags,
                self.file_id,
            );
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

        let mut param_types: Vec<Type> = Vec::new();
        for p in node.params {
            let pty = match p.ty.as_ref() {
                Some(t) => self.resolve_ty(t),
                None => Type::Var(self.unify_table.fresh()),
            };
            param_types.push(pty);
        }
        self.set_param_types(node.params, &param_types);

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
            let _u = self.unify_table.unify(
                Type::Var(ret_var),
                body_ty,
                body_span,
                self.diags,
                self.file_id,
            );
        }

        if let Some(ann_ret) = expected_ret {
            let actual_ret = self.unify_table.resolve(Type::Var(ret_var));
            let _u =
                self.unify_table
                    .unify(ann_ret, actual_ret, node.span, self.diags, self.file_id);
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
            | BinOp::Shr => self
                .unify_table
                .unify(lhs_ty, rhs_ty, span, self.diags, self.file_id),

            // Logical: both sides must be Bool, result is Bool.
            BinOp::And | BinOp::Or | BinOp::Xor => {
                let _u = self.unify_table.unify(
                    Type::Prim(PrimTy::Bool),
                    lhs_ty,
                    span,
                    self.diags,
                    self.file_id,
                );
                let _v = self.unify_table.unify(
                    Type::Prim(PrimTy::Bool),
                    rhs_ty,
                    span,
                    self.diags,
                    self.file_id,
                );
                Type::Prim(PrimTy::Bool)
            }

            // Comparison: both sides must agree, result is Bool.
            BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                let _u = self
                    .unify_table
                    .unify(lhs_ty, rhs_ty, span, self.diags, self.file_id);
                Type::Prim(PrimTy::Bool)
            }

            BinOp::In => {
                // Element-in-collection; result is Bool.
                Type::Prim(PrimTy::Bool)
            }

            // Range: both sides Int, result is an array/range.
            BinOp::Range | BinOp::RangeExcl => {
                let _u = self
                    .unify_table
                    .unify(lhs_ty, rhs_ty, span, self.diags, self.file_id);
                // For now, return a fresh var (range type not yet modelled).
                Type::Var(self.unify_table.fresh())
            }

            BinOp::Cons => {
                // Cons: result type is an array of the lhs element type.
                Type::Array(Box::new(lhs_ty), None)
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
                self.infer_call(callee_ty, base, args, span, ctx)
            }

            PostfixOp::Index {
                args,
                span: idx_span,
            } => {
                let _base_ty = self.infer(base, ctx);
                for &a in args {
                    let _ty = self.infer(a, ctx);
                }
                let _ = idx_span;
                // Return type is a fresh var (array element type).
                Type::Var(self.unify_table.fresh())
            }

            PostfixOp::Field { .. } => {
                let _base_ty = self.infer(base, ctx);
                // Field access type -- needs record type knowledge (Phase 8).
                Type::Var(self.unify_table.fresh())
            }

            PostfixOp::RecDot { fields, .. } => {
                let base_ty = self.infer(base, ctx);
                self.infer_field_inits(fields, ctx);
                // Result is same type as base (record update).
                base_ty
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
                // Check if this is a generic call (has a DefId with scheme_vars).
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
                            // Instantiate with fresh vars.
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

                // Unify args.
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
                    let u = self.unify_table.unify(
                        param_ty.clone(),
                        arg_ty,
                        arg_span,
                        self.diags,
                        self.file_id,
                    );
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
                // Unknown function type -- infer args, build Arrow from result.
                let arg_types: Vec<Type> = args.iter().map(|&a| self.infer(a, ctx)).collect();
                let ret_var = self.unify_table.fresh();
                let arrow = Type::Arrow(arg_types, Box::new(Type::Var(ret_var)));
                let _u =
                    self.unify_table
                        .unify(Type::Var(v), arrow, span, self.diags, self.file_id);
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
            None => {
                // No else branch: unify result with Unit.
                self.unify_branch_type(result_var, Type::Prim(PrimTy::Unit), span)
            }
        }
    }

    fn unify_branch_type(&mut self, result_var: TypeVarId, branch_ty: Type, span: Span) -> Type {
        self.unify_table.unify(
            Type::Var(result_var),
            branch_ty,
            span,
            self.diags,
            self.file_id,
        )
    }

    fn check_guard(&mut self, guard: Option<Idx<Expr>>, ctx: &AstArenas) {
        if let Some(g) = guard {
            let gty = self.infer(g, ctx);
            let gspan = Self::expr_span(g, ctx);
            let _u = self.unify_table.unify(
                Type::Prim(PrimTy::Bool),
                gty,
                gspan,
                self.diags,
                self.file_id,
            );
        }
    }

    fn check_cond(&mut self, cond: &Cond, span: Span, ctx: &AstArenas) {
        match cond {
            Cond::Expr(e) => {
                let ty = self.infer(*e, ctx);
                let _u = self.unify_table.unify(
                    Type::Prim(PrimTy::Bool),
                    ty,
                    span,
                    self.diags,
                    self.file_id,
                );
            }
            Cond::Case { init, .. } => {
                let _ty = self.infer(*init, ctx);
            }
        }
    }

    fn infer_match_arm(&mut self, arm: &MatchArm, ctx: &AstArenas) -> Type {
        self.infer(arm.body, ctx)
    }

    fn set_pat_type(&mut self, pat: &Pat, ty: Type) {
        match pat {
            Pat::Ident { span, .. } => {
                if let Some(&def_id) = self.pat_defs.get(span) {
                    self.set_def_type(def_id, ty);
                }
            }
            Pat::Prod { elements, .. } => {
                // Tuple pattern: distribute.
                if let Type::Tuple(ref elems) = ty {
                    let len = elems.len().min(elements.len());
                    for i in 0..len {
                        self.set_pat_type(&elements[i], elems[i].clone());
                    }
                }
            }
            Pat::Wild { .. }
            | Pat::Lit { .. }
            | Pat::Error { .. }
            | Pat::Arr { .. }
            | Pat::AnonRec { .. }
            | Pat::Or { .. } => {}
        }
    }

    fn set_def_type(&mut self, def_id: DefId, ty: Type) {
        let idx = usize::try_from(def_id.0).expect("DefId in range");
        if let Some(info) = self.defs.get_mut(idx) {
            info.ty = Some(ty);
        }
    }

    fn def_type(&mut self, def_id: DefId, span: Span) -> Type {
        let idx = usize::try_from(def_id.0).expect("DefId in range");
        let info = self.defs.get(idx).expect("DefId is valid");
        if let Some(ty) = &info.ty {
            let ty = ty.clone();
            let scheme_vars = info.scheme_vars.clone();
            if scheme_vars.is_empty() {
                self.unify_table.resolve(ty)
            } else {
                // Instantiate generic type.
                let fresh: Vec<TypeVarId> = scheme_vars
                    .iter()
                    .map(|_| self.unify_table.fresh())
                    .collect();
                instantiate(&ty, &scheme_vars, &fresh)
            }
        } else {
            // Not yet typed; assign a fresh var.
            let v = self.unify_table.fresh();
            let _ = span;
            let idx2 = usize::try_from(def_id.0).expect("DefId in range");
            if let Some(info) = self.defs.get_mut(idx2) {
                info.ty = Some(Type::Var(v));
            }
            Type::Var(v)
        }
    }

    fn push_ty_param_scope(&mut self, ty_params: &[TyParam]) -> Vec<TypeVarId> {
        let mut frame: HashMap<Symbol, TypeVarId> = HashMap::new();
        let mut vars: Vec<TypeVarId> = Vec::new();
        for tp in ty_params {
            let v = self.unify_table.fresh();
            let _prev = frame.insert(tp.name, v);
            vars.push(v);
        }
        self.ty_scope.push(frame);
        vars
    }

    fn set_param_types(&mut self, params: &[Param], param_types: &[Type]) {
        for (p, pty) in params.iter().zip(param_types.iter()) {
            if let Some(&def_id) = self.pat_defs.get(&p.span) {
                self.set_def_type(def_id, pty.clone());
            }
        }
    }

    fn find_def_by_name(&self, name: Symbol) -> Option<DefId> {
        self.defs.iter().find(|d| d.name == name).map(|d| d.id)
    }

    fn expr_span(idx: Idx<Expr>, ctx: &AstArenas) -> Span {
        span_of_expr(ctx.exprs.get(idx))
    }
}

const fn span_of_expr(expr: &Expr) -> Span {
    match expr {
        Expr::Lit { span, .. }
        | Expr::Ident { span, .. }
        | Expr::Unit { span }
        | Expr::Paren { span, .. }
        | Expr::Tuple { span, .. }
        | Expr::Block { span, .. }
        | Expr::Array { span, .. }
        | Expr::AnonRec { span, .. }
        | Expr::If { span, .. }
        | Expr::Match { span, .. }
        | Expr::While { span, .. }
        | Expr::Loop { span, .. }
        | Expr::For { span, .. }
        | Expr::Label { span, .. }
        | Expr::Return { span, .. }
        | Expr::Break { span, .. }
        | Expr::Cycle { span, .. }
        | Expr::Defer { span, .. }
        | Expr::Import { span, .. }
        | Expr::Export { span, .. }
        | Expr::Using { span, .. }
        | Expr::Record { span, .. }
        | Expr::Choice { span, .. }
        | Expr::FnDef { span, .. }
        | Expr::Lambda { span, .. }
        | Expr::Bind { span, .. }
        | Expr::Prefix { span, .. }
        | Expr::Binary { span, .. }
        | Expr::Assign { span, .. }
        | Expr::Postfix { span, .. }
        | Expr::Error { span } => *span,
    }
}
