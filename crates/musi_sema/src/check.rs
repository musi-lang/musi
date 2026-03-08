//! Bidirectional type checker with Hindley–Milner–style inference.
//!
//! Walks the AST in expression order, infers types bottom-up and checks them
//! top-down.  Unification is handled by a standard union-find table.
//!
//! The output is a side table mapping each `Idx<Expr>` to its [`Type`].  The
//! [`DefInfo`] entries for all resolved bindings are updated in place with the
//! inferred types.

mod infer;
mod util;

use std::collections::HashMap;

use musi_ast::{AstArenas, Expr, ParsedModule, Ty};
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
                self.vars[idx]
                    .as_ref()
                    .map_or(Type::Var(v), |bound| self.resolve(bound.clone()))
            }
            other => other,
        }
    }

    /// Recursively resolves bound type variables and erases free ones to [`Type::Error`].
    ///
    /// Call this before exporting a type from a module so that `TypeVarId`s
    /// do not escape into a different `UnifyTable` (which would cause an OOB panic).
    ///
    /// # Panics
    ///
    /// Panics if a `TypeVarId` is not valid for this table (i.e., if it is out of bounds).
    #[must_use]
    pub fn freeze_type(&self, ty: Type) -> Type {
        match ty {
            Type::Var(v) => {
                let idx = usize::try_from(v.0).expect("TypeVarId in range");
                match self.vars.get(idx) {
                    Some(Some(bound)) => self.freeze_type(bound.clone()),
                    _ => Type::Error, // free or foreign var → erase
                }
            }
            Type::Arrow(params, ret) => Type::Arrow(
                params.into_iter().map(|p| self.freeze_type(p)).collect(),
                Box::new(self.freeze_type(*ret)),
            ),
            Type::Tuple(elems) => {
                Type::Tuple(elems.into_iter().map(|e| self.freeze_type(e)).collect())
            }
            Type::Array(elem, size) => Type::Array(Box::new(self.freeze_type(*elem)), size),
            Type::Named(id, args) => {
                Type::Named(id, args.into_iter().map(|a| self.freeze_type(a)).collect())
            }
            other => other,
        }
    }

    fn unify_sequence(
        &mut self,
        a: Vec<Type>,
        b: Vec<Type>,
        span: Span,
        diags: &mut DiagnosticBag,
        file_id: FileId,
    ) -> Vec<Type> {
        a.into_iter()
            .zip(b)
            .map(|(ta, tb)| self.unify(ta, tb, span, diags, file_id))
            .collect()
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
                self.vars[idx]
                    .as_ref()
                    .is_some_and(|bound| self.occurs(v, bound))
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
            (ref ra, ref rb) if ra == rb => ra.clone(),
            (Type::Error, _) | (_, Type::Error) => Type::Error,
            (Type::Var(v), t) | (t, Type::Var(v)) => self.unify_var(v, t, span, diags, file_id),
            (Type::Prim(pa), Type::Prim(pb)) => Self::unify_prim(pa, pb, span, diags, file_id),
            (Type::Arrow(p1, r1), Type::Arrow(p2, r2)) => {
                self.unify_arrow(p1, *r1, p2, *r2, span, diags, file_id)
            }
            (Type::Named(d1, a1), Type::Named(d2, a2)) => {
                self.unify_named(d1, a1, d2, a2, span, diags, file_id)
            }
            (Type::Array(elem1, size1), Type::Array(elem2, size2)) => {
                self.unify_array(*elem1, size1, *elem2, size2, span, diags, file_id)
            }
            (Type::Tuple(a), Type::Tuple(b)) => self.unify_tuple(a, b, span, diags, file_id),
            (a, b) => Self::unify_mismatch(&a, &b, span, diags, file_id),
        }
    }

    fn unify_var(
        &mut self,
        v: TypeVarId,
        t: Type,
        span: Span,
        diags: &mut DiagnosticBag,
        file_id: FileId,
    ) -> Type {
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

    fn unify_prim(
        pa: PrimTy,
        pb: PrimTy,
        span: Span,
        diags: &mut DiagnosticBag,
        file_id: FileId,
    ) -> Type {
        let _d = diags.error(
            format!("type mismatch: expected `{pa}`, found `{pb}`"),
            span,
            file_id,
        );
        Type::Error
    }

    #[allow(clippy::too_many_arguments)]
    fn unify_arrow(
        &mut self,
        p1: Vec<Type>,
        r1: Type,
        p2: Vec<Type>,
        r2: Type,
        span: Span,
        diags: &mut DiagnosticBag,
        file_id: FileId,
    ) -> Type {
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
        let params = self.unify_sequence(p1, p2, span, diags, file_id);
        let ret = Box::new(self.unify(r1, r2, span, diags, file_id));
        Type::Arrow(params, ret)
    }

    #[allow(clippy::too_many_arguments)]
    fn unify_named(
        &mut self,
        d1: DefId,
        a1: Vec<Type>,
        d2: DefId,
        a2: Vec<Type>,
        span: Span,
        diags: &mut DiagnosticBag,
        file_id: FileId,
    ) -> Type {
        if d1 != d2 {
            let _d = diags.error(
                String::from("type mismatch: incompatible named types"),
                span,
                file_id,
            );
            return Type::Error;
        }
        if a1.len() != a2.len() {
            let _d = diags.error(String::from("type argument count mismatch"), span, file_id);
            return Type::Error;
        }
        let args = self.unify_sequence(a1, a2, span, diags, file_id);
        Type::Named(d1, args)
    }

    #[allow(clippy::too_many_arguments)]
    fn unify_array(
        &mut self,
        elem1: Type,
        size1: Option<usize>,
        elem2: Type,
        size2: Option<usize>,
        span: Span,
        diags: &mut DiagnosticBag,
        file_id: FileId,
    ) -> Type {
        if size1 != size2 {
            let _d = diags.error(
                format!(
                    "array size mismatch: expected {}, found {}",
                    size1.map_or_else(|| "unsized".to_owned(), |n| n.to_string()),
                    size2.map_or_else(|| "unsized".to_owned(), |n| n.to_string()),
                ),
                span,
                file_id,
            );
            return Type::Error;
        }
        let elem = self.unify(elem1, elem2, span, diags, file_id);
        Type::Array(Box::new(elem), size1)
    }

    fn unify_tuple(
        &mut self,
        a: Vec<Type>,
        b: Vec<Type>,
        span: Span,
        diags: &mut DiagnosticBag,
        file_id: FileId,
    ) -> Type {
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
        let elems = self.unify_sequence(a, b, span, diags, file_id);
        Type::Tuple(elems)
    }

    fn unify_mismatch(
        a: &Type,
        b: &Type,
        span: Span,
        diags: &mut DiagnosticBag,
        file_id: FileId,
    ) -> Type {
        let _d = diags.error(
            format!("type mismatch: expected `{a}`, found `{b}`"),
            span,
            file_id,
        );
        Type::Error
    }
}

impl Default for UnifyTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Substitutes `scheme_vars[i]` → `fresh_vars[i]` throughout `ty`.
pub(super) fn instantiate(ty: &Type, scheme_vars: &[TypeVarId], fresh_vars: &[TypeVarId]) -> Type {
    match ty {
        Type::Var(v) => scheme_vars
            .iter()
            .position(|sv| sv == v)
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
pub(super) struct FnDefNode<'a> {
    pub(super) name: musi_shared::Symbol,
    pub(super) ty_params: &'a [musi_ast::TyParam],
    pub(super) params: &'a [musi_ast::Param],
    pub(super) ret_ty: Option<&'a musi_ast::Ty>,
    pub(super) body: Option<musi_shared::Idx<musi_ast::Expr>>,
    pub(super) span: musi_shared::Span,
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
    /// Record field types collected from `record` definitions: type `DefId` → field name → type.
    record_fields: HashMap<DefId, HashMap<Symbol, Type>>,
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
            record_fields: HashMap::new(),
        }
    }

    /// Type-checks an entire module, walking each top-level item in order.
    pub fn check_module(&mut self, module: &ParsedModule) {
        for &item_idx in module.ctx.expr_lists.get_slice(module.items) {
            let ty = self.infer(item_idx, &module.ctx);
            let _prev = self.expr_types.insert(item_idx, ty);
        }
    }

    /// Records the inferred type for `idx` and returns it.
    pub fn infer(&mut self, idx: Idx<Expr>, ctx: &AstArenas) -> Type {
        let ty = self.infer_expr(idx, ctx);
        let resolved = self.unify_table.resolve(ty);
        let _prev = self.expr_types.insert(idx, resolved.clone());
        resolved
    }

    fn resolve_ty(&self, ty: &Ty) -> Type {
        match ty {
            Ty::Named { name, args, .. } => {
                let name_str = self.interner.resolve(*name);
                if let Some(prim) = PrimTy::from_name(name_str) {
                    if args.is_empty() {
                        return Type::Prim(prim);
                    }
                    return Type::Prim(prim);
                }
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
            Ty::Option { inner, .. } => {
                let inner_ty = self.resolve_ty(inner);
                self.find_type_def_by_str("Option")
                    .map_or(Type::Error, |opt_id| Type::Named(opt_id, vec![inner_ty]))
            }
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

    /// Finds the [`DefId`] for a user-defined type by string name.
    fn find_type_def_by_str(&self, name_str: &str) -> Option<DefId> {
        self.defs
            .iter()
            .find(|d| d.kind == DefKind::Type && self.interner.resolve(d.name) == name_str)
            .map(|d| d.id)
    }
}
