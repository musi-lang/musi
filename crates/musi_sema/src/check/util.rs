//! Utility methods: pattern typing, definition types, scope helpers.

use std::collections::HashMap;

use musi_ast::{AstArenas, Expr, Param, Pat, TyParam};
use musi_shared::{Idx, Span, Symbol};

use crate::def::DefId;
use crate::types::{Type, TypeVarId};

use super::{TypeChecker, instantiate};

impl TypeChecker<'_> {
    pub(super) fn set_pat_type(&mut self, pat: &Pat, ty: Type) {
        match pat {
            Pat::Ident { span, .. } => {
                if let Some(&def_id) = self.pat_defs.get(span) {
                    self.set_def_type(def_id, ty);
                }
            }
            Pat::Prod { elements, .. } => {
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
            | Pat::Or { .. }
            | Pat::DotPrefix { .. } => {}
        }
    }

    pub(super) fn set_def_type(&mut self, def_id: DefId, ty: Type) {
        let idx = usize::try_from(def_id.0).expect("DefId in range");
        if let Some(info) = self.defs.get_mut(idx) {
            info.ty = Some(ty);
        }
    }

    pub(super) fn def_type(&mut self, def_id: DefId, span: Span) -> Type {
        let idx = usize::try_from(def_id.0).expect("DefId in range");
        let info = self.defs.get(idx).expect("DefId is valid");
        if let Some(ty) = &info.ty {
            let ty = ty.clone();
            let scheme_vars = info.scheme_vars.clone();
            if scheme_vars.is_empty() {
                self.unify_table.resolve(ty)
            } else {
                let fresh: Vec<TypeVarId> = scheme_vars
                    .iter()
                    .map(|_| self.unify_table.fresh())
                    .collect();
                instantiate(&ty, &scheme_vars, &fresh)
            }
        } else {
            let v = self.unify_table.fresh();
            let _ = span;
            let idx2 = usize::try_from(def_id.0).expect("DefId in range");
            if let Some(info) = self.defs.get_mut(idx2) {
                info.ty = Some(Type::Var(v));
            }
            Type::Var(v)
        }
    }

    pub(super) fn push_ty_param_scope(&mut self, ty_params: &[TyParam]) -> Vec<TypeVarId> {
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

    /// Resolves parameter types (defaulting to fresh type vars) and records them.
    /// Returns the collected `Vec<Type>` for use in building function types.
    pub(super) fn collect_param_types(&mut self, params: &[Param]) -> Vec<Type> {
        let mut param_types: Vec<Type> = Vec::new();
        for p in params {
            let pty = match p.ty.as_ref() {
                Some(t) => self.resolve_ty(t),
                None => Type::Var(self.unify_table.fresh()),
            };
            param_types.push(pty);
        }
        self.set_param_types(params, &param_types);
        param_types
    }

    pub(super) fn set_param_types(&mut self, params: &[Param], param_types: &[Type]) {
        for (p, pty) in params.iter().zip(param_types.iter()) {
            if let Some(&def_id) = self.pat_defs.get(&p.span) {
                self.set_def_type(def_id, pty.clone());
            }
        }
    }

    /// Format a type for use in error messages, resolving `Named` to the real name.
    pub(super) fn fmt_ty_err(&self, ty: &Type) -> String {
        match ty {
            Type::Named(id, args) => {
                let name = self
                    .defs
                    .get(id.0 as usize)
                    .map(|d| self.interner.resolve(d.name))
                    .unwrap_or("?");
                if args.is_empty() {
                    name.to_owned()
                } else {
                    let as_ = args
                        .iter()
                        .map(|a| self.fmt_ty_err(a))
                        .collect::<Vec<_>>()
                        .join(", ");
                    format!("{name}[{as_}]")
                }
            }
            other => format!("{other}"),
        }
    }

    /// Unify `a` and `b`, emitting diagnostics with proper type names on mismatch.
    ///
    /// Intercepts `Named`-type mismatches before delegating to `unify_table.unify`
    /// so that error messages show real names instead of `Type(N)`.
    pub(super) fn unify(&mut self, a: Type, b: Type, span: Span) -> Type {
        let ra = self.unify_table.resolve(a);
        let rb = self.unify_table.resolve(b);

        // For mismatches involving Named types, build a human-readable message.
        // We do this before calling unify_table.unify so that we have interner access.
        match (&ra, &rb) {
            // Two different named types.
            (Type::Named(d1, _), Type::Named(d2, _)) if d1 != d2 => {
                let sa = self.fmt_ty_err(&ra);
                let sb = self.fmt_ty_err(&rb);
                let _d = self.diags.error(
                    format!("type mismatch: expected `{sa}`, found `{sb}`"),
                    span,
                    self.file_id,
                );
                return Type::Error;
            }
            // Named vs. a concrete non-var/non-error type.
            (Type::Named(..), rb_inner)
                if !matches!(rb_inner, Type::Var(_) | Type::Error | Type::Named(..)) =>
            {
                let sa = self.fmt_ty_err(&ra);
                let sb = self.fmt_ty_err(&rb);
                let _d = self.diags.error(
                    format!("type mismatch: expected `{sa}`, found `{sb}`"),
                    span,
                    self.file_id,
                );
                return Type::Error;
            }
            (ra_inner, Type::Named(..))
                if !matches!(ra_inner, Type::Var(_) | Type::Error | Type::Named(..)) =>
            {
                let sa = self.fmt_ty_err(&ra);
                let sb = self.fmt_ty_err(&rb);
                let _d = self.diags.error(
                    format!("type mismatch: expected `{sa}`, found `{sb}`"),
                    span,
                    self.file_id,
                );
                return Type::Error;
            }
            _ => {}
        }

        self.unify_table.unify(ra, rb, span, self.diags, self.file_id)
    }

    pub(super) fn find_def_by_name(&self, name: Symbol) -> Option<DefId> {
        self.defs.iter().find(|d| d.name == name).map(|d| d.id)
    }

    pub(super) fn expr_span(idx: Idx<Expr>, ctx: &AstArenas) -> Span {
        span_of_expr(ctx.exprs.get(idx))
    }
}

pub(super) const fn span_of_expr(expr: &Expr) -> Span {
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
        | Expr::ClassDef { span, .. }
        | Expr::GivenDef { span, .. }
        | Expr::Bind { span, .. }
        | Expr::Prefix { span, .. }
        | Expr::Binary { span, .. }
        | Expr::Assign { span, .. }
        | Expr::Postfix { span, .. }
        | Expr::DotPrefix { span, .. }
        | Expr::Error { span } => *span,
    }
}
