//! Bidirectional type checker.

#[cfg(test)]
mod tests;

pub mod decl;
pub mod effects;
pub mod expr;
pub mod pat;
pub mod ty;

use std::collections::HashMap;
use std::mem;

use music_ast::{AstArenas, Expr};
use music_shared::{Arena, DiagnosticBag, FileId, Idx, Interner, Span};

use crate::def::{DefId, DefTable};
use crate::error::SemaError;
use crate::scope::{ScopeId, ScopeTree};
use crate::types::{EffectRow, InstanceInfo, Obligation, Type, fmt_type};
use crate::unify::UnifyTable;
use crate::well_known::WellKnown;

/// Read-only environment for the type checker.
pub struct CheckContext<'a> {
    pub(crate) ast: &'a AstArenas,
    pub(crate) interner: &'a Interner,
    pub(crate) file_id: FileId,
    pub(crate) well_known: &'a WellKnown,
    pub(crate) expr_defs: &'a HashMap<Idx<Expr>, DefId>,
}

/// Mutable type-checking state built up during checking.
pub struct TypeStore {
    pub(crate) unify: UnifyTable,
    pub(crate) types: Arena<Type>,
    pub(crate) obligations: Vec<Obligation>,
    pub(crate) instances: Vec<InstanceInfo>,
    pub(crate) expr_types: HashMap<Idx<Expr>, Idx<Type>>,
}

pub struct Checker<'a> {
    pub(crate) ctx: CheckContext<'a>,
    pub(crate) store: TypeStore,
    pub(crate) diags: &'a mut DiagnosticBag,
    pub(crate) defs: &'a mut DefTable,
    pub(crate) scopes: &'a mut ScopeTree,
    pub(crate) current_scope: ScopeId,
    pub(crate) current_effects: EffectRow,
}

impl<'a> Checker<'a> {
    #[must_use]
    pub fn new(
        ctx: CheckContext<'a>,
        diags: &'a mut DiagnosticBag,
        defs: &'a mut DefTable,
        scopes: &'a mut ScopeTree,
        scope: ScopeId,
    ) -> Self {
        Self {
            ctx,
            store: TypeStore {
                unify: UnifyTable::new(),
                types: Arena::new(),
                obligations: vec![],
                instances: vec![],
                expr_types: HashMap::new(),
            },
            diags,
            defs,
            scopes,
            current_scope: scope,
            current_effects: EffectRow::PURE,
        }
    }

    /// Synthesises a type for `expr` (inference mode, direction ↑).
    pub fn synth(&mut self, expr: Idx<Expr>) -> Idx<Type> {
        self::expr::synth(self, expr)
    }

    /// Checks `expr` against `expected` (checking mode, direction ↓).
    pub fn check(&mut self, expr: Idx<Expr>, expected: Idx<Type>) {
        self::expr::check(self, expr, expected);
    }

    pub(crate) fn fresh_var(&mut self, span: Span) -> Idx<Type> {
        self.store.unify.fresh(span, &mut self.store.types)
    }

    pub(crate) fn alloc_ty(&mut self, ty: Type) -> Idx<Type> {
        self.store.types.alloc(ty)
    }

    /// Creates a `Type::Named` for a well-known type with no arguments.
    pub(crate) fn named_ty(&mut self, def: DefId) -> Idx<Type> {
        self.store.types.alloc(Type::Named { def, args: vec![] })
    }

    /// Unifies two types, reporting a diagnostic on failure.
    pub(crate) fn unify_or_report(&mut self, expected: Idx<Type>, found: Idx<Type>, span: Span) {
        if !self
            .store
            .unify
            .unify(expected, found, &mut self.store.types, self.ctx.well_known)
        {
            let defs_vec: Vec<_> = self.defs.iter().cloned().collect();
            let exp_str = fmt_type(expected, &self.store.types, &defs_vec, self.ctx.interner);
            let found_str = fmt_type(found, &self.store.types, &defs_vec, self.ctx.interner);
            let _d = self.diags.report(
                &SemaError::TypeMismatch {
                    expected: exp_str,
                    found: found_str,
                },
                span,
                self.ctx.file_id,
            );
        }
    }

    pub(crate) fn record_type(&mut self, expr: Idx<Expr>, ty: Idx<Type>) {
        let _prev = self.store.expr_types.insert(expr, ty);
    }

    /// Allocates the error (poison) type.
    pub(crate) fn error_ty(&mut self) -> Idx<Type> {
        self.store.types.alloc(Type::Error)
    }

    /// Resolves a type through any chain of unification bindings.
    pub(crate) fn resolve_ty(&self, ty: Idx<Type>) -> Idx<Type> {
        self.store.unify.resolve(ty, &self.store.types)
    }

    pub fn resolve_obligations(&mut self) {
        let obligations = mem::take(&mut self.store.obligations);
        for obligation in &obligations {
            let resolved = self.store.instances.iter().any(|inst| {
                inst.class == obligation.class
                    && self.store.unify.unify(
                        inst.target,
                        obligation
                            .args
                            .first()
                            .copied()
                            .unwrap_or_else(|| self.store.types.alloc(Type::Error)),
                        &mut self.store.types,
                        self.ctx.well_known,
                    )
            });
            if !resolved {
                let class_name = self
                    .ctx
                    .interner
                    .resolve(self.defs.get(obligation.class).name);
                let defs_vec: Vec<_> = self.defs.iter().cloned().collect();
                let ty_str = obligation.args.first().map_or_else(
                    || Box::from("_"),
                    |&first_arg| {
                        fmt_type(first_arg, &self.store.types, &defs_vec, self.ctx.interner)
                    },
                );
                let _d = self.diags.report(
                    &SemaError::NoInstance {
                        class: Box::from(class_name),
                        ty: ty_str,
                    },
                    obligation.span,
                    self.ctx.file_id,
                );
            }
        }
    }

    #[must_use]
    pub fn finish(self) -> CheckerResult {
        CheckerResult {
            types: self.store.types,
            unify: self.store.unify,
            expr_types: self.store.expr_types,
            instances: self.store.instances,
        }
    }
}

pub struct CheckerResult {
    pub types: Arena<Type>,
    pub unify: UnifyTable,
    pub expr_types: HashMap<Idx<Expr>, Idx<Type>>,
    pub instances: Vec<InstanceInfo>,
}
