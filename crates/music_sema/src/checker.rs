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

use music_ast::AstArenas;
use music_shared::{Arena, DiagnosticBag, FileId, Idx, Interner, Span};

use crate::def::{DefId, DefTable};
use crate::error::SemaError;
use crate::scope::{ScopeId, ScopeTree};
use crate::types::{EffectRow, InstanceInfo, Obligation, Type, fmt_type};
use crate::unify::UnifyTable;
use crate::well_known::WellKnown;

/// The bidirectional type checker.
pub struct Checker<'a> {
    pub(crate) ast: &'a AstArenas,
    pub(crate) interner: &'a Interner,
    pub(crate) file_id: FileId,
    pub(crate) diags: &'a mut DiagnosticBag,
    pub(crate) defs: &'a mut DefTable,
    pub(crate) scopes: &'a mut ScopeTree,
    pub(crate) unify: UnifyTable,
    pub(crate) types: Arena<Type>,
    pub(crate) well_known: &'a WellKnown,
    pub(crate) current_scope: ScopeId,
    pub(crate) current_effects: EffectRow,
    pub(crate) obligations: Vec<Obligation>,
    pub(crate) instances: Vec<InstanceInfo>,
    pub(crate) expr_types: HashMap<Idx<music_ast::Expr>, Idx<Type>>,
    pub(crate) expr_defs: &'a HashMap<Idx<music_ast::Expr>, DefId>,
}

impl<'a> Checker<'a> {
    /// Creates a new type checker.
    #[must_use]
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        ast: &'a AstArenas,
        interner: &'a Interner,
        file_id: FileId,
        diags: &'a mut DiagnosticBag,
        defs: &'a mut DefTable,
        scopes: &'a mut ScopeTree,
        well_known: &'a WellKnown,
        scope: ScopeId,
        expr_defs: &'a HashMap<Idx<music_ast::Expr>, DefId>,
    ) -> Self {
        Self {
            ast,
            interner,
            file_id,
            diags,
            defs,
            scopes,
            unify: UnifyTable::new(),
            types: Arena::new(),
            well_known,
            current_scope: scope,
            current_effects: EffectRow::PURE,
            obligations: Vec::new(),
            instances: Vec::new(),
            expr_types: HashMap::new(),
            expr_defs,
        }
    }

    /// Synthesises a type for `expr` (inference mode, direction ↑).
    pub fn synth(&mut self, expr: Idx<music_ast::Expr>) -> Idx<Type> {
        self::expr::synth(self, expr)
    }

    /// Checks `expr` against `expected` (checking mode, direction ↓).
    pub fn check(&mut self, expr: Idx<music_ast::Expr>, expected: Idx<Type>) {
        self::expr::check(self, expr, expected);
    }

    /// Creates a fresh unification variable.
    pub(crate) fn fresh_var(&mut self, span: Span) -> Idx<Type> {
        self.unify.fresh(span, &mut self.types)
    }

    /// Allocates a type in the arena.
    pub(crate) fn alloc_ty(&mut self, ty: Type) -> Idx<Type> {
        self.types.alloc(ty)
    }

    /// Creates a `Type::Named` for a well-known type with no arguments.
    pub(crate) fn named_ty(&mut self, def: DefId) -> Idx<Type> {
        self.types.alloc(Type::Named {
            def,
            args: Vec::new(),
        })
    }

    /// Unifies two types, reporting a diagnostic on failure.
    pub(crate) fn unify_or_report(&mut self, expected: Idx<Type>, found: Idx<Type>, span: Span) {
        if !self
            .unify
            .unify(expected, found, &mut self.types, self.well_known)
        {
            let defs_vec: Vec<_> = self.defs.iter().cloned().collect();
            let exp_str = fmt_type(expected, &self.types, &defs_vec, self.interner);
            let found_str = fmt_type(found, &self.types, &defs_vec, self.interner);
            let _d = self.diags.report(
                &SemaError::TypeMismatch {
                    expected: exp_str,
                    found: found_str,
                },
                span,
                self.file_id,
            );
        }
    }

    /// Records the inferred type for an expression.
    pub(crate) fn record_type(&mut self, expr: Idx<music_ast::Expr>, ty: Idx<Type>) {
        let _prev = self.expr_types.insert(expr, ty);
    }

    /// Allocates the error (poison) type.
    pub(crate) fn error_ty(&mut self) -> Idx<Type> {
        self.types.alloc(Type::Error)
    }

    /// Resolves a type through any chain of unification bindings.
    pub(crate) fn resolve_ty(&self, ty: Idx<Type>) -> Idx<Type> {
        self.unify.resolve(ty, &self.types)
    }

    /// Resolves pending typeclass obligations.
    pub fn resolve_obligations(&mut self) {
        let obligations = mem::take(&mut self.obligations);
        for obligation in &obligations {
            let resolved = self.instances.iter().any(|inst| {
                inst.class == obligation.class
                    && self.unify.unify(
                        inst.target,
                        obligation
                            .args
                            .first()
                            .copied()
                            .unwrap_or_else(|| self.types.alloc(Type::Error)),
                        &mut self.types,
                        self.well_known,
                    )
            });
            if !resolved {
                let class_name = self.interner.resolve(self.defs.get(obligation.class).name);
                let defs_vec: Vec<_> = self.defs.iter().cloned().collect();
                let ty_str = obligation.args.first().map_or_else(
                    || Box::from("_"),
                    |&first_arg| fmt_type(first_arg, &self.types, &defs_vec, self.interner),
                );
                let _d = self.diags.report(
                    &SemaError::NoInstance {
                        class: Box::from(class_name),
                        ty: ty_str,
                    },
                    obligation.span,
                    self.file_id,
                );
            }
        }
    }

    /// Consumes the checker and returns the collected results.
    #[must_use]
    pub fn finish(self) -> CheckerResult {
        CheckerResult {
            types: self.types,
            unify: self.unify,
            expr_types: self.expr_types,
            instances: self.instances,
        }
    }
}

/// Result of the type checking phase.
pub struct CheckerResult {
    pub types: Arena<Type>,
    pub unify: UnifyTable,
    pub expr_types: HashMap<Idx<music_ast::Expr>, Idx<Type>>,
    pub instances: Vec<InstanceInfo>,
}
