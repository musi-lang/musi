//! Bidirectional type checker.

#[cfg(test)]
mod tests;

pub mod effects;
pub mod expr;
pub mod pat;
pub mod stmt;
pub mod ty;

use std::collections::HashMap;
use std::collections::hash_map::RandomState;
use std::hash::BuildHasher;
use std::mem;

use music_ast::ty::TyParam;
use music_ast::{AstArenas, ExprIdx};
use music_shared::{Arena, DiagnosticBag, FileId, Interner, Span, Symbol};

use crate::def::{DefId, DefKind, DefTable};
use crate::error::SemaError;
use crate::scope::{ScopeId, ScopeTree};
use crate::types::{DictLookup, EffectRow, InstanceInfo, Obligation, Type, TypeIdx, fmt_type};
use crate::unify::UnifyTable;
use crate::well_known::WellKnown;

/// Read-only environment for the type checker.
pub struct CheckContext<'a, S: BuildHasher = RandomState> {
    pub(crate) ast: &'a AstArenas,
    pub(crate) interner: &'a Interner,
    pub(crate) file_id: FileId,
    pub(crate) well_known: &'a WellKnown,
    pub(crate) expr_defs: &'a HashMap<ExprIdx, DefId>,
    pub(crate) pat_defs: &'a HashMap<Span, DefId>,
    /// Pre-computed types for import expressions, keyed by the import path symbol.
    /// Populated by the multi-file pipeline before sema runs.
    pub(crate) import_types: &'a HashMap<Symbol, TypeIdx, S>,
    /// Inferred law variables from the resolver, keyed by law span.
    pub(crate) law_inferred_vars: &'a HashMap<Span, Vec<(Symbol, DefId)>>,
    /// Maps (class `DefId`, operator `Symbol`) -> member `DefId` for operator dispatch.
    pub(crate) class_op_members: &'a HashMap<(DefId, Symbol), DefId>,
}

/// Mutable type-checking state built up during checking.
pub struct TypeStore {
    pub(crate) unify: UnifyTable,
    pub(crate) types: Arena<Type>,
    pub(crate) obligations: Vec<Obligation>,
    pub(crate) instances: Vec<InstanceInfo>,
    pub(crate) expr_types: HashMap<ExprIdx, TypeIdx>,
    /// Maps `BinOp` expression index -> the instance method `DefId` that handles it.
    pub(crate) binop_dispatch: HashMap<ExprIdx, DefId>,
    /// Maps `BinOp` expression -> dictionary lookup info for polymorphic dispatch.
    pub(crate) binop_dict_dispatch: HashMap<ExprIdx, DictLookup>,
    /// Maps function `DefId` -> ordered list of class constraints (for dict params).
    pub(crate) fn_constraints: HashMap<DefId, Vec<Obligation>>,
    /// In-scope class obligations for the current generic function.
    pub(crate) active_obligations: Vec<Obligation>,
}

pub struct Checker<'a, S: BuildHasher = RandomState> {
    pub(crate) ctx: CheckContext<'a, S>,
    pub(crate) store: TypeStore,
    pub(crate) diags: &'a mut DiagnosticBag,
    pub(crate) defs: &'a mut DefTable,
    pub(crate) scopes: &'a mut ScopeTree,
    pub(crate) current_scope: ScopeId,
    pub(crate) current_effects: EffectRow,
}

impl<'a, S: BuildHasher> Checker<'a, S> {
    /// Creates a checker with pre-populated type arena and unification table.
    #[must_use]
    pub fn new_with_state(
        ctx: CheckContext<'a, S>,
        diags: &'a mut DiagnosticBag,
        defs: &'a mut DefTable,
        scopes: &'a mut ScopeTree,
        scope: ScopeId,
        types: Arena<Type>,
        unify: UnifyTable,
    ) -> Self {
        Self {
            ctx,
            store: TypeStore {
                unify,
                types,
                obligations: vec![],
                instances: vec![],
                expr_types: HashMap::new(),
                binop_dispatch: HashMap::new(),
                binop_dict_dispatch: HashMap::new(),
                fn_constraints: HashMap::new(),
                active_obligations: vec![],
            },
            diags,
            defs,
            scopes,
            current_scope: scope,
            current_effects: EffectRow::PURE,
        }
    }

    /// Synthesises a type for `expr` (inference mode, direction ↑).
    pub fn synth(&mut self, expr: ExprIdx) -> TypeIdx {
        self::expr::synth(self, expr)
    }

    pub(crate) fn fresh_var(&mut self, span: Span) -> TypeIdx {
        self.store.unify.fresh(span, &mut self.store.types)
    }

    pub(crate) fn alloc_ty(&mut self, ty: Type) -> TypeIdx {
        self.store.types.alloc(ty)
    }

    /// Creates a `Type::Named` for a well-known type with no arguments.
    pub(crate) fn named_ty(&mut self, def: DefId) -> TypeIdx {
        self.store.types.alloc(Type::Named { def, args: vec![] })
    }

    /// Unifies two types, reporting a diagnostic on failure.
    pub(crate) fn unify_or_report(&mut self, expected: TypeIdx, found: TypeIdx, span: Span) {
        if !self
            .store
            .unify
            .unify(expected, found, &mut self.store.types, self.ctx.well_known)
        {
            let defs_vec: Vec<_> = self.defs.iter().cloned().collect();
            let exp_str = fmt_type(
                expected,
                &self.store.types,
                &defs_vec,
                self.ctx.interner,
                Some(&self.store.unify),
            );
            let found_str = fmt_type(
                found,
                &self.store.types,
                &defs_vec,
                self.ctx.interner,
                Some(&self.store.unify),
            );
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

    pub(crate) fn record_type(&mut self, expr: ExprIdx, ty: TypeIdx) {
        let _prev = self.store.expr_types.insert(expr, ty);
    }

    /// Allocates the error (poison) type.
    pub(crate) fn error_ty(&mut self) -> TypeIdx {
        self.store.types.alloc(Type::Error)
    }

    /// Resolves a type through any chain of unification bindings.
    pub(crate) fn resolve_ty(&self, ty: TypeIdx) -> TypeIdx {
        self.store.unify.resolve(ty, &self.store.types)
    }

    pub(crate) fn enter_ty_param_scope(&mut self, params: &[TyParam]) -> (ScopeId, Vec<DefId>) {
        let parent = self.current_scope;
        self.current_scope = self.scopes.push_child(parent);
        let mut def_ids = Vec::with_capacity(params.len());
        for param in params {
            let id = self
                .defs
                .alloc(param.name, DefKind::Type, param.span, self.ctx.file_id);
            let _prev = self.scopes.define(self.current_scope, param.name, id);
            def_ids.push(id);
        }
        (parent, def_ids)
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
                        fmt_type(
                            first_arg,
                            &self.store.types,
                            &defs_vec,
                            self.ctx.interner,
                            Some(&self.store.unify),
                        )
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
            binop_dispatch: self.store.binop_dispatch,
            binop_dict_dispatch: self.store.binop_dict_dispatch,
            fn_constraints: self.store.fn_constraints,
        }
    }
}

pub struct CheckerResult {
    pub types: Arena<Type>,
    pub unify: UnifyTable,
    pub expr_types: HashMap<ExprIdx, TypeIdx>,
    pub instances: Vec<InstanceInfo>,
    pub binop_dispatch: HashMap<ExprIdx, DefId>,
    pub binop_dict_dispatch: HashMap<ExprIdx, DictLookup>,
    pub fn_constraints: HashMap<DefId, Vec<Obligation>>,
}
