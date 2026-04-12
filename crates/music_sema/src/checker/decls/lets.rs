use music_arena::SliceRange;
use music_hir::{
    HirBinder, HirConstraint, HirEffectSet, HirExprId, HirExprKind, HirLetMods, HirMods, HirOrigin,
    HirParam, HirPatId, HirPatKind, HirTyId, HirTyKind,
};
use music_names::{Ident, NameBindingId, Symbol};

use super::super::CheckPass;
use super::super::DiagKind;
use super::super::decls::check_foreign_let;
use super::super::exprs::check_expr;
use super::super::patterns::{bind_pat, bound_name_from_pat, pat_is_irrefutable};
use super::super::schemes::BindingScheme;
use super::effects::require_declared_effects;
use super::imports::{bind_imported_alias, bind_module_pattern, module_target_for_expr};
use crate::api::{ConstraintFacts, ExprFacts};
use crate::effects::EffectRow;

type ConstraintFactsList = Box<[ConstraintFacts]>;

pub(in super::super) struct LetExprInput {
    pub(in super::super) expr_id: HirExprId,
    pub(in super::super) origin: HirOrigin,
    pub(in super::super) expr_mods: HirMods,
    pub(in super::super) mods: HirLetMods,
    pub(in super::super) pat: HirPatId,
    pub(in super::super) type_params: SliceRange<HirBinder>,
    pub(in super::super) has_param_clause: bool,
    pub(in super::super) params: SliceRange<HirParam>,
    pub(in super::super) constraints: SliceRange<HirConstraint>,
    pub(in super::super) effects: Option<HirEffectSet>,
    pub(in super::super) sig: Option<HirExprId>,
    pub(in super::super) value: HirExprId,
}

struct RecCallableSeed<'a> {
    binding: Option<NameBindingId>,
    mods: HirLetMods,
    param_types: &'a [HirTyId],
    effects: Option<&'a HirEffectSet>,
    declared_ty: Option<HirTyId>,
    type_params: &'a [Symbol],
    constraints: &'a [ConstraintFacts],
}

struct CallableLetCheckInput<'a> {
    origin: HirOrigin,
    exported: bool,
    mods: HirLetMods,
    pat: HirPatId,
    params: SliceRange<HirParam>,
    effects: Option<&'a HirEffectSet>,
    declared_ty: Option<HirTyId>,
    value: HirExprId,
    binding: Option<NameBindingId>,
    type_params: Box<[Symbol]>,
    constraints: ConstraintFactsList,
}

struct NonCallableLetCheckInput {
    origin: HirOrigin,
    exported: bool,
    mods: HirLetMods,
    pat: HirPatId,
    value: HirExprId,
    binding: Option<NameBindingId>,
    declared_ty: Option<HirTyId>,
    is_module_stmt: bool,
    bound_name: Option<Ident>,
    type_params: Box<[Symbol]>,
    constraints: ConstraintFactsList,
}

pub(in super::super) fn check_let_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    input: LetExprInput,
) -> ExprFacts {
    ctx.check_let_expr(input)
}

impl CheckPass<'_, '_, '_> {
    fn lower_let_type_params(&self, type_params: SliceRange<HirBinder>) -> Box<[Symbol]> {
        self.binders(type_params)
            .into_iter()
            .map(|binder| binder.name.name)
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    fn validate_non_callable_let_pattern(
        &mut self,
        origin: HirOrigin,
        pat: HirPatId,
        value: HirExprId,
        value_ty: HirTyId,
    ) {
        if !pat_is_irrefutable(self, pat) {
            self.diag(
                origin.span,
                DiagKind::PlainLetRequiresIrrefutablePattern,
                "",
            );
        }
        if matches!(self.ty(value_ty).kind, HirTyKind::Module)
            && matches!(self.pat(pat).kind, HirPatKind::Record { .. })
            && module_target_for_expr(self, value).is_none()
        {
            self.diag(
                origin.span,
                DiagKind::ModuleDestructuringRequiresStaticModule,
                "",
            );
        }
        if matches!(self.pat(pat).kind, HirPatKind::Record { .. })
            && !matches!(
                self.ty(value_ty).kind,
                HirTyKind::Record { .. } | HirTyKind::Module
            )
        {
            self.diag(
                origin.span,
                DiagKind::RecordDestructuringRequiresRecordOrModule,
                "",
            );
        }
    }

    fn insert_let_binding_scheme(
        &mut self,
        binding: NameBindingId,
        ty: HirTyId,
        effects: EffectRow,
        type_params: Box<[Symbol]>,
        constraints: ConstraintFactsList,
    ) {
        self.insert_binding_type(binding, ty);
        self.insert_binding_effects(binding, effects.clone());
        let evidence_keys = self
            .evidence_scope_for_constraints(&constraints)
            .into_keys()
            .collect::<Vec<_>>()
            .into_boxed_slice();
        self.insert_binding_scheme(
            binding,
            BindingScheme {
                type_params,
                constraints,
                ty,
                effects,
            },
        );
        self.set_binding_evidence_keys(binding, evidence_keys);
    }

    fn check_callable_let_binding(
        &mut self,
        origin: HirOrigin,
        param_types: &[HirTyId],
        constraints: &[ConstraintFacts],
        effects: Option<&HirEffectSet>,
        declared_ty: Option<HirTyId>,
        value: HirExprId,
    ) -> (HirTyId, EffectRow) {
        let mut callable_effects =
            effects.map_or(EffectRow::empty(), |set| self.lower_effect_row(set));
        let evidence_scope = self.evidence_scope_for_constraints(constraints);
        self.push_evidence_scope(evidence_scope);
        if let Some(expected) = declared_ty {
            self.push_expected_ty(expected);
        }
        let body_facts = check_expr(self, value);
        if declared_ty.is_some() {
            let _ = self.pop_expected_ty();
        }
        let _ = self.pop_evidence_scope();
        if effects.is_none() {
            callable_effects = body_facts.effects.clone();
        } else {
            callable_effects =
                require_declared_effects(self, origin, &callable_effects, &body_facts.effects);
        }
        let result_ty = declared_ty.unwrap_or(body_facts.ty);
        self.type_mismatch(origin, result_ty, body_facts.ty);
        let params = self.alloc_ty_list(param_types.iter().copied());
        let ty = self.alloc_ty(HirTyKind::Arrow {
            params,
            ret: result_ty,
            is_effectful: !callable_effects.is_pure(),
        });
        (ty, callable_effects)
    }

    fn seed_recursive_callable_scheme(&mut self, seed: &RecCallableSeed<'_>) {
        if !seed.mods.is_rec {
            return;
        }
        let Some(binding) = seed.binding else {
            return;
        };
        let builtins = self.builtins();
        let provisional_effects = seed
            .effects
            .map_or(EffectRow::empty(), |set| self.lower_effect_row(set));
        let provisional_ret = seed.declared_ty.unwrap_or(builtins.unknown);
        let params = self.alloc_ty_list(seed.param_types.iter().copied());
        let provisional_ty = self.alloc_ty(HirTyKind::Arrow {
            params,
            ret: provisional_ret,
            is_effectful: !provisional_effects.is_pure(),
        });
        self.insert_let_binding_scheme(
            binding,
            provisional_ty,
            provisional_effects,
            seed.type_params.to_vec().into_boxed_slice(),
            seed.constraints.to_vec().into_boxed_slice(),
        );
    }

    fn check_value_with_expected_ty(
        &mut self,
        declared_ty: Option<HirTyId>,
        value: HirExprId,
    ) -> ExprFacts {
        if let Some(expected) = declared_ty {
            self.push_expected_ty(expected);
        }
        let facts = check_expr(self, value);
        if declared_ty.is_some() {
            let _ = self.pop_expected_ty();
        }
        facts
    }

    fn check_non_callable_let_value(
        &mut self,
        origin: HirOrigin,
        is_module_stmt: bool,
        bound_name: Option<Ident>,
        declared_ty: Option<HirTyId>,
        value: HirExprId,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let Some(name) = bound_name.filter(|_| is_module_stmt) else {
            return self.check_value_with_expected_ty(declared_ty, value);
        };

        match &self.expr(value).kind {
            HirExprKind::Data { variants, fields } => {
                self.check_bound_data(name, variants.clone(), fields.clone())
            }
            HirExprKind::Effect { members } => {
                self.check_bound_effect(value, name, members.clone())
            }
            HirExprKind::Class {
                constraints,
                members,
            } => self.check_bound_class(value, name, constraints.clone(), members.clone()),
            HirExprKind::Instance {
                type_params,
                constraints,
                class,
                members,
            } => {
                let _ = self.check_instance_expr(
                    value,
                    origin,
                    *type_params,
                    constraints.clone(),
                    *class,
                    members,
                );
                ExprFacts::new(builtins.unit, EffectRow::empty())
            }
            _ => self.check_value_with_expected_ty(declared_ty, value),
        }
    }

    fn check_callable_let_expr(&mut self, input: CallableLetCheckInput<'_>) -> HirTyId {
        let CallableLetCheckInput {
            origin,
            exported,
            mods,
            pat,
            params,
            effects,
            declared_ty,
            value,
            binding,
            type_params,
            constraints,
        } = input;
        if !matches!(
            self.pat(pat).kind,
            HirPatKind::Bind { .. } | HirPatKind::Wildcard
        ) {
            self.diag(
                origin.span,
                DiagKind::CallableLetRequiresSimpleBindingPattern,
                "",
            );
        }
        if exported && !type_params.is_empty() && !constraints.is_empty() {
            self.diag(
                origin.span,
                DiagKind::ExportedCallableRequiresConcreteConstraints,
                "",
            );
        }
        let param_types = self.lower_params(params);
        self.seed_recursive_callable_scheme(&RecCallableSeed {
            binding,
            mods,
            param_types: &param_types,
            effects,
            declared_ty,
            type_params: &type_params,
            constraints: &constraints,
        });
        let (ty, callable_effects) = self.check_callable_let_binding(
            origin,
            &param_types,
            &constraints,
            effects,
            declared_ty,
            value,
        );
        if let Some(binding) = binding {
            self.insert_let_binding_scheme(binding, ty, callable_effects, type_params, constraints);
        }
        ty
    }

    fn check_non_callable_let_expr(&mut self, input: NonCallableLetCheckInput) -> HirTyId {
        let NonCallableLetCheckInput {
            origin,
            exported: _exported,
            mods,
            pat,
            value,
            binding,
            declared_ty,
            is_module_stmt,
            bound_name,
            type_params,
            constraints,
        } = input;
        let builtins = self.builtins();
        if !constraints.is_empty() {
            self.diag(origin.span, DiagKind::ConstrainedNonCallableBinding, "");
        }
        if mods.is_rec
            && let Some(binding) = binding
        {
            self.insert_binding_type(binding, declared_ty.unwrap_or(builtins.unknown));
        }
        let value_facts = self.check_non_callable_let_value(
            origin,
            is_module_stmt,
            bound_name,
            declared_ty,
            value,
        );
        self.validate_non_callable_let_pattern(origin, pat, value, value_facts.ty);
        let ty = declared_ty.unwrap_or(value_facts.ty);
        self.type_mismatch(origin, ty, value_facts.ty);
        if let Some(binding) = binding {
            self.insert_let_binding_scheme(
                binding,
                ty,
                EffectRow::empty(),
                type_params,
                constraints,
            );
        }
        ty
    }

    fn check_let_expr(&mut self, input: LetExprInput) -> ExprFacts {
        let builtins = self.builtins();
        let is_module_stmt = self.in_module_stmt();
        let LetExprInput {
            expr_id,
            origin,
            expr_mods,
            mods,
            pat,
            type_params,
            has_param_clause,
            params,
            constraints,
            effects,
            sig,
            value,
        } = input;
        let bound_name = bound_name_from_pat(self, pat);
        let binding = bound_name.and_then(|ident| self.binding_id_for_decl(ident));
        let type_params = self.lower_let_type_params(type_params);
        let constraints = self.lower_constraints(constraints);
        let declared_ty = sig.map(|expr| {
            let origin = self.expr(expr).origin;
            self.lower_type_expr(expr, origin)
        });

        let final_ty = if is_module_stmt && expr_mods.foreign.is_some() {
            check_foreign_let(self, expr_id).unwrap_or(builtins.unknown)
        } else if has_param_clause {
            self.check_callable_let_expr(CallableLetCheckInput {
                origin,
                exported: expr_mods.export.is_some(),
                mods,
                pat,
                params,
                effects: effects.as_ref(),
                declared_ty,
                value,
                binding,
                type_params,
                constraints,
            })
        } else {
            self.check_non_callable_let_expr(NonCallableLetCheckInput {
                origin,
                exported: expr_mods.export.is_some(),
                mods,
                pat,
                value,
                binding,
                declared_ty,
                is_module_stmt,
                bound_name,
                type_params,
                constraints,
            })
        };

        if !bind_module_pattern(self, pat, value) {
            bind_pat(self, pat, final_ty);
        }
        if let Some(binding) = binding
            && let Some(target) = module_target_for_expr(self, value)
        {
            self.insert_binding_module_target(binding, target);
        }
        if let Some(name) = bound_name {
            bind_imported_alias(self, name, value);
        }
        ExprFacts::new(builtins.unit, EffectRow::empty())
    }
}
