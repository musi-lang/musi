use std::collections::HashMap;
use std::iter::repeat;

use music_hir::{HirOrigin, HirTyId, HirTyKind};

use crate::api::{ConstraintAnswer, ConstraintFacts, ConstraintKey, ConstraintKind};
use crate::checker::{CheckPass, DiagKind};
use crate::effects::EffectRow;

use super::{BindingScheme, ConstraintObligation, InstantiatedBinding, TypeSubstMap};

impl CheckPass<'_, '_, '_> {
    pub fn scheme_value_ty(&mut self, scheme: &BindingScheme) -> HirTyId {
        if !matches!(self.ty(scheme.ty).kind, HirTyKind::Arrow { .. }) {
            return scheme.ty;
        }
        let mut ty = scheme.ty;
        let type_ty = self.builtins().type_;
        let kinds = scheme
            .type_param_kinds
            .iter()
            .copied()
            .chain(repeat(type_ty))
            .take(scheme.type_params.len())
            .collect::<Vec<_>>();
        for (binder, binder_ty) in scheme.type_params.iter().copied().zip(kinds).rev() {
            ty = self.alloc_ty(HirTyKind::Pi {
                binder,
                binder_ty,
                body: ty,
                is_effectful: false,
            });
        }
        ty
    }

    pub fn instantiate_pi_ty(
        &mut self,
        origin: HirOrigin,
        ty: HirTyId,
        args: &[HirTyId],
    ) -> Option<InstantiatedBinding> {
        let mut binders = Vec::new();
        let mut body = ty;
        while let HirTyKind::Pi {
            binder,
            binder_ty: _,
            body: next,
            is_effectful: _,
        } = self.ty(body).kind
        {
            binders.push(binder);
            body = next;
        }
        if binders.is_empty() {
            return None;
        }
        let scheme = BindingScheme {
            type_param_kinds: Box::default(),
            type_params: binders.into_boxed_slice(),
            param_names: Box::default(),
            comptime_params: Box::default(),
            constraints: Box::default(),
            ty: body,
            effects: EffectRow::empty(),
        };
        self.instantiate_binding_scheme(origin, &scheme, args)
    }

    pub fn instantiate_binding_scheme(
        &mut self,
        origin: HirOrigin,
        scheme: &BindingScheme,
        args: &[HirTyId],
    ) -> Option<InstantiatedBinding> {
        let ctx = self;
        if scheme.type_params.len() != args.len() {
            ctx.diag(origin.span, DiagKind::TypeApplicationArityMismatch, "");
            return None;
        }
        let subst = scheme
            .type_params
            .iter()
            .copied()
            .zip(args.iter().copied())
            .collect::<TypeSubstMap>();
        Some(ctx.instantiate_binding_with_subst(scheme, &subst))
    }

    pub fn instantiate_monomorphic_scheme(
        &mut self,
        scheme: &BindingScheme,
    ) -> InstantiatedBinding {
        self.instantiate_binding_with_subst(scheme, &TypeSubstMap::new())
    }

    pub fn resolve_obligations_to_answers(
        &mut self,
        origin: HirOrigin,
        obligations: &[ConstraintObligation],
    ) -> Option<Box<[ConstraintAnswer]>> {
        let mut stack = Vec::<String>::new();
        let mut answers = Vec::new();
        for obligation in obligations {
            if !matches!(obligation.kind, ConstraintKind::Implements) {
                let _ = self.solve_obligation(origin, obligation, &mut stack);
                continue;
            }
            answers.push(self.resolve_obligation_answer(origin, obligation, &mut stack)?);
        }
        Some(answers.into_boxed_slice())
    }

    pub fn answer_scope_for_constraints(
        &mut self,
        constraints: &[ConstraintFacts],
    ) -> HashMap<ConstraintKey, ConstraintAnswer> {
        constraints
            .iter()
            .filter_map(|constraint| {
                self.constraint_key_for_facts(constraint).map(|key| {
                    let answer = ConstraintAnswer::Param { key: key.clone() };
                    (key, answer)
                })
            })
            .collect()
    }

    pub(in crate::checker) fn instantiate_binding_with_subst(
        &mut self,
        scheme: &BindingScheme,
        subst: &TypeSubstMap,
    ) -> InstantiatedBinding {
        let ctx = self;
        let ty = ctx.substitute_ty(scheme.ty, subst);
        let effects = ctx.substitute_effect_row(&scheme.effects, subst);
        let obligations = scheme
            .constraints
            .iter()
            .map(|constraint| ctx.instantiate_obligation(constraint, subst))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        InstantiatedBinding {
            ty,
            effects,
            obligations,
        }
    }

    pub(super) fn instantiate_obligation(
        &mut self,
        constraint: &ConstraintFacts,
        subst: &TypeSubstMap,
    ) -> ConstraintObligation {
        let ctx = self;
        ConstraintObligation {
            kind: constraint.kind,
            subject: subst
                .get(&constraint.name)
                .copied()
                .unwrap_or_else(|| ctx.named_type_for_symbol(constraint.name)),
            value: ctx.substitute_ty(constraint.value, subst),
            shape_key: constraint.shape_key.clone(),
        }
    }

    pub(in crate::checker) fn constraint_key_for_facts(
        &mut self,
        constraint: &ConstraintFacts,
    ) -> Option<ConstraintKey> {
        matches!(constraint.kind, ConstraintKind::Implements).then(|| {
            ConstraintKey::new(
                constraint.kind,
                self.named_type_for_symbol(constraint.name),
                constraint.value,
                constraint.shape_key.clone(),
            )
        })
    }
}
