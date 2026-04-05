use music_arena::SliceRange;
use music_hir::{
    HirConstraint, HirEffectSet, HirExprId, HirExprKind, HirLetMods, HirOrigin, HirParam, HirPatId,
    HirTyId, HirTyKind,
};
use music_names::{Ident, NameBindingId, Symbol};

use super::super::CheckPass;
use super::super::exprs::check_expr;
use super::super::normalize::{
    lower_constraints, lower_effect_row, lower_params, lower_type_expr, type_mismatch,
};
use super::super::patterns::{bind_pat, bound_name_from_pat};
use super::super::schemes::BindingScheme;
use super::declarations::{check_bound_class, check_bound_data, check_bound_effect};
use super::effects::require_declared_effects;
use super::imports::{bind_imported_alias, bind_module_pattern, module_target_for_expr};
use crate::api::{ConstraintFacts, ExprFacts};
use crate::effects::EffectRow;

pub(in super::super) struct LetExprInput {
    pub(in super::super) origin: HirOrigin,
    pub(in super::super) mods: HirLetMods,
    pub(in super::super) pat: HirPatId,
    pub(in super::super) type_params: SliceRange<Ident>,
    pub(in super::super) has_param_clause: bool,
    pub(in super::super) params: SliceRange<HirParam>,
    pub(in super::super) constraints: SliceRange<HirConstraint>,
    pub(in super::super) effects: Option<HirEffectSet>,
    pub(in super::super) sig: Option<HirExprId>,
    pub(in super::super) value: HirExprId,
}

fn lower_let_type_params(
    ctx: &CheckPass<'_, '_, '_>,
    type_params: SliceRange<Ident>,
) -> Box<[Symbol]> {
    ctx.idents(type_params)
        .into_iter()
        .map(|ident| ident.name)
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn insert_let_binding_scheme(
    ctx: &mut CheckPass<'_, '_, '_>,
    binding: NameBindingId,
    ty: HirTyId,
    effects: EffectRow,
    type_params: Box<[Symbol]>,
    constraints: Box<[ConstraintFacts]>,
) {
    ctx.insert_binding_type(binding, ty);
    ctx.insert_binding_effects(binding, effects.clone());
    ctx.insert_binding_scheme(
        binding,
        BindingScheme {
            type_params,
            constraints,
            ty,
            effects,
        },
    );
}

fn check_callable_let_binding(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    params: SliceRange<HirParam>,
    effects: Option<&HirEffectSet>,
    declared_ty: Option<HirTyId>,
    value: HirExprId,
) -> (HirTyId, EffectRow) {
    let param_types = lower_params(ctx, params);
    let mut callable_effects = effects.map_or(EffectRow::empty(), |set| lower_effect_row(ctx, set));
    if let Some(expected) = declared_ty {
        ctx.push_expected_ty(expected);
    }
    let body_facts = check_expr(ctx, value);
    if declared_ty.is_some() {
        let _ = ctx.pop_expected_ty();
    }
    if effects.is_none() {
        callable_effects = body_facts.effects.clone();
    } else {
        callable_effects =
            require_declared_effects(ctx, origin, &callable_effects, &body_facts.effects);
    }
    let result_ty = declared_ty.unwrap_or(body_facts.ty);
    type_mismatch(ctx, origin, result_ty, body_facts.ty);
    let params = ctx.alloc_ty_list(param_types.iter().copied());
    let ty = ctx.alloc_ty(HirTyKind::Arrow {
        params,
        ret: result_ty,
        is_effectful: !callable_effects.is_pure(),
    });
    (ty, callable_effects)
}

pub(in super::super) fn check_let_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    input: LetExprInput,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let LetExprInput {
        origin,
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
    let bound_name = bound_name_from_pat(ctx, pat);
    let binding = bound_name.and_then(|ident| ctx.binding_id_for_decl(ident));
    let type_params = lower_let_type_params(ctx, type_params);
    let constraints = lower_constraints(ctx, constraints);
    let declared_ty = sig.map(|expr| {
        let origin = ctx.expr(expr).origin;
        lower_type_expr(ctx, expr, origin)
    });

    if mods.is_rec
        && let Some(binding) = binding
    {
        ctx.insert_binding_type(binding, declared_ty.unwrap_or(builtins.unknown));
    }

    let final_ty = if has_param_clause {
        let (ty, callable_effects) =
            check_callable_let_binding(ctx, origin, params, effects.as_ref(), declared_ty, value);
        if let Some(binding) = binding {
            insert_let_binding_scheme(ctx, binding, ty, callable_effects, type_params, constraints);
        }
        ty
    } else {
        let value_facts = if let Some(name) = bound_name {
            match ctx.expr(value).kind {
                HirExprKind::Data { variants, fields } => {
                    check_bound_data(ctx, name, variants, fields)
                }
                HirExprKind::Effect { members } => check_bound_effect(ctx, value, name, members),
                HirExprKind::Class {
                    constraints,
                    members,
                } => check_bound_class(ctx, value, name, constraints, members),
                _ => {
                    if let Some(expected) = declared_ty {
                        ctx.push_expected_ty(expected);
                    }
                    let facts = check_expr(ctx, value);
                    if declared_ty.is_some() {
                        let _ = ctx.pop_expected_ty();
                    }
                    facts
                }
            }
        } else {
            if let Some(expected) = declared_ty {
                ctx.push_expected_ty(expected);
            }
            let facts = check_expr(ctx, value);
            if declared_ty.is_some() {
                let _ = ctx.pop_expected_ty();
            }
            facts
        };
        let ty = declared_ty.unwrap_or(value_facts.ty);
        type_mismatch(ctx, origin, ty, value_facts.ty);
        if let Some(binding) = binding {
            insert_let_binding_scheme(
                ctx,
                binding,
                ty,
                EffectRow::empty(),
                type_params,
                constraints,
            );
        }
        ty
    };

    if !bind_module_pattern(ctx, pat, value) {
        bind_pat(ctx, pat, final_ty);
    }
    if let Some(binding) = binding
        && let Some(target) = module_target_for_expr(ctx, value)
    {
        ctx.insert_binding_module_target(binding, target);
    }
    if let Some(name) = bound_name {
        bind_imported_alias(ctx, name, value);
    }
    ExprFacts {
        ty: builtins.unit,
        effects: EffectRow::empty(),
    }
}
