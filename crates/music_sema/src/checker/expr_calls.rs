use music_arena::SliceRange;
use music_hir::{HirArg, HirDim, HirExprId, HirExprKind, HirOrigin, HirTyId, HirTyKind};

use crate::api::ExprFacts;
use crate::effects::EffectRow;

use super::decls::{call_effects_for_expr, module_export_for_expr, module_target_for_expr};
use super::exprs::{check_expr, peel_mut_ty};
use super::normalize::{lower_type_expr, type_mismatch};
use super::schemes::{
    BindingScheme, instantiate_binding_scheme, instantiate_monomorphic_scheme, scheme_from_export,
    solve_obligations,
};
use super::{CheckPass, DiagKind};

pub(super) fn check_call_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    callee: HirExprId,
    args: SliceRange<HirArg>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let callee_facts = check_expr(ctx, callee);
    let (params, ret) = if let HirTyKind::Arrow { params, ret, .. } = ctx.ty(callee_facts.ty).kind {
        (ctx.ty_ids(params), ret)
    } else {
        ctx.diag(origin.span, DiagKind::InvalidCallTarget, "");
        return ExprFacts {
            ty: builtins.unknown,
            effects: callee_facts.effects,
        };
    };

    let args_vec = ctx.args(args);
    let mut effects = callee_facts.effects;
    let mut param_index = 0usize;
    let mut has_runtime_spread = false;

    for arg in &args_vec {
        check_call_arg(
            ctx,
            arg,
            &params,
            &mut param_index,
            &mut effects,
            &mut has_runtime_spread,
        );
    }

    if !has_runtime_spread {
        if param_index != params.len() {
            ctx.diag(origin.span, DiagKind::CallArityMismatch, "");
        }
    } else if param_index > params.len() {
        ctx.diag(origin.span, DiagKind::CallArityMismatch, "");
    }

    merge_call_effects(ctx, origin, callee, &mut effects);
    ExprFacts { ty: ret, effects }
}

pub(super) fn check_apply_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    origin: HirOrigin,
    callee: HirExprId,
    args: SliceRange<HirExprId>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let callee_facts = check_expr(ctx, callee);
    let effectful_eval = callee_facts.effects;
    let args = ctx
        .expr_ids(args)
        .into_iter()
        .map(|arg| {
            let arg_origin = ctx.expr(arg).origin;
            lower_type_expr(ctx, arg, arg_origin)
        })
        .collect::<Vec<_>>();
    let Some(scheme) = callable_scheme_for_expr(ctx, callee) else {
        ctx.diag(origin.span, DiagKind::InvalidTypeApplication, "");
        return ExprFacts {
            ty: builtins.unknown,
            effects: effectful_eval,
        };
    };
    let Some(instantiated) = instantiate_binding_scheme(ctx, origin, &scheme, &args) else {
        return ExprFacts {
            ty: builtins.unknown,
            effects: effectful_eval,
        };
    };
    if let Some(target) = module_target_for_expr(ctx, callee) {
        ctx.set_expr_module_target(expr_id, target);
    }
    solve_obligations(ctx, origin, &instantiated.obligations);
    ctx.set_expr_callable_effects(expr_id, instantiated.effects.clone());
    ExprFacts {
        ty: instantiated.ty,
        effects: effectful_eval,
    }
}

fn check_call_arg(
    ctx: &mut CheckPass<'_, '_, '_>,
    arg: &HirArg,
    params: &[HirTyId],
    param_index: &mut usize,
    effects: &mut EffectRow,
    has_runtime_spread: &mut bool,
) {
    let builtins = ctx.builtins();
    if !arg.spread {
        let expected = params
            .get(*param_index)
            .copied()
            .unwrap_or(builtins.unknown);
        ctx.push_expected_ty(expected);
        let facts = check_expr(ctx, arg.expr);
        let _ = ctx.pop_expected_ty();
        effects.union_with(&facts.effects);
        let arg_origin = ctx.expr(arg.expr).origin;
        type_mismatch(ctx, arg_origin, expected, facts.ty);
        *param_index = param_index.saturating_add(1);
        return;
    }

    let facts = check_expr(ctx, arg.expr);
    effects.union_with(&facts.effects);
    let spread_origin = ctx.expr(arg.expr).origin;
    let spread_ty = peel_mut_ty(ctx, facts.ty);
    check_call_spread_arg(
        ctx,
        spread_origin,
        spread_ty,
        params,
        param_index,
        has_runtime_spread,
    );
}

fn check_call_spread_arg(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    spread_ty: HirTyId,
    params: &[HirTyId],
    param_index: &mut usize,
    has_runtime_spread: &mut bool,
) {
    let builtins = ctx.builtins();
    match ctx.ty(spread_ty).kind {
        HirTyKind::Tuple { items } => {
            let item_tys = ctx.ty_ids(items);
            for found in item_tys {
                let expected = params
                    .get(*param_index)
                    .copied()
                    .unwrap_or(builtins.unknown);
                type_mismatch(ctx, origin, expected, found);
                *param_index = param_index.saturating_add(1);
            }
        }
        HirTyKind::Array { dims, item } => check_call_array_spread(
            ctx,
            origin,
            dims,
            item,
            params,
            param_index,
            has_runtime_spread,
        ),
        _ => ctx.diag(origin.span, DiagKind::InvalidCallSpreadSource, ""),
    }
}

fn check_call_array_spread(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    dims: SliceRange<HirDim>,
    item: HirTyId,
    params: &[HirTyId],
    param_index: &mut usize,
    has_runtime_spread: &mut bool,
) {
    let builtins = ctx.builtins();
    let dims_vec = ctx.dims(dims);
    if dims_vec.is_empty() {
        if ctx.ty(item).kind == HirTyKind::Any {
            *has_runtime_spread = true;
        } else {
            ctx.diag(origin.span, DiagKind::CallRuntimeSpreadRequiresArrayAny, "");
        }
        return;
    }
    if dims_vec.len() != 1 {
        ctx.diag(origin.span, DiagKind::CallSpreadRequiresTupleOrArray, "");
        return;
    }
    match dims_vec[0] {
        HirDim::Int(len) => {
            for _ in 0..len {
                let expected = params
                    .get(*param_index)
                    .copied()
                    .unwrap_or(builtins.unknown);
                type_mismatch(ctx, origin, expected, item);
                *param_index = param_index.saturating_add(1);
            }
        }
        HirDim::Unknown | HirDim::Name(_) if ctx.ty(item).kind == HirTyKind::Any => {
            *has_runtime_spread = true;
        }
        _ => ctx.diag(origin.span, DiagKind::CallRuntimeSpreadRequiresArrayAny, ""),
    }
}

fn merge_call_effects(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    callee: HirExprId,
    effects: &mut EffectRow,
) {
    if let Some(extra) = call_effects_for_expr(ctx, callee) {
        effects.union_with(&extra);
        return;
    }
    let Some(scheme) = callable_scheme_for_expr(ctx, callee) else {
        return;
    };
    if !scheme.type_params.is_empty() {
        return;
    }
    let instantiated = instantiate_monomorphic_scheme(ctx, &scheme);
    solve_obligations(ctx, origin, &instantiated.obligations);
    effects.union_with(&instantiated.effects);
}

fn callable_scheme_for_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr: HirExprId,
) -> Option<BindingScheme> {
    match ctx.expr(expr).kind {
        HirExprKind::Name { name } => ctx
            .binding_id_for_use(name)
            .and_then(|binding| ctx.binding_scheme(binding).cloned()),
        HirExprKind::Field { base, name, .. } => module_export_for_expr(ctx, base, name)
            .map(|(surface, export)| scheme_from_export(ctx, &surface, &export)),
        _ => None,
    }
}
