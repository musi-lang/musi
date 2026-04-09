use std::collections::BTreeMap;

use music_arena::SliceRange;
use music_hir::{
    HirAccessKind, HirBinaryOp, HirBinder, HirCaseArm, HirConstraint, HirExprId, HirExprKind,
    HirLitId, HirLitKind, HirMemberDef, HirOrigin, HirParam, HirPrefixOp, HirQuoteKind,
    HirRecordItem, HirSpliceKind, HirTemplatePart, HirTyField, HirTyId, HirTyKind,
};
use music_names::Ident;

use crate::api::ExprFacts;

use super::CheckPass;
use super::attrs::validate_expr_attrs;
use super::decls::{
    LetExprInput, check_handle_expr, check_import_expr, check_instance_expr, check_let_expr,
    check_perform_expr, check_resume_expr, module_export_for_expr,
};
use super::expr_aggregates::{
    check_array_expr, check_array_ty_expr, check_record_expr, check_variant_expr,
};
use super::expr_calls::{check_apply_expr, check_call_expr};
use super::normalize::{lower_params, lower_type_expr, symbol_value_type, type_mismatch};
use super::patterns::bind_pat;
use super::schemes::{instantiate_monomorphic_scheme, scheme_from_export, solve_obligations};
use crate::effects::EffectRow;

pub fn check_module_root(ctx: &mut CheckPass<'_, '_, '_>, id: HirExprId) -> ExprFacts {
    check_module_stmt(ctx, id)
}

pub fn check_expr(ctx: &mut CheckPass<'_, '_, '_>, id: HirExprId) -> ExprFacts {
    let expr = ctx.expr(id);
    let origin = expr.origin;
    let attrs = expr.mods.attrs;
    if !attrs.is_empty() {
        validate_expr_attrs(ctx, origin, attrs, id);
    }
    let facts = check_expr_kind(ctx, id);
    ctx.set_expr_facts(id, facts.clone());
    facts
}

fn check_expr_kind(ctx: &mut CheckPass<'_, '_, '_>, id: HirExprId) -> ExprFacts {
    let builtins = ctx.builtins();
    let expr = ctx.expr(id);
    let kind = expr.kind.clone();
    match kind {
        HirExprKind::Error => ExprFacts {
            ty: builtins.error,
            effects: EffectRow::empty(),
        },
        HirExprKind::Name { name } => check_name_expr(ctx, id, name),
        HirExprKind::Lit { lit } => check_lit_expr(ctx, lit),
        HirExprKind::Template { parts } => check_template_expr(ctx, parts),
        HirExprKind::Sequence { exprs } => check_sequence_expr(ctx, exprs),
        HirExprKind::Tuple { items } => check_tuple_expr(ctx, items),
        HirExprKind::Array { items } => check_array_expr(ctx, items),
        HirExprKind::ArrayTy { dims, item } => check_array_ty_expr(ctx, dims, item),
        HirExprKind::Record { items } => check_record_expr(ctx, items),
        HirExprKind::Variant { tag, args } => check_variant_expr(ctx, tag, args),
        HirExprKind::Pi {
            binder,
            binder_ty,
            ret,
            is_effectful,
        } => check_pi_expr(ctx, binder, binder_ty, ret, is_effectful),
        HirExprKind::Lambda {
            params,
            ret_ty,
            body,
        } => check_lambda_expr(ctx, params, ret_ty, body),
        HirExprKind::Call { callee, args } => check_call_expr(ctx, expr.origin, callee, args),
        HirExprKind::Apply { callee, args } => check_apply_expr(ctx, id, expr.origin, callee, args),
        HirExprKind::Index { base, args } => check_index_expr(ctx, expr.origin, base, args),
        HirExprKind::Field { base, access, name } => {
            check_field_expr(ctx, id, expr.origin, base, access, name)
        }
        HirExprKind::RecordUpdate { base, items } => {
            check_record_update_expr(ctx, expr.origin, base, items)
        }
        HirExprKind::TypeTest { base, ty, as_name } => {
            check_type_test_expr(ctx, id, base, ty, as_name)
        }
        HirExprKind::TypeCast { base, ty } => check_type_cast_expr(ctx, base, ty),
        HirExprKind::Prefix { op, expr: inner } => check_prefix_expr(ctx, expr.origin, &op, inner),
        HirExprKind::Binary { op, left, right } => {
            check_binary_expr(ctx, expr.origin, &op, left, right)
        }
        HirExprKind::Let {
            mods,
            pat,
            type_params,
            has_param_clause,
            params,
            constraints,
            effects,
            sig,
            value,
        } => check_let_kind(
            ctx,
            LetExprInput {
                expr_id: id,
                origin: expr.origin,
                expr_mods: expr.mods,
                mods,
                pat,
                type_params,
                has_param_clause,
                params,
                constraints,
                effects,
                sig,
                value,
            },
        ),
        other => check_decl_expr(ctx, id, expr.origin, other),
    }
}

fn check_decl_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    id: HirExprId,
    origin: HirOrigin,
    kind: HirExprKind,
) -> ExprFacts {
    let builtins = ctx.builtins();
    match kind {
        HirExprKind::Error => ExprFacts {
            ty: builtins.error,
            effects: EffectRow::empty(),
        },
        HirExprKind::Name { name } => check_name_expr(ctx, id, name),
        HirExprKind::Lit { lit } => check_lit_expr(ctx, lit),
        HirExprKind::Template { parts } => check_template_expr(ctx, parts),
        HirExprKind::Sequence { exprs } => check_sequence_expr(ctx, exprs),
        HirExprKind::Tuple { items } => check_tuple_expr(ctx, items),
        HirExprKind::Array { items } => check_array_expr(ctx, items),
        HirExprKind::ArrayTy { dims, item } => check_array_ty_expr(ctx, dims, item),
        HirExprKind::Record { items } => check_record_expr(ctx, items),
        HirExprKind::Variant { tag, args } => check_variant_expr(ctx, tag, args),
        HirExprKind::Pi {
            binder,
            binder_ty,
            ret,
            is_effectful,
        } => check_pi_expr(ctx, binder, binder_ty, ret, is_effectful),
        HirExprKind::Lambda {
            params,
            ret_ty,
            body,
        } => check_lambda_expr(ctx, params, ret_ty, body),
        HirExprKind::Call { callee, args } => check_call_expr(ctx, origin, callee, args),
        HirExprKind::Apply { callee, args } => check_apply_expr(ctx, id, origin, callee, args),
        HirExprKind::Index { base, args } => check_index_expr(ctx, origin, base, args),
        HirExprKind::Field { base, access, name } => {
            check_field_expr(ctx, id, origin, base, access, name)
        }
        HirExprKind::RecordUpdate { base, items } => {
            check_record_update_expr(ctx, origin, base, items)
        }
        HirExprKind::TypeTest { base, ty, as_name } => {
            check_type_test_expr(ctx, id, base, ty, as_name)
        }
        HirExprKind::TypeCast { base, ty } => check_type_cast_expr(ctx, base, ty),
        HirExprKind::Prefix { op, expr: inner } => check_prefix_expr(ctx, origin, &op, inner),
        HirExprKind::Binary { op, left, right } => check_binary_expr(ctx, origin, &op, left, right),
        HirExprKind::Let {
            mods,
            pat,
            type_params,
            has_param_clause,
            params,
            constraints,
            effects,
            sig,
            value,
        } => check_let_kind(
            ctx,
            LetExprInput {
                expr_id: id,
                origin,
                expr_mods: ctx.expr(id).mods,
                mods,
                pat,
                type_params,
                has_param_clause,
                params,
                constraints,
                effects,
                sig,
                value,
            },
        ),
        HirExprKind::Import { arg } => check_import_expr(ctx, id, arg),
        HirExprKind::Case { scrutinee, arms } => check_case_expr(ctx, scrutinee, arms),
        HirExprKind::Data { .. }
        | HirExprKind::Effect { .. }
        | HirExprKind::Class { .. }
        | HirExprKind::Instance { .. } => {
            ctx.diag(origin.span, "declaration form used as value", "");
            ExprFacts {
                ty: builtins.unknown,
                effects: EffectRow::empty(),
            }
        }
        HirExprKind::Perform { expr: inner } => check_perform_expr(ctx, origin, inner),
        HirExprKind::Handle {
            expr: inner,
            handler,
            clauses,
        } => check_handle_expr(ctx, origin, inner, handler, clauses),
        HirExprKind::Resume { expr: inner } => check_resume_expr(ctx, origin, inner),
        HirExprKind::Quote { kind } => check_quote_expr(ctx, kind),
        HirExprKind::Splice { kind } => check_splice_expr(ctx, kind),
    }
}

fn check_module_stmt(ctx: &mut CheckPass<'_, '_, '_>, id: HirExprId) -> ExprFacts {
    ctx.enter_module_stmt();
    let expr = ctx.expr(id);
    let origin = expr.origin;
    let facts = match expr.kind {
        HirExprKind::Sequence { exprs } => {
            let mut ty = ctx.builtins().unit;
            let mut effects = EffectRow::empty();
            for expr_id in ctx.expr_ids(exprs) {
                let facts = check_module_stmt(ctx, expr_id);
                ty = facts.ty;
                effects.union_with(&facts.effects);
            }
            ExprFacts { ty, effects }
        }
        HirExprKind::Instance {
            type_params,
            constraints,
            class,
            members,
        } => {
            let _ = check_instance_kind(ctx, id, origin, type_params, constraints, class, &members);
            ExprFacts {
                ty: ctx.builtins().unit,
                effects: EffectRow::empty(),
            }
        }
        _ => check_expr(ctx, id),
    };
    ctx.set_expr_facts(id, facts.clone());
    ctx.exit_module_stmt();
    facts
}

fn check_let_kind(ctx: &mut CheckPass<'_, '_, '_>, input: LetExprInput) -> ExprFacts {
    check_let_expr(ctx, input)
}

fn check_instance_kind(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    origin: HirOrigin,
    type_params: SliceRange<HirBinder>,
    constraints: SliceRange<HirConstraint>,
    class: HirExprId,
    members: &SliceRange<HirMemberDef>,
) -> ExprFacts {
    check_instance_expr(
        ctx,
        expr_id,
        origin,
        type_params,
        constraints,
        class,
        members,
    )
}

fn check_name_expr(ctx: &mut CheckPass<'_, '_, '_>, expr_id: HirExprId, name: Ident) -> ExprFacts {
    let builtins = ctx.builtins();
    if let Some(binding) = ctx.binding_id_for_use(name)
        && ctx.is_gated_binding(binding)
    {
        ctx.diag(name.span, "unavailable on this target", "");
        return ExprFacts {
            ty: builtins.unknown,
            effects: EffectRow::empty(),
        };
    }
    if let Some(binding) = ctx.binding_id_for_use(name)
        && let Some(target) = ctx.binding_module_target(binding).cloned()
    {
        ctx.set_expr_module_target(expr_id, target);
    }
    if let Some(binding) = ctx.binding_id_for_use(name)
        && let Some(scheme) = ctx.binding_scheme(binding).cloned()
        && scheme.type_params.is_empty()
    {
        let instantiated = instantiate_monomorphic_scheme(ctx, &scheme);
        solve_obligations(ctx, ctx.expr(expr_id).origin, &instantiated.obligations);
        return ExprFacts {
            ty: instantiated.ty,
            effects: EffectRow::empty(),
        };
    }
    let ty = ctx
        .binding_id_for_use(name)
        .and_then(|binding| ctx.binding_type(binding))
        .unwrap_or_else(|| symbol_value_type(ctx, name.name));
    ExprFacts {
        ty,
        effects: EffectRow::empty(),
    }
}

fn check_lit_expr(ctx: &CheckPass<'_, '_, '_>, lit: HirLitId) -> ExprFacts {
    let builtins = ctx.builtins();
    let ty = match ctx.lit_kind(lit) {
        HirLitKind::Int { .. } | HirLitKind::Rune { .. } => builtins.int_,
        HirLitKind::Float { .. } => builtins.float_,
        HirLitKind::String { .. } => builtins.string_,
    };
    ExprFacts {
        ty,
        effects: EffectRow::empty(),
    }
}

fn check_template_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    parts: SliceRange<HirTemplatePart>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let mut effects = EffectRow::empty();
    for part in ctx.template_parts(parts) {
        if let HirTemplatePart::Expr { expr } = part {
            let facts = check_expr(ctx, expr);
            let origin = ctx.expr(expr).origin;
            type_mismatch(ctx, origin, builtins.string_, facts.ty);
            effects.union_with(&facts.effects);
        }
    }
    ExprFacts {
        ty: builtins.string_,
        effects,
    }
}

fn check_sequence_expr(ctx: &mut CheckPass<'_, '_, '_>, exprs: SliceRange<HirExprId>) -> ExprFacts {
    let builtins = ctx.builtins();
    let mut effects = EffectRow::empty();
    let mut ty = builtins.unit;
    let exprs = ctx.expr_ids(exprs);
    let len = exprs.len();
    let expected = ctx.expected_ty();
    for (idx, expr) in exprs.into_iter().enumerate() {
        let suppress_expected = expected.is_some() && idx + 1 != len;
        let saved_expected = suppress_expected.then(|| ctx.pop_expected_ty()).flatten();
        let facts = check_expr(ctx, expr);
        if let Some(saved) = saved_expected {
            ctx.push_expected_ty(saved);
        }
        effects.union_with(&facts.effects);
        ty = facts.ty;
    }
    ExprFacts { ty, effects }
}

fn check_tuple_expr(ctx: &mut CheckPass<'_, '_, '_>, items: SliceRange<HirExprId>) -> ExprFacts {
    let mut effects = EffectRow::empty();
    let item_types = ctx
        .expr_ids(items)
        .into_iter()
        .map(|expr| {
            let facts = check_expr(ctx, expr);
            effects.union_with(&facts.effects);
            facts.ty
        })
        .collect::<Vec<_>>();
    let items = ctx.alloc_ty_list(item_types);
    let ty = ctx.alloc_ty(HirTyKind::Tuple { items });
    ExprFacts { ty, effects }
}

fn check_pi_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    binder: Ident,
    binder_ty: HirExprId,
    ret: HirExprId,
    is_effectful: bool,
) -> ExprFacts {
    let binder_origin = ctx.expr(binder_ty).origin;
    let param_ty = lower_type_expr(ctx, binder_ty, binder_origin);
    if let Some(binding) = ctx.binding_id_for_decl(binder) {
        ctx.insert_binding_type(binding, param_ty);
    }
    let ret_origin = ctx.expr(ret).origin;
    let ret_ty = lower_type_expr(ctx, ret, ret_origin);
    let params = ctx.alloc_ty_list([param_ty]);
    let ty = ctx.alloc_ty(HirTyKind::Arrow {
        params,
        ret: ret_ty,
        is_effectful,
    });
    ExprFacts {
        ty,
        effects: EffectRow::empty(),
    }
}

fn check_lambda_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    params: SliceRange<HirParam>,
    ret_ty: Option<HirExprId>,
    body: HirExprId,
) -> ExprFacts {
    let param_types = lower_params(ctx, params);
    let declared_ret = ret_ty.map(|ret| {
        let origin = ctx.expr(ret).origin;
        lower_type_expr(ctx, ret, origin)
    });
    if let Some(expected) = declared_ret {
        ctx.push_expected_ty(expected);
    }
    let body_facts = check_expr(ctx, body);
    if declared_ret.is_some() {
        let _ = ctx.pop_expected_ty();
    }
    let result_ty = declared_ret.unwrap_or(body_facts.ty);
    if let Some(ret) = ret_ty {
        let origin = ctx.expr(ret).origin;
        type_mismatch(ctx, origin, result_ty, body_facts.ty);
    }
    let params = ctx.alloc_ty_list(param_types.iter().copied());
    let ty = ctx.alloc_ty(HirTyKind::Arrow {
        params,
        ret: result_ty,
        is_effectful: !body_facts.effects.is_pure(),
    });
    ExprFacts {
        ty,
        effects: EffectRow::empty(),
    }
}

fn check_index_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    base: HirExprId,
    args: SliceRange<HirExprId>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let base_facts = check_expr(ctx, base);
    let mut effects = base_facts.effects.clone();
    let arg_count = check_index_args(ctx, args, &mut effects);
    let ty = if let HirTyKind::Array { dims, item } = ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
        let dims = ctx.dims(dims);
        if !dims.is_empty() && dims.len() != arg_count {
            ctx.diag(origin.span, "invalid index arity", "");
        }
        item
    } else {
        ctx.diag(origin.span, "invalid index target", "");
        builtins.unknown
    };
    ExprFacts { ty, effects }
}

fn check_index_args(
    ctx: &mut CheckPass<'_, '_, '_>,
    args: SliceRange<HirExprId>,
    effects: &mut EffectRow,
) -> usize {
    let builtins = ctx.builtins();
    let index_exprs = ctx.expr_ids(args);
    for index_expr in &index_exprs {
        let facts = check_expr(ctx, *index_expr);
        effects.union_with(&facts.effects);
        let index_origin = ctx.expr(*index_expr).origin;
        type_mismatch(ctx, index_origin, builtins.int_, facts.ty);
    }
    index_exprs.len()
}

fn check_field_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    origin: HirOrigin,
    base: HirExprId,
    access: HirAccessKind,
    name: Ident,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let base_facts = check_expr(ctx, base);
    let effects = base_facts.effects.clone();

    if let HirExprKind::Name { name: effect_name } = ctx.expr(base).kind {
        let effect_name = ctx.resolve_symbol(effect_name.name);
        let op_name = ctx.resolve_symbol(name.name);
        if let Some(effect) = ctx.effect_def(effect_name) {
            if let Some(op) = effect.ops.get(op_name).cloned() {
                let params = ctx.alloc_ty_list(op.params.iter().copied());
                let ty = ctx.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret: op.result,
                    is_effectful: true,
                });
                return ExprFacts { ty, effects };
            }
        }
    }

    let ty = match ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
        HirTyKind::Record { fields } => ctx
            .ty_fields(fields)
            .into_iter()
            .find(|field| field.name == name.name)
            .map_or_else(
                || {
                    ctx.diag(origin.span, "unknown field", "");
                    builtins.unknown
                },
                |field| field.ty,
            ),
        HirTyKind::Module => {
            if let Some((surface, export)) = module_export_for_expr(ctx, base, name) {
                if let Some(target) = export.module_target.clone() {
                    ctx.set_expr_module_target(expr_id, target);
                }
                let scheme = scheme_from_export(ctx, &surface, &export);
                ctx.set_expr_callable_effects(expr_id, scheme.effects.clone());
                scheme.ty
            } else {
                ctx.diag(origin.span, "unknown export", "");
                builtins.unknown
            }
        }
        _ => {
            if !matches!(access, HirAccessKind::Direct) {
                ctx.diag(origin.span, "invalid optional field access", "");
            }
            builtins.unknown
        }
    };
    ExprFacts { ty, effects }
}

fn check_record_update_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    base: HirExprId,
    items: SliceRange<HirRecordItem>,
) -> ExprFacts {
    let base_facts = check_expr(ctx, base);
    let mut effects = base_facts.effects.clone();
    let mut fields =
        if let HirTyKind::Record { fields } = ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
            ctx.ty_fields(fields)
                .into_iter()
                .map(|field| (field.name, field.ty))
                .collect::<BTreeMap<_, _>>()
        } else {
            ctx.diag(origin.span, "invalid record update target", "");
            BTreeMap::new()
        };
    for record_item in ctx.record_items(items) {
        if record_item.spread {
            let facts = check_expr(ctx, record_item.value);
            effects.union_with(&facts.effects);
            let spread_origin = ctx.expr(record_item.value).origin;
            let spread_ty = peel_mut_ty(ctx, facts.ty);
            let HirTyKind::Record {
                fields: spread_fields,
            } = ctx.ty(spread_ty).kind
            else {
                ctx.diag(spread_origin.span, "invalid record spread source", "");
                continue;
            };
            for spread_field in ctx.ty_fields(spread_fields) {
                let _prev = fields.insert(spread_field.name, spread_field.ty);
            }
            continue;
        }

        let expected = record_item
            .name
            .and_then(|name| fields.get(&name.name).copied())
            .unwrap_or_else(|| ctx.builtins().unknown);
        ctx.push_expected_ty(expected);
        let facts = check_expr(ctx, record_item.value);
        let _ = ctx.pop_expected_ty();
        effects.union_with(&facts.effects);
        if let Some(name) = record_item.name {
            let _prev = fields.insert(name.name, facts.ty);
        }
    }
    let fields = ctx.alloc_ty_fields(fields.into_iter().map(|(name, ty)| HirTyField { name, ty }));
    let ty = ctx.alloc_ty(HirTyKind::Record { fields });
    ExprFacts { ty, effects }
}

fn check_type_test_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    base: HirExprId,
    ty_expr: HirExprId,
    as_name: Option<Ident>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let base_facts = check_expr(ctx, base);
    let origin = ctx.expr(ty_expr).origin;
    let target = lower_type_expr(ctx, ty_expr, origin);
    if contains_mut_ty(ctx, target) {
        ctx.diag(origin.span, "`mut` not allowed in type test target", "");
    }
    ctx.set_type_test_target(expr_id, target);
    if let Some(binding) = as_name.and_then(|ident| ctx.binding_id_for_decl(ident)) {
        ctx.insert_binding_type(binding, base_facts.ty);
    }
    ExprFacts {
        ty: builtins.bool_,
        effects: base_facts.effects,
    }
}

fn check_type_cast_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    base: HirExprId,
    ty_expr: HirExprId,
) -> ExprFacts {
    let base_facts = check_expr(ctx, base);
    let origin = ctx.expr(ty_expr).origin;
    let ty = lower_type_expr(ctx, ty_expr, origin);
    if contains_mut_ty(ctx, ty) {
        ctx.diag(origin.span, "`mut` not allowed in type cast target", "");
    }
    ExprFacts {
        ty,
        effects: base_facts.effects,
    }
}

fn check_prefix_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    op: &HirPrefixOp,
    inner: HirExprId,
) -> ExprFacts {
    let inner_facts = check_expr(ctx, inner);
    let ty = match op {
        HirPrefixOp::Neg => numeric_unary_type(ctx, origin, inner_facts.ty),
        HirPrefixOp::Not => {
            let bool_ty = ctx.builtins().bool_;
            type_mismatch(ctx, origin, bool_ty, inner_facts.ty);
            bool_ty
        }
        HirPrefixOp::Mut => ctx.alloc_ty(HirTyKind::Mut {
            inner: inner_facts.ty,
        }),
    };
    ExprFacts {
        ty,
        effects: inner_facts.effects,
    }
}

fn check_binary_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
) -> ExprFacts {
    if matches!(op, HirBinaryOp::Assign) {
        return check_assign_expr(ctx, origin, left, right);
    }
    let builtins = ctx.builtins();
    let left_facts = check_expr(ctx, left);
    let right_facts = check_expr(ctx, right);
    let mut effects = left_facts.effects.clone();
    effects.union_with(&right_facts.effects);
    let ty = match op {
        HirBinaryOp::Assign => builtins.unit,
        HirBinaryOp::Arrow | HirBinaryOp::EffectArrow => {
            let left_origin = ctx.expr(left).origin;
            let left_ty = lower_type_expr(ctx, left, left_origin);
            let params = ctx.alloc_ty_list([left_ty]);
            let right_origin = ctx.expr(right).origin;
            let ret = lower_type_expr(ctx, right, right_origin);
            ctx.alloc_ty(HirTyKind::Arrow {
                params,
                ret,
                is_effectful: matches!(op, HirBinaryOp::EffectArrow),
            })
        }
        HirBinaryOp::Add
            if matches!(ctx.ty(left_facts.ty).kind, HirTyKind::Type)
                || matches!(ctx.ty(right_facts.ty).kind, HirTyKind::Type) =>
        {
            let left_origin = ctx.expr(left).origin;
            let right_origin = ctx.expr(right).origin;
            let left_ty = lower_type_expr(ctx, left, left_origin);
            let right_ty = lower_type_expr(ctx, right, right_origin);
            ctx.alloc_ty(HirTyKind::Sum {
                left: left_ty,
                right: right_ty,
            })
        }
        HirBinaryOp::Add
            if matches!(ctx.ty(left_facts.ty).kind, HirTyKind::String)
                || matches!(ctx.ty(right_facts.ty).kind, HirTyKind::String) =>
        {
            type_mismatch(ctx, origin, builtins.string_, left_facts.ty);
            type_mismatch(ctx, origin, builtins.string_, right_facts.ty);
            builtins.string_
        }
        HirBinaryOp::Add
        | HirBinaryOp::Sub
        | HirBinaryOp::Mul
        | HirBinaryOp::Div
        | HirBinaryOp::Rem
        | HirBinaryOp::Shl
        | HirBinaryOp::Shr => numeric_binary_type(ctx, origin, left_facts.ty, right_facts.ty),
        HirBinaryOp::Eq
        | HirBinaryOp::Ne
        | HirBinaryOp::Lt
        | HirBinaryOp::Gt
        | HirBinaryOp::Le
        | HirBinaryOp::Ge
        | HirBinaryOp::In => builtins.bool_,
        HirBinaryOp::And | HirBinaryOp::Or | HirBinaryOp::Xor => {
            type_mismatch(ctx, origin, builtins.bool_, left_facts.ty);
            type_mismatch(ctx, origin, builtins.bool_, right_facts.ty);
            builtins.bool_
        }
        HirBinaryOp::Pipe | HirBinaryOp::UserOp(_) => builtins.unknown,
    };
    ExprFacts { ty, effects }
}

fn check_assign_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    left: HirExprId,
    right: HirExprId,
) -> ExprFacts {
    let builtins = ctx.builtins();

    let (expected_rhs, mut effects) = match ctx.expr(left).kind {
        HirExprKind::Name { name } => {
            let binding = ctx.binding_id_for_use(name);
            let ty = binding
                .and_then(|binding| ctx.binding_type(binding))
                .unwrap_or_else(|| symbol_value_type(ctx, name.name));
            if is_mut_ty(ctx, ty) {
                (peel_mut_ty(ctx, ty), EffectRow::empty())
            } else {
                ctx.diag(origin.span, "write requires `mut T`", "");
                (builtins.unknown, EffectRow::empty())
            }
        }
        HirExprKind::Index { base, args } => {
            let base_facts = check_expr(ctx, base);
            let mut effects = base_facts.effects;
            let arg_count = check_index_args(ctx, args, &mut effects);

            let expected = match ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
                HirTyKind::Array { dims, item } if is_mut_ty(ctx, base_facts.ty) => {
                    let dims = ctx.dims(dims);
                    if !dims.is_empty() && dims.len() != arg_count {
                        ctx.diag(origin.span, "invalid index arity", "");
                    }
                    item
                }
                HirTyKind::Array { .. } => {
                    ctx.diag(origin.span, "write requires `mut []T`", "");
                    builtins.unknown
                }
                _ => {
                    ctx.diag(origin.span, "invalid index target", "");
                    builtins.unknown
                }
            };
            (expected, effects)
        }
        HirExprKind::Field { base, name, .. } => {
            let base_facts = check_expr(ctx, base);
            let effects = base_facts.effects;

            let expected = match ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
                HirTyKind::Record { fields } if is_mut_ty(ctx, base_facts.ty) => ctx
                    .ty_fields(fields)
                    .into_iter()
                    .find(|field| field.name == name.name)
                    .map_or_else(
                        || {
                            ctx.diag(origin.span, "unknown field", "");
                            builtins.unknown
                        },
                        |field| field.ty,
                    ),
                HirTyKind::Record { .. } => {
                    ctx.diag(origin.span, "write requires `mut { ... }`", "");
                    builtins.unknown
                }
                _ => {
                    ctx.diag(origin.span, "invalid field update target", "");
                    builtins.unknown
                }
            };
            (expected, effects)
        }
        _ => {
            ctx.diag(origin.span, "unsupported assignment target", "");
            (builtins.unknown, EffectRow::empty())
        }
    };

    ctx.push_expected_ty(expected_rhs);
    let rhs_facts = check_expr(ctx, right);
    let _ = ctx.pop_expected_ty();
    effects.union_with(&rhs_facts.effects);
    type_mismatch(ctx, origin, expected_rhs, rhs_facts.ty);

    ExprFacts {
        ty: builtins.unit,
        effects,
    }
}

fn check_case_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    scrutinee: HirExprId,
    arms: SliceRange<HirCaseArm>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let scrutinee_facts = check_expr(ctx, scrutinee);
    let mut effects = scrutinee_facts.effects.clone();
    let mut result_ty = builtins.unknown;
    for arm in ctx.case_arms(arms) {
        bind_pat(ctx, arm.pat, scrutinee_facts.ty);
        if let Some(guard) = arm.guard {
            let guard_facts = check_expr(ctx, guard);
            let origin = ctx.expr(guard).origin;
            type_mismatch(ctx, origin, builtins.bool_, guard_facts.ty);
            effects.union_with(&guard_facts.effects);
        }
        let arm_facts = check_expr(ctx, arm.expr);
        effects.union_with(&arm_facts.effects);
        if result_ty == builtins.unknown {
            result_ty = arm_facts.ty;
        } else {
            let origin = ctx.expr(arm.expr).origin;
            type_mismatch(ctx, origin, result_ty, arm_facts.ty);
        }
    }
    ExprFacts {
        ty: result_ty,
        effects,
    }
}

fn check_quote_expr(ctx: &CheckPass<'_, '_, '_>, _kind: HirQuoteKind) -> ExprFacts {
    ExprFacts {
        ty: ctx.builtins().syntax,
        effects: EffectRow::empty(),
    }
}

fn check_splice_expr(ctx: &CheckPass<'_, '_, '_>, _kind: HirSpliceKind) -> ExprFacts {
    ExprFacts {
        ty: ctx.builtins().syntax,
        effects: EffectRow::empty(),
    }
}

pub(super) fn peel_mut_ty(ctx: &CheckPass<'_, '_, '_>, mut ty: HirTyId) -> HirTyId {
    while let HirTyKind::Mut { inner } = ctx.ty(ty).kind {
        ty = inner;
    }
    ty
}

fn contains_mut_ty(ctx: &CheckPass<'_, '_, '_>, ty: HirTyId) -> bool {
    match &ctx.ty(ty).kind {
        HirTyKind::Mut { .. } => true,
        HirTyKind::Named { args, .. } => ctx
            .ty_ids(*args)
            .into_iter()
            .any(|ty| contains_mut_ty(ctx, ty)),
        HirTyKind::Pi {
            binder_ty, body, ..
        } => contains_mut_ty(ctx, *binder_ty) || contains_mut_ty(ctx, *body),
        HirTyKind::Arrow { params, ret, .. } => {
            ctx.ty_ids(*params)
                .into_iter()
                .any(|ty| contains_mut_ty(ctx, ty))
                || contains_mut_ty(ctx, *ret)
        }
        HirTyKind::Sum { left, right } => {
            contains_mut_ty(ctx, *left) || contains_mut_ty(ctx, *right)
        }
        HirTyKind::Tuple { items } => ctx
            .ty_ids(*items)
            .into_iter()
            .any(|ty| contains_mut_ty(ctx, ty)),
        HirTyKind::Array { item, .. } => contains_mut_ty(ctx, *item),
        HirTyKind::Record { fields } => ctx
            .ty_fields(fields.clone())
            .into_iter()
            .any(|field| contains_mut_ty(ctx, field.ty)),
        HirTyKind::Error
        | HirTyKind::Unknown
        | HirTyKind::Type
        | HirTyKind::Syntax
        | HirTyKind::Any
        | HirTyKind::Empty
        | HirTyKind::Unit
        | HirTyKind::Bool
        | HirTyKind::Nat
        | HirTyKind::Int
        | HirTyKind::Float
        | HirTyKind::String
        | HirTyKind::CString
        | HirTyKind::CPtr
        | HirTyKind::Module
        | HirTyKind::NatLit(_) => false,
    }
}

fn is_mut_ty(ctx: &CheckPass<'_, '_, '_>, ty: HirTyId) -> bool {
    matches!(ctx.ty(ty).kind, HirTyKind::Mut { .. })
}

fn numeric_unary_type(ctx: &mut CheckPass<'_, '_, '_>, origin: HirOrigin, ty: HirTyId) -> HirTyId {
    let builtins = ctx.builtins();
    if ty == builtins.int_ || ty == builtins.float_ {
        ty
    } else {
        ctx.diag(origin.span, "numeric operand required", "");
        builtins.unknown
    }
}

fn numeric_binary_type(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    left: HirTyId,
    right: HirTyId,
) -> HirTyId {
    let builtins = ctx.builtins();
    if left == builtins.float_ || right == builtins.float_ {
        type_mismatch(ctx, origin, builtins.float_, left);
        type_mismatch(ctx, origin, builtins.float_, right);
        builtins.float_
    } else {
        type_mismatch(ctx, origin, builtins.int_, left);
        type_mismatch(ctx, origin, builtins.int_, right);
        builtins.int_
    }
}
