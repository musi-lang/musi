use std::collections::BTreeMap;

use music_arena::SliceRange;
use music_hir::{
    HirAccessKind, HirArg, HirArrayItem, HirBinaryOp, HirCaseArm, HirConstraint, HirDim, HirExprId,
    HirExprKind, HirLitId, HirLitKind, HirMemberDef, HirOrigin, HirParam, HirPrefixOp,
    HirQuoteKind, HirRecordItem, HirSpliceKind, HirTemplatePart, HirTyField, HirTyId, HirTyKind,
};
use music_names::Ident;

use crate::api::ExprFacts;

use super::CheckPass;
use super::decls::{
    LetExprInput, call_effects_for_expr, check_attributed_expr, check_class_expr, check_data_expr,
    check_effect_expr, check_foreign_expr, check_handle_expr, check_import_expr,
    check_instance_expr, check_let_expr, check_perform_expr, check_resume_expr,
    module_export_for_expr, module_target_for_expr,
};
use super::normalize::{lower_params, lower_type_expr, symbol_value_type, type_mismatch};
use super::patterns::bind_pat;
use super::schemes::{
    BindingScheme, instantiate_binding_scheme, instantiate_monomorphic_scheme, scheme_from_export,
    solve_obligations,
};
use crate::effects::EffectRow;

pub fn check_expr(ctx: &mut CheckPass<'_, '_, '_>, id: HirExprId) -> ExprFacts {
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
        HirExprKind::TypeTest { base, ty, as_name } => check_type_test_expr(ctx, base, ty, as_name),
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
                origin: expr.origin,
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
    match kind {
        HirExprKind::Error => ExprFacts {
            ty: ctx.builtins().error,
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
        HirExprKind::TypeTest { base, ty, as_name } => check_type_test_expr(ctx, base, ty, as_name),
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
            },
        ),
        HirExprKind::Import { arg } => check_import_expr(ctx, id, arg),
        HirExprKind::Export { expr: inner, .. } => check_export_expr(ctx, id, inner),
        HirExprKind::Case { scrutinee, arms } => check_case_expr(ctx, scrutinee, arms),
        HirExprKind::Data { variants, fields } => check_data_expr(ctx, variants, fields),
        HirExprKind::Effect { members } => check_effect_expr(ctx, members),
        HirExprKind::Class {
            constraints,
            members,
        } => check_class_expr(ctx, id, constraints, members, None),
        HirExprKind::Instance {
            type_params,
            constraints,
            class,
            members,
        } => check_instance_kind(ctx, id, origin, type_params, constraints, class, &members),
        HirExprKind::Foreign { abi, decls } => check_foreign_expr(ctx, abi, decls),
        HirExprKind::Perform { expr: inner } => check_perform_expr(ctx, origin, inner),
        HirExprKind::Handle {
            expr: inner,
            handler,
            clauses,
        } => check_handle_expr(ctx, origin, inner, handler, clauses),
        HirExprKind::Resume { expr: inner } => check_resume_expr(ctx, origin, inner),
        HirExprKind::Quote { kind } => check_quote_expr(ctx, kind),
        HirExprKind::Splice { kind } => check_splice_expr(ctx, kind),
        HirExprKind::Attributed { attrs, expr: inner } => {
            check_attributed_expr(ctx, origin, attrs, inner)
        }
    }
}

fn check_let_kind(ctx: &mut CheckPass<'_, '_, '_>, input: LetExprInput) -> ExprFacts {
    check_let_expr(ctx, input)
}

fn check_instance_kind(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    origin: HirOrigin,
    type_params: SliceRange<Ident>,
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

fn check_export_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    inner: HirExprId,
) -> ExprFacts {
    let facts = check_expr(ctx, inner);
    if let Some(target) = module_target_for_expr(ctx, inner) {
        ctx.set_expr_module_target(expr_id, target);
    }
    facts
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
    for expr in ctx.expr_ids(exprs) {
        let facts = check_expr(ctx, expr);
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

fn check_array_expr(ctx: &mut CheckPass<'_, '_, '_>, items: SliceRange<HirArrayItem>) -> ExprFacts {
    let builtins = ctx.builtins();
    let mut effects = EffectRow::empty();
    let mut item_ty = builtins.unknown;
    for item in ctx.array_items(items) {
        let facts = check_expr(ctx, item.expr);
        effects.union_with(&facts.effects);
        if item_ty == builtins.unknown {
            item_ty = facts.ty;
        } else {
            let origin = ctx.expr(item.expr).origin;
            type_mismatch(ctx, origin, item_ty, facts.ty);
        }
    }
    let dims = ctx.alloc_dims([HirDim::Unknown]);
    let ty = ctx.alloc_ty(HirTyKind::Array {
        dims,
        item: item_ty,
    });
    ExprFacts { ty, effects }
}

fn check_array_ty_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    dims: SliceRange<HirDim>,
    item: HirExprId,
) -> ExprFacts {
    let origin = ctx.expr(item).origin;
    let item_ty = lower_type_expr(ctx, item, origin);
    let ty = ctx.alloc_ty(HirTyKind::Array {
        dims,
        item: item_ty,
    });
    ExprFacts {
        ty,
        effects: EffectRow::empty(),
    }
}

fn check_record_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    items: SliceRange<HirRecordItem>,
) -> ExprFacts {
    let mut effects = EffectRow::empty();
    let mut fields = BTreeMap::<Box<str>, HirTyField>::new();
    for item in ctx.record_items(items) {
        let facts = check_expr(ctx, item.value);
        effects.union_with(&facts.effects);
        if item.spread {
            let span = ctx.expr(item.value).origin.span;
            ctx.diag(span, "record spread not supported", "");
            continue;
        }
        let Some(name) = item.name else {
            continue;
        };
        let key: Box<str> = ctx.resolve_symbol(name.name).into();
        let prev = fields.insert(
            key,
            HirTyField {
                name: name.name,
                ty: facts.ty,
            },
        );
        if prev.is_some() {
            let span = ctx.expr(item.value).origin.span;
            ctx.diag(span, "duplicate record field", "");
        }
    }
    let fields = ctx.alloc_ty_fields(fields.into_values());
    let ty = ctx.alloc_ty(HirTyKind::Record { fields });
    ExprFacts { ty, effects }
}

fn check_variant_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    _tag: Ident,
    args: SliceRange<HirExprId>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let mut effects = EffectRow::empty();
    for arg in ctx.expr_ids(args) {
        let facts = check_expr(ctx, arg);
        effects.union_with(&facts.effects);
    }
    ExprFacts {
        ty: builtins.unknown,
        effects,
    }
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
    let body_facts = check_expr(ctx, body);
    let result_ty = ret_ty.map_or(body_facts.ty, |ret| {
        let origin = ctx.expr(ret).origin;
        lower_type_expr(ctx, ret, origin)
    });
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

fn check_call_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    callee: HirExprId,
    args: SliceRange<HirArg>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let callee_facts = check_expr(ctx, callee);
    let args = ctx.args(args);
    let mut effects = callee_facts.effects.clone();
    let arg_types = args
        .iter()
        .map(|arg| {
            let facts = check_expr(ctx, arg.expr);
            effects.union_with(&facts.effects);
            facts.ty
        })
        .collect::<Vec<_>>();
    let (params, ret) = if let HirTyKind::Arrow { params, ret, .. } = ctx.ty(callee_facts.ty).kind {
        (ctx.ty_ids(params), ret)
    } else {
        ctx.diag(origin.span, "invalid call target", "");
        return ExprFacts {
            ty: builtins.unknown,
            effects,
        };
    };
    if params.len() == arg_types.len() {
        for (expected, found) in params.into_iter().zip(arg_types) {
            type_mismatch(ctx, origin, expected, found);
        }
    } else {
        ctx.diag(origin.span, "call arity mismatch", "");
    }
    if let Some(extra) = call_effects_for_expr(ctx, callee) {
        effects.union_with(&extra);
    } else if let Some(scheme) = callable_scheme_for_expr(ctx, callee)
        && scheme.type_params.is_empty()
    {
        let instantiated = instantiate_monomorphic_scheme(ctx, &scheme);
        solve_obligations(ctx, origin, &instantiated.obligations);
        effects.union_with(&instantiated.effects);
    }
    ExprFacts { ty: ret, effects }
}

fn check_apply_expr(
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
            let origin = ctx.expr(arg).origin;
            lower_type_expr(ctx, arg, origin)
        })
        .collect::<Vec<_>>();
    let Some(scheme) = callable_scheme_for_expr(ctx, callee) else {
        ctx.diag(origin.span, "invalid type application", "");
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

fn check_index_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    base: HirExprId,
    args: SliceRange<HirExprId>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let base_facts = check_expr(ctx, base);
    let mut effects = base_facts.effects.clone();
    for arg in ctx.expr_ids(args) {
        let facts = check_expr(ctx, arg);
        effects.union_with(&facts.effects);
    }
    let ty = if let HirTyKind::Array { item, .. } = ctx.ty(base_facts.ty).kind {
        item
    } else {
        ctx.diag(origin.span, "invalid index target", "");
        builtins.unknown
    };
    ExprFacts { ty, effects }
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

    let ty = match ctx.ty(base_facts.ty).kind {
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
    let mut fields = if let HirTyKind::Record { fields } = ctx.ty(base_facts.ty).kind {
        ctx.ty_fields(fields)
            .into_iter()
            .map(|field| (field.name, field.ty))
            .collect::<BTreeMap<_, _>>()
    } else {
        ctx.diag(origin.span, "invalid record update target", "");
        BTreeMap::new()
    };
    for item in ctx.record_items(items) {
        let facts = check_expr(ctx, item.value);
        effects.union_with(&facts.effects);
        if let Some(name) = item.name {
            let _prev = fields.insert(name.name, facts.ty);
        }
    }
    let fields = ctx.alloc_ty_fields(fields.into_iter().map(|(name, ty)| HirTyField { name, ty }));
    let ty = ctx.alloc_ty(HirTyKind::Record { fields });
    ExprFacts { ty, effects }
}

fn check_type_test_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    base: HirExprId,
    ty_expr: HirExprId,
    as_name: Option<Ident>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let base_facts = check_expr(ctx, base);
    let origin = ctx.expr(ty_expr).origin;
    let _ = lower_type_expr(ctx, ty_expr, origin);
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
    let builtins = ctx.builtins();
    let left_facts = check_expr(ctx, left);
    let right_facts = check_expr(ctx, right);
    let mut effects = left_facts.effects.clone();
    effects.union_with(&right_facts.effects);
    let ty = match op {
        HirBinaryOp::Assign => {
            type_mismatch(ctx, origin, left_facts.ty, right_facts.ty);
            builtins.unit
        }
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
