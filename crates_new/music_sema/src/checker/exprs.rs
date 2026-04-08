use std::collections::{BTreeMap, BTreeSet};

use music_arena::SliceRange;
use music_base::Span;
use music_hir::{
    HirAccessKind, HirArg, HirArrayItem, HirBinaryOp, HirCaseArm, HirConstraint, HirDim, HirExprId,
    HirExprKind, HirLitId, HirLitKind, HirMemberDef, HirOrigin, HirParam, HirPrefixOp,
    HirQuoteKind, HirRecordItem, HirSpliceKind, HirTemplatePart, HirTyField, HirTyId, HirTyKind,
};
use music_names::Ident;

use crate::api::ExprFacts;

use super::CheckPass;
use super::state::DataDef;
use super::decls::{
    LetExprInput, call_effects_for_expr, check_attributed_expr, check_foreign_expr,
    check_handle_expr, check_import_expr, check_instance_expr, check_let_expr, check_perform_expr,
    check_resume_expr, module_export_for_expr, module_target_for_expr,
};
use super::normalize::{lower_params, lower_type_expr, symbol_value_type, type_mismatch};
use super::patterns::bind_pat;
use super::schemes::{
    BindingScheme, instantiate_binding_scheme, instantiate_monomorphic_scheme, scheme_from_export,
    solve_obligations,
};
use crate::effects::EffectRow;

pub fn check_module_root(ctx: &mut CheckPass<'_, '_, '_>, id: HirExprId) -> ExprFacts {
    check_module_stmt(ctx, id)
}

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
        HirExprKind::Data { .. }
        | HirExprKind::Effect { .. }
        | HirExprKind::Class { .. }
        | HirExprKind::Instance { .. }
        | HirExprKind::Foreign { .. } => {
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
        HirExprKind::Attributed { attrs, expr: inner } => {
            check_attributed_expr(ctx, origin, attrs, inner)
        }
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
        HirExprKind::Export { expr: inner, .. } => {
            let inner_facts = check_module_stmt(ctx, inner);
            ctx.set_expr_facts(inner, inner_facts.clone());
            let key = ctx.module_key().clone();
            ctx.set_expr_module_target(id, key);
            inner_facts
        }
        HirExprKind::Attributed { attrs, expr: inner } => {
            let facts = check_attributed_expr(ctx, origin, attrs, inner);
            ctx.set_expr_facts(inner, facts.clone());
            facts
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
        HirExprKind::Foreign { abi, decls } => check_foreign_expr(ctx, origin, abi, decls, None),
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
    let builtins = ctx.builtins();
    if let Some(binding) = ctx.binding_id_for_use(name) && ctx.is_gated_binding(binding) {
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

fn check_array_expr(ctx: &mut CheckPass<'_, '_, '_>, items: SliceRange<HirArrayItem>) -> ExprFacts {
    let builtins = ctx.builtins();
    let mut effects = EffectRow::empty();
    let (expected_dims, expected_item) = expected_array_contract(ctx);
    let mut item_ty = expected_item.unwrap_or(builtins.unknown);

    let mut has_runtime_spread = false;
    let mut known_len: u32 = 0;
    let items_vec = ctx.array_items(items);
    for array_item in &items_vec {
        if !array_item.spread {
            ctx.push_expected_ty(item_ty);
            let facts = check_expr(ctx, array_item.expr);
            let _ = ctx.pop_expected_ty();
            effects.union_with(&facts.effects);
            if item_ty == builtins.unknown {
                item_ty = facts.ty;
            } else {
                let origin = ctx.expr(array_item.expr).origin;
                type_mismatch(ctx, origin, item_ty, facts.ty);
            }
            known_len = known_len.saturating_add(1);
            continue;
        }

        let spread_facts = check_expr(ctx, array_item.expr);
        effects.union_with(&spread_facts.effects);
        let spread_origin = ctx.expr(array_item.expr).origin;
        let spread_ty = peel_mut_ty(ctx, spread_facts.ty);
        match ctx.ty(spread_ty).kind {
            HirTyKind::Tuple { items } => {
                let item_tys = ctx.ty_ids(items);
                for found in item_tys {
                    merge_array_item_ty(ctx, spread_origin, &mut item_ty, found);
                    known_len = known_len.saturating_add(1);
                }
            }
            HirTyKind::Array { dims, item } => {
                let dims_vec = ctx.dims(dims);
                if dims_vec.is_empty() {
                    has_runtime_spread = true;
                    merge_array_item_ty(ctx, spread_origin, &mut item_ty, item);
                    continue;
                }
                if dims_vec.len() != 1 {
                    ctx.diag(spread_origin.span, "array spread requires 1D array", "");
                    continue;
                }
                match dims_vec[0] {
                    HirDim::Int(len) => {
                        merge_array_item_ty(ctx, spread_origin, &mut item_ty, item);
                        known_len = known_len.saturating_add(len);
                    }
                    HirDim::Unknown | HirDim::Name(_) => {
                        has_runtime_spread = true;
                        merge_array_item_ty(ctx, spread_origin, &mut item_ty, item);
                    }
                }
            }
            _ => ctx.diag(spread_origin.span, "invalid array spread source", ""),
        }
    }

    check_array_literal_expected_len(ctx, expected_dims.as_ref(), &items_vec, has_runtime_spread, known_len);

    let dims = expected_dims.unwrap_or_else(|| ctx.alloc_dims([HirDim::Unknown]));
    let ty = ctx.alloc_ty(HirTyKind::Array {
        dims,
        item: item_ty,
    });
    ExprFacts { ty, effects }
}

fn expected_array_contract(ctx: &CheckPass<'_, '_, '_>) -> (Option<SliceRange<HirDim>>, Option<HirTyId>) {
    let expected_array = ctx.expected_ty().and_then(|expected| {
        let expected_inner = peel_mut_ty(ctx, expected);
        match ctx.ty(expected_inner).kind {
            HirTyKind::Array { dims, item } => Some((dims, item)),
            _ => None,
        }
    });
    let expected_dims = expected_array.as_ref().map(|(dims, _)| dims.clone());
    let expected_item = expected_array.as_ref().map(|(_, item)| *item);
    (expected_dims, expected_item)
}

fn merge_array_item_ty(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    item_ty: &mut HirTyId,
    found: HirTyId,
) {
    let builtins = ctx.builtins();
    if *item_ty == builtins.unknown {
        *item_ty = found;
    } else {
        type_mismatch(ctx, origin, *item_ty, found);
    }
}

fn check_array_literal_expected_len(
    ctx: &mut CheckPass<'_, '_, '_>,
    expected_dims: Option<&SliceRange<HirDim>>,
    items: &[HirArrayItem],
    has_runtime_spread: bool,
    known_len: u32,
) {
    let Some(expected_dims) = expected_dims else {
        return;
    };
    let dims_vec = ctx.dims(expected_dims.clone());
    if dims_vec.len() != 1 {
        return;
    }
    let HirDim::Int(expected_len) = dims_vec[0] else {
        return;
    };
    let span = items.first().map_or_else(
        || Span::new(0, 0),
        |array_item| ctx.expr(array_item.expr).origin.span,
    );
    if has_runtime_spread {
        ctx.diag(span, "array literal length unknown due to runtime spread", "");
    } else if expected_len != known_len {
        ctx.diag(span, "array literal length mismatch", "");
    }
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
    let expected_record = ctx.expected_ty().and_then(|expected| {
        let expected_inner = peel_mut_ty(ctx, expected);
        match ctx.ty(expected_inner).kind {
            HirTyKind::Record { fields } => Some(
                ctx.ty_fields(fields)
                    .into_iter()
                    .map(|field| (field.name, field.ty))
                    .collect::<BTreeMap<_, _>>(),
            ),
            _ => None,
        }
    });

    let mut seen_explicit = BTreeSet::<Box<str>>::new();
    let mut fields = BTreeMap::<Box<str>, HirTyField>::new();
    for record_item in ctx.record_items(items) {
        if record_item.spread {
            let facts = check_expr(ctx, record_item.value);
            effects.union_with(&facts.effects);
            let origin = ctx.expr(record_item.value).origin;
            let spread_ty = peel_mut_ty(ctx, facts.ty);
            let HirTyKind::Record { fields: spread_fields } = ctx.ty(spread_ty).kind else {
                ctx.diag(origin.span, "invalid record spread source", "");
                continue;
            };
            for spread_field in ctx.ty_fields(spread_fields) {
                let key: Box<str> = ctx.resolve_symbol(spread_field.name).into();
                let _prev = fields.insert(key, spread_field);
            }
            continue;
        }

        let Some(name) = record_item.name else {
            let facts = check_expr(ctx, record_item.value);
            effects.union_with(&facts.effects);
            continue;
        };
        let expected_field_ty = expected_record
            .as_ref()
            .and_then(|map| map.get(&name.name).copied())
            .unwrap_or_else(|| ctx.builtins().unknown);
        ctx.push_expected_ty(expected_field_ty);
        let facts = check_expr(ctx, record_item.value);
        let _ = ctx.pop_expected_ty();
        effects.union_with(&facts.effects);

        let key: Box<str> = ctx.resolve_symbol(name.name).into();
        if !seen_explicit.insert(key.clone()) {
            let span = ctx.expr(record_item.value).origin.span;
            ctx.diag(span, "duplicate record field", "");
        }
        let _prev = fields.insert(
            key,
            HirTyField {
                name: name.name,
                ty: facts.ty,
            },
        );
    }
    let fields = ctx.alloc_ty_fields(fields.into_values());
    let ty = ctx.alloc_ty(HirTyKind::Record { fields });
    ExprFacts { ty, effects }
}

fn check_variant_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    tag: Ident,
    args: SliceRange<HirExprId>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    if let Some(facts) = check_sum_constructor_variant(ctx, tag, args) {
        return facts;
    }

    let mut effects = EffectRow::empty();
    let expected_ty = ctx.expected_ty().and_then(|ty| variant_context_ty(ctx, ty));
    let expected_ty = expected_ty.or_else(|| infer_variant_context_ty(ctx, tag));
    let Some(expected_ty) = expected_ty else {
        check_exprs_collect_effects(ctx, ctx.expr_ids(args), &mut effects);
        return ExprFacts {
            ty: builtins.unknown,
            effects,
        };
    };

    let data_def = expected_data_def(ctx, expected_ty);
    let Some(data_def) = data_def else {
        check_exprs_collect_effects(ctx, ctx.expr_ids(args), &mut effects);
        ctx.diag(tag.span, "variant constructor missing data type context", "");
        return ExprFacts {
            ty: builtins.unknown,
            effects,
        };
    };

    let tag_name = ctx.resolve_symbol(tag.name);
    let Some(variant) = data_def.variants.get(tag_name) else {
        check_exprs_collect_effects(ctx, ctx.expr_ids(args), &mut effects);
        ctx.diag(tag.span, "unknown data variant", "");
        return ExprFacts {
            ty: expected_ty,
            effects,
        };
    };

    let expected_payload = variant.payload;
    let arg_exprs = ctx.expr_ids(args);
    let expected_args: Vec<HirTyId> = expected_payload.map_or_else(Vec::new, |payload_ty| {
        match &ctx.ty(payload_ty).kind {
            HirTyKind::Tuple { items } => ctx.ty_ids(*items),
            _ => vec![payload_ty],
        }
    });

    typecheck_positional_args(
        ctx,
        tag.span,
        &expected_args,
        arg_exprs,
        &mut effects,
        "variant constructor arity mismatch",
    );

    ExprFacts {
        ty: expected_ty,
        effects,
    }
}

fn check_sum_constructor_variant(
    ctx: &mut CheckPass<'_, '_, '_>,
    tag: Ident,
    args: SliceRange<HirExprId>,
) -> Option<ExprFacts> {
    let builtins = ctx.builtins();
    let mut effects = EffectRow::empty();
    let expected_sum_ty = ctx.expected_ty().and_then(|ty| {
        let inner = peel_mut_ty(ctx, ty);
        matches!(ctx.ty(inner).kind, HirTyKind::Sum { .. }).then_some(inner)
    })?;
    let HirTyKind::Sum { left, right } = ctx.ty(expected_sum_ty).kind else {
        return Some(ExprFacts {
            ty: builtins.unknown,
            effects,
        });
    };
    let tag_name = ctx.resolve_symbol(tag.name);
    let chosen = match tag_name {
        "Left" => Some(left),
        "Right" => Some(right),
        _ => None,
    }?;

    let _sum_def = ctx.ensure_sum_data_def(left, right);
    let arg_exprs = ctx.expr_ids(args);
    let expected_args: Vec<HirTyId> = match &ctx.ty(chosen).kind {
        HirTyKind::Tuple { items } => ctx.ty_ids(*items),
        _ => vec![chosen],
    };
    typecheck_positional_args(
        ctx,
        tag.span,
        &expected_args,
        arg_exprs,
        &mut effects,
        "sum constructor arity mismatch",
    );
    Some(ExprFacts {
        ty: expected_sum_ty,
        effects,
    })
}

fn typecheck_positional_args(
    ctx: &mut CheckPass<'_, '_, '_>,
    diag_span: Span,
    expected_args: &[HirTyId],
    arg_exprs: Vec<HirExprId>,
    effects: &mut EffectRow,
    arity_diag: &str,
) {
    let builtins = ctx.builtins();
    if expected_args.len() != arg_exprs.len() {
        ctx.diag(diag_span, arity_diag, "");
    }
    for (index, arg) in arg_exprs.into_iter().enumerate() {
        let expected = expected_args
            .get(index)
            .copied()
            .unwrap_or(builtins.unknown);
        ctx.push_expected_ty(expected);
        let facts = check_expr(ctx, arg);
        let _ = ctx.pop_expected_ty();
        effects.union_with(&facts.effects);
        let origin = ctx.expr(arg).origin;
        type_mismatch(ctx, origin, expected, facts.ty);
    }
}

fn check_exprs_collect_effects(
    ctx: &mut CheckPass<'_, '_, '_>,
    exprs: Vec<HirExprId>,
    effects: &mut EffectRow,
) {
    for expr in exprs {
        let facts = check_expr(ctx, expr);
        effects.union_with(&facts.effects);
    }
}

fn variant_context_ty(ctx: &CheckPass<'_, '_, '_>, ty: HirTyId) -> Option<HirTyId> {
    expected_data_def(ctx, ty).map(|_| ty)
}

fn expected_data_def<'a>(ctx: &'a CheckPass<'_, '_, '_>, ty: HirTyId) -> Option<&'a DataDef> {
    match ctx.ty(ty).kind {
        HirTyKind::Named { name, .. } => ctx.data_def(ctx.resolve_symbol(name)),
        _ => None,
    }
}

fn infer_variant_context_ty(ctx: &mut CheckPass<'_, '_, '_>, tag: Ident) -> Option<HirTyId> {
    let tag_name = ctx.resolve_symbol(tag.name);
    let mut matches = ctx
        .data_defs()
        .iter()
        .filter_map(|(name, data)| data.variants.contains_key(tag_name).then_some(name.clone()))
        .collect::<Vec<Box<str>>>();

    match matches.len() {
        0 => {
            ctx.diag(tag.span, "unknown data variant", "");
            None
        }
        1 => {
            let data_name = matches.pop()?;
            let name = ctx.intern(data_name.as_ref());
            let args = ctx.alloc_ty_list([]);
            Some(ctx.alloc_ty(HirTyKind::Named { name, args }))
        }
        _ => {
            ctx.diag(
                tag.span,
                "ambiguous variant tag; add type annotation to disambiguate",
                "",
            );
            None
        }
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

fn check_call_expr(
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
        ctx.diag(origin.span, "invalid call target", "");
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
        if !arg.spread {
            let expected = params.get(param_index).copied().unwrap_or(builtins.unknown);
            ctx.push_expected_ty(expected);
            let facts = check_expr(ctx, arg.expr);
            let _ = ctx.pop_expected_ty();
            effects.union_with(&facts.effects);
            let arg_origin = ctx.expr(arg.expr).origin;
            type_mismatch(ctx, arg_origin, expected, facts.ty);
            param_index = param_index.saturating_add(1);
            continue;
        }

        let facts = check_expr(ctx, arg.expr);
        effects.union_with(&facts.effects);
        let spread_origin = ctx.expr(arg.expr).origin;
        let spread_ty = peel_mut_ty(ctx, facts.ty);

        match ctx.ty(spread_ty).kind {
            HirTyKind::Tuple { items } => {
                let item_tys = ctx.ty_ids(items);
                for found in item_tys {
                    let expected = params.get(param_index).copied().unwrap_or(builtins.unknown);
                    type_mismatch(ctx, spread_origin, expected, found);
                    param_index = param_index.saturating_add(1);
                }
            }
            HirTyKind::Array { dims, item } => {
                let dims_vec = ctx.dims(dims);
                if dims_vec.is_empty() {
                    if ctx.ty(item).kind == HirTyKind::Any {
                        has_runtime_spread = true;
                    } else {
                        ctx.diag(spread_origin.span, "call runtime spread requires `[]Any`", "");
                    }
                    continue;
                }
                if dims_vec.len() != 1 {
                    ctx.diag(spread_origin.span, "call spread requires 1D array or tuple", "");
                    continue;
                }
                match dims_vec[0] {
                    HirDim::Int(len) => {
                        for _ in 0..len {
                            let expected =
                                params.get(param_index).copied().unwrap_or(builtins.unknown);
                            type_mismatch(ctx, spread_origin, expected, item);
                            param_index = param_index.saturating_add(1);
                        }
                    }
                    HirDim::Unknown | HirDim::Name(_) if ctx.ty(item).kind == HirTyKind::Any => {
                        has_runtime_spread = true;
                    }
                    _ => ctx.diag(spread_origin.span, "call runtime spread requires `[]Any`", ""),
                }
            }
            _ => ctx.diag(spread_origin.span, "invalid call spread source", ""),
        }
    }

    if !has_runtime_spread {
        if param_index != params.len() {
            ctx.diag(origin.span, "call arity mismatch", "");
        }
    } else if param_index > params.len() {
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
    let ty = if let HirTyKind::Array { item, .. } = ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
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
    let mut fields = if let HirTyKind::Record { fields } = ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
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
            let HirTyKind::Record { fields: spread_fields } = ctx.ty(spread_ty).kind else {
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
            if let Some(binding) = binding
                && !ctx.is_binding_mutable(binding)
            {
                ctx.diag(origin.span, "binding not mutable", "");
            }
            let ty = binding
                .and_then(|binding| ctx.binding_type(binding))
                .unwrap_or_else(|| symbol_value_type(ctx, name.name));
            (ty, EffectRow::empty())
        }
        HirExprKind::Index { base, args } => {
            let base_facts = check_expr(ctx, base);
            let mut effects = base_facts.effects;

            let index_exprs = ctx.expr_ids(args);
            if index_exprs.len() != 1 {
                ctx.diag(origin.span, "invalid index arity", "");
            }
            if let Some(index_expr) = index_exprs.first().copied() {
                let index_facts = check_expr(ctx, index_expr);
                effects.union_with(&index_facts.effects);
                let index_origin = ctx.expr(index_expr).origin;
                type_mismatch(ctx, index_origin, builtins.int_, index_facts.ty);
            }

            let expected = match ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
                HirTyKind::Array { item, .. } if is_mut_ty(ctx, base_facts.ty) => item,
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

fn peel_mut_ty(ctx: &CheckPass<'_, '_, '_>, mut ty: HirTyId) -> HirTyId {
    while let HirTyKind::Mut { inner } = ctx.ty(ty).kind {
        ty = inner;
    }
    ty
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
