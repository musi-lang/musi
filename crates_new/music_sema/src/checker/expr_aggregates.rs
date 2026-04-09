use std::collections::{BTreeMap, BTreeSet};

use music_arena::SliceRange;
use music_base::Span;
use music_hir::{
    HirArrayItem, HirDim, HirExprId, HirOrigin, HirRecordItem, HirTyField, HirTyId, HirTyKind,
};
use music_names::Ident;

use crate::api::ExprFacts;
use crate::effects::EffectRow;

use super::CheckPass;
use super::exprs::peel_mut_ty;
use super::normalize::{lower_type_expr, type_mismatch};
use super::state::DataDef;

pub(super) fn check_array_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    items: SliceRange<HirArrayItem>,
) -> ExprFacts {
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
            let facts = super::exprs::check_expr(ctx, array_item.expr);
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

        let spread_facts = super::exprs::check_expr(ctx, array_item.expr);
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

    check_array_literal_expected_len(
        ctx,
        expected_dims.as_ref(),
        &items_vec,
        has_runtime_spread,
        known_len,
    );

    let dims = expected_dims.unwrap_or_else(|| ctx.alloc_dims([HirDim::Unknown]));
    let ty = ctx.alloc_ty(HirTyKind::Array {
        dims,
        item: item_ty,
    });
    ExprFacts { ty, effects }
}

pub(super) fn check_array_ty_expr(
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

pub(super) fn check_record_expr(
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
            let facts = super::exprs::check_expr(ctx, record_item.value);
            effects.union_with(&facts.effects);
            let origin = ctx.expr(record_item.value).origin;
            let spread_ty = peel_mut_ty(ctx, facts.ty);
            let HirTyKind::Record {
                fields: spread_fields,
            } = ctx.ty(spread_ty).kind
            else {
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
            let facts = super::exprs::check_expr(ctx, record_item.value);
            effects.union_with(&facts.effects);
            continue;
        };
        let expected_field_ty = expected_record
            .as_ref()
            .and_then(|map| map.get(&name.name).copied())
            .unwrap_or_else(|| ctx.builtins().unknown);
        ctx.push_expected_ty(expected_field_ty);
        let facts = super::exprs::check_expr(ctx, record_item.value);
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

pub(super) fn check_variant_expr(
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
        ctx.diag(
            tag.span,
            "variant constructor missing data type context",
            "",
        );
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
    let expected_args: Vec<HirTyId> =
        expected_payload.map_or_else(Vec::new, |payload_ty| match &ctx.ty(payload_ty).kind {
            HirTyKind::Tuple { items } => ctx.ty_ids(*items),
            _ => vec![payload_ty],
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

fn expected_array_contract(
    ctx: &CheckPass<'_, '_, '_>,
) -> (Option<SliceRange<HirDim>>, Option<HirTyId>) {
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
        ctx.diag(
            span,
            "array literal length unknown due to runtime spread",
            "",
        );
    } else if expected_len != known_len {
        ctx.diag(span, "array literal length mismatch", "");
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
        let facts = super::exprs::check_expr(ctx, arg);
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
        let facts = super::exprs::check_expr(ctx, expr);
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
