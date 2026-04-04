use std::collections::BTreeMap;

use music_hir::{HirPatId, HirPatKind, HirTyId, HirTyKind};
use music_names::Ident;

use crate::api::PatFacts;
use crate::context::{CheckPass, PassBase};
use crate::exprs::check_expr;
use crate::normalize::type_mismatch;

pub fn bound_name_from_pat(ctx: &PassBase<'_, '_>, pat: HirPatId) -> Option<Ident> {
    match ctx.pat(pat).kind {
        HirPatKind::Bind { name } => Some(name),
        _ => None,
    }
}

pub fn bind_pat(ctx: &mut CheckPass<'_, '_>, pat: HirPatId, ty: HirTyId) {
    let builtins = ctx.builtins();
    let pat_node = ctx.pat(pat);
    ctx.set_pat_facts(pat, PatFacts { ty });
    match pat_node.kind {
        HirPatKind::Error | HirPatKind::Wildcard => {}
        HirPatKind::Bind { name } => {
            if let Some(binding) = ctx.binding_id_for_decl(name) {
                ctx.insert_binding_type(binding, ty);
            }
        }
        HirPatKind::Lit { expr } => {
            let facts = check_expr(ctx, expr);
            let origin = ctx.expr(expr).origin;
            type_mismatch(ctx, origin, ty, facts.ty);
        }
        HirPatKind::Tuple { items } => {
            let tuple_items = match ctx.ty(ty).kind {
                HirTyKind::Tuple { items: tuple_items }
                    if ctx.ty_ids(tuple_items).len() == ctx.pat_ids(items).len() =>
                {
                    Some(ctx.ty_ids(tuple_items))
                }
                _ => None,
            };
            for (idx, item) in ctx.pat_ids(items).into_iter().enumerate() {
                let item_ty = tuple_items
                    .as_ref()
                    .and_then(|items| items.get(idx).copied())
                    .unwrap_or(builtins.unknown);
                bind_pat(ctx, item, item_ty);
            }
        }
        HirPatKind::Array { items } => {
            let item_ty = match ctx.ty(ty).kind {
                HirTyKind::Array { item, .. } => item,
                _ => builtins.unknown,
            };
            for item in ctx.pat_ids(items) {
                bind_pat(ctx, item, item_ty);
            }
        }
        HirPatKind::Record { fields } => {
            let record_fields = match ctx.ty(ty).kind {
                HirTyKind::Record { fields } => ctx
                    .ty_fields(fields)
                    .into_iter()
                    .map(|field| (field.name, field.ty))
                    .collect::<BTreeMap<_, _>>(),
                _ => BTreeMap::new(),
            };
            for field in ctx.record_pat_fields(fields) {
                let field_ty = record_fields
                    .get(&field.name.name)
                    .copied()
                    .unwrap_or(builtins.unknown);
                if let Some(value) = field.value {
                    bind_pat(ctx, value, field_ty);
                } else if let Some(binding) = ctx.binding_id_for_decl(field.name) {
                    ctx.insert_binding_type(binding, field_ty);
                }
            }
        }
        HirPatKind::Variant { tag: _, args } => {
            for arg in ctx.pat_ids(args) {
                bind_pat(ctx, arg, builtins.unknown);
            }
        }
        HirPatKind::Or { left, right } => {
            bind_pat(ctx, left, ty);
            bind_pat(ctx, right, ty);
        }
        HirPatKind::As { pat, name } => {
            bind_pat(ctx, pat, ty);
            if let Some(binding) = ctx.binding_id_for_decl(name) {
                ctx.insert_binding_type(binding, ty);
            }
        }
    }
}
