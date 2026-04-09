use music_arena::SliceRange;
use music_hir::{HirExprId, HirExprKind, HirParam, HirPatKind, HirTyKind};
use music_sema::DefinitionKey;

use super::{LetItemInput, LowerCtx, TopLevelItems};
use crate::api::{IrCallable, IrDataDef, IrGlobal, IrParam};

pub(super) fn collect_top_level_items(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    exported: bool,
    items: &mut TopLevelItems,
) {
    let sema = ctx.sema;
    let expr = sema.module().store.exprs.get(expr_id);
    let exported = exported || expr.mods.export.is_some();
    match &expr.kind {
        HirExprKind::Sequence { exprs } => {
            for expr in sema.module().store.expr_ids.get(*exprs).iter().copied() {
                collect_top_level_items(ctx, expr, exported, items);
            }
        }
        HirExprKind::Let {
            pat,
            has_param_clause,
            params,
            value,
            ..
        } => {
            let is_callable =
                *has_param_clause || !sema.module().store.params.get(params.clone()).is_empty();
            collect_let_item(
                ctx,
                LetItemInput {
                    expr_id,
                    pat: *pat,
                    params: params.clone(),
                    value: *value,
                    is_callable,
                    exported,
                },
                items,
            );
        }
        _ => {}
    }
}

fn collect_let_item(ctx: &mut LowerCtx<'_>, input: LetItemInput, items: &mut TopLevelItems) {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let LetItemInput {
        expr_id,
        pat,
        params,
        value,
        is_callable,
        exported,
    } = input;
    let HirPatKind::Bind { name } = sema.module().store.pats.get(pat).kind else {
        return;
    };
    let binding = super::decl_binding_id(sema, name);
    if let Some(binding) = binding {
        if sema.is_gated_binding(binding) {
            return;
        }
    }

    if sema.module().store.exprs.get(expr_id).mods.foreign.is_some() {
        items.foreigns.push(super::lower_foreign_let(
            sema,
            interner,
            expr_id,
            name,
            params,
            exported,
        ));
        return;
    }

    let module_target = sema.expr_module_target(value).cloned();
    let effects = sema.expr_effects(value).clone();
    if matches!(sema.ty(sema.expr_ty(value)).kind, HirTyKind::Module)
        || matches!(
            sema.module().store.exprs.get(value).kind,
            HirExprKind::Import { .. }
        )
    {
        return;
    }

    match &sema.module().store.exprs.get(value).kind {
        HirExprKind::Data { variants, fields } => {
            let name_text: Box<str> = interner.resolve(name.name).into();
            let def = sema.data_def(name_text.as_ref());
            let key = def.map_or_else(
                || DefinitionKey {
                    module: ctx.module_key.clone(),
                    name: name_text.clone(),
                },
                |data| data.key.clone(),
            );
            let repr_kind = def.and_then(|data| data.repr_kind.clone());
            let layout_align = def.and_then(|data| data.layout_align);
            let layout_pack = def.and_then(|data| data.layout_pack);
            items.data_defs.push(IrDataDef {
                key,
                variant_count: u32::try_from(sema.module().store.variants.get(variants.clone()).len())
                    .expect("variant count overflow"),
                field_count: u32::try_from(sema.module().store.fields.get(fields.clone()).len())
                    .expect("field count overflow"),
                repr_kind,
                layout_align,
                layout_pack,
            });
        }
        HirExprKind::Effect { .. } | HirExprKind::Class { .. } | HirExprKind::Instance { .. } => {}
        _ if is_callable => {
            items.callables.push(IrCallable {
                binding,
                name: interner.resolve(name.name).into(),
                params: lower_params(ctx, params),
                body: super::lower_expr(ctx, value),
                exported,
                effects,
                module_target,
            });
        }
        _ => {
            items.globals.push(IrGlobal {
                binding,
                name: interner.resolve(name.name).into(),
                body: super::lower_expr(ctx, value),
                exported,
                effects,
                module_target,
            });
        }
    }
}

fn lower_params(ctx: &LowerCtx<'_>, params: SliceRange<HirParam>) -> Box<[IrParam]> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    sema
        .module()
        .store
        .params
        .get(params)
        .iter()
        .map(|param| IrParam {
            binding: super::decl_binding_id(sema, param.name).expect("param binding missing"),
            name: interner.resolve(param.name.name).into(),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}
