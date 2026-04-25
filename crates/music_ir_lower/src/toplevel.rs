use std::collections::BTreeMap;
use std::iter::repeat_n;

use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirExprId, HirExprKind, HirMemberDef, HirMemberKind, HirParam, HirPatKind, HirTyId,
    HirTyKind, simple_hir_ty_display_name,
};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId};
use music_sema::{
    DefinitionKey, EffectRow, ExportedValue, GivenFacts, SemaDataDef, SemaModule, SurfaceTyId,
    SurfaceTyKind,
};

use super::closures::lower_user_params;
use super::{LetItemInput, LowerCtx, TopLevelItems};
use music_ir::{
    IrCallable, IrDataDef, IrDataVariantDef, IrExpr, IrExprKind, IrForeignDef, IrGlobal,
    IrModuleInitPart, IrNameRef, IrOrigin, IrParam, IrRecordField,
};

pub(crate) fn collect_top_level_items(
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
        HirExprKind::Unsafe { body } => {
            collect_top_level_items(ctx, *body, exported, items);
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
        HirExprKind::Given { members, .. } => {
            _ = collect_given_item(ctx, expr_id, members, exported, items);
        }
        _ => {
            items
                .init_parts
                .push(IrModuleInitPart::expr(super::lower_expr(ctx, expr_id)));
        }
    }
}

mod given;
mod items;
mod profile;

use given::collect_given_item;
use items::collect_let_item;

#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct ProfileAttrs {
    pub(crate) hot: bool,
    pub(crate) cold: bool,
}

pub(crate) fn profile_attrs(
    sema: &SemaModule,
    interner: &Interner,
    attrs: SliceRange<HirAttr>,
) -> ProfileAttrs {
    profile::profile_attrs(sema, interner, attrs)
}
