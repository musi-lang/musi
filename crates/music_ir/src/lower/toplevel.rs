use music_arena::SliceRange;
use music_hir::{HirExprId, HirExprKind, HirParam, HirPatKind, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId};
use music_sema::{DefinitionKey, EffectRow, SemaDataDef, SemaModule};

use super::{LetItemInput, LowerCtx, TopLevelItems};
use crate::api::{
    IrCallable, IrDataDef, IrDataVariantDef, IrExpr, IrExprKind, IrForeignDef, IrGlobal, IrParam,
};

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

    if let Some(foreign) =
        lower_foreign_item(sema, interner, expr_id, name, params.clone(), exported)
    {
        items.foreigns.push(foreign);
        return;
    }

    let module_target = sema.expr_module_target(value).cloned();
    let effects = sema
        .try_expr_effects(value)
        .unwrap_or_else(|| super::invalid_lowering_path("expr effects missing for top-level value"))
        .clone();
    if let Some(global) = lower_exported_import_global(
        ctx,
        ExportedImportGlobalInput {
            binding,
            name,
            value,
            exported,
            effects: &effects,
            module_target: module_target.clone(),
        },
    ) {
        items.globals.push(global);
        return;
    }

    if skip_module_value(sema, value) {
        return;
    }

    if let Some(data_def) = lower_data_item(ctx, name, value) {
        items.data_defs.push(data_def);
        return;
    }

    if matches!(
        sema.module().store.exprs.get(value).kind,
        HirExprKind::Effect { .. } | HirExprKind::Class { .. } | HirExprKind::Instance { .. }
    ) {
        return;
    }

    if is_callable {
        items.callables.push(lower_callable_item(
            ctx,
            CallableItemInput {
                binding,
                name,
                params,
                value,
                exported,
                effects,
                module_target,
            },
        ));
        return;
    }

    items.globals.push(lower_global_item(
        ctx,
        GlobalItemInput {
            binding,
            name,
            value,
            exported,
            effects,
            module_target,
        },
    ));
}

fn lower_foreign_item(
    sema: &SemaModule,
    interner: &Interner,
    expr_id: HirExprId,
    name: Ident,
    params: SliceRange<HirParam>,
    exported: bool,
) -> Option<IrForeignDef> {
    sema.module()
        .store
        .exprs
        .get(expr_id)
        .mods
        .foreign
        .is_some()
        .then(|| super::lower_foreign_let(sema, interner, expr_id, name, params, exported))
}

fn lower_data_item(ctx: &LowerCtx<'_>, name: Ident, value: HirExprId) -> Option<IrDataDef> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let HirExprKind::Data {
        variants: _,
        fields,
    } = &sema.module().store.exprs.get(value).kind
    else {
        return None;
    };
    let name_text: Box<str> = interner.resolve(name.name).into();
    let def = sema.data_def(name_text.as_ref());
    let key = def.map_or_else(
        || DefinitionKey {
            module: ctx.module_key.clone(),
            name: name_text.clone(),
        },
        |data| data.key().clone(),
    );
    let repr_kind = def.and_then(|data| data.repr_kind().map(Into::into));
    let layout_align = def.and_then(SemaDataDef::layout_align);
    let layout_pack = def.and_then(SemaDataDef::layout_pack);
    let variants = def.map_or_else(
        || {
            let field_tys = sema
                .module()
                .store
                .fields
                .get(fields.clone())
                .iter()
                .map(|_| Box::<str>::from("Unknown"))
                .collect::<Vec<_>>();
            vec![IrDataVariantDef {
                name: name_text.clone(),
                field_tys: field_tys.into_boxed_slice(),
            }]
            .into_boxed_slice()
        },
        |data| {
            data.variants()
                .map(|(variant_name, variant)| IrDataVariantDef {
                    name: variant_name.into(),
                    field_tys: variant
                        .field_tys()
                        .iter()
                        .copied()
                        .map(|ty| render_hir_ty_name(sema, ty, interner))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                })
                .collect::<Vec<_>>()
                .into_boxed_slice()
        },
    );
    let field_count = variants
        .iter()
        .map(|variant| variant.field_tys.len())
        .max()
        .unwrap_or_else(|| sema.module().store.fields.get(fields.clone()).len());
    Some(IrDataDef {
        key,
        variant_count: u32::try_from(variants.len())
            .unwrap_or_else(|_| super::invalid_lowering_path("variant count overflow")),
        field_count: u32::try_from(field_count)
            .unwrap_or_else(|_| super::invalid_lowering_path("field count overflow")),
        variants,
        repr_kind,
        layout_align,
        layout_pack,
    })
}

fn render_hir_ty_name(sema: &SemaModule, ty: HirTyId, interner: &Interner) -> Box<str> {
    match &sema.ty(ty).kind {
        HirTyKind::Error => "<error>".into(),
        HirTyKind::Unknown => "Unknown".into(),
        HirTyKind::Type => "Type".into(),
        HirTyKind::Syntax => "Syntax".into(),
        HirTyKind::Any => "Any".into(),
        HirTyKind::Empty => "Empty".into(),
        HirTyKind::Unit => "Unit".into(),
        HirTyKind::Bool => "Bool".into(),
        HirTyKind::Nat => "Nat".into(),
        HirTyKind::Int => "Int".into(),
        HirTyKind::Float => "Float".into(),
        HirTyKind::String => "String".into(),
        HirTyKind::CString => "CString".into(),
        HirTyKind::CPtr => "CPtr".into(),
        HirTyKind::Module => "Module".into(),
        HirTyKind::NatLit(value) => value.to_string().into(),
        HirTyKind::Named { name, args } => {
            super::render_named_ty_name(sema, *name, *args, interner)
        }
        HirTyKind::Tuple { items } => format!(
            "({})",
            sema.module()
                .store
                .ty_ids
                .get(*items)
                .iter()
                .copied()
                .map(|item| render_hir_ty_name(sema, item, interner).into_string())
                .collect::<Vec<_>>()
                .join(", ")
        )
        .into_boxed_str(),
        HirTyKind::Array { .. }
        | HirTyKind::Pi { .. }
        | HirTyKind::Arrow { .. }
        | HirTyKind::Sum { .. }
        | HirTyKind::Mut { .. }
        | HirTyKind::Record { .. } => super::render_ty_name(sema, ty, interner),
    }
}

struct CallableItemInput {
    binding: Option<NameBindingId>,
    name: Ident,
    params: SliceRange<HirParam>,
    value: HirExprId,
    exported: bool,
    effects: EffectRow,
    module_target: Option<ModuleKey>,
}

fn lower_callable_item(ctx: &mut LowerCtx<'_>, input: CallableItemInput) -> IrCallable {
    let interner = ctx.interner;
    IrCallable {
        binding: input.binding,
        name: interner.resolve(input.name.name).into(),
        params: lower_params(ctx, input.params),
        body: super::lower_expr(ctx, input.value),
        exported: input.exported,
        effects: input.effects,
        module_target: input.module_target,
    }
}

struct GlobalItemInput {
    binding: Option<NameBindingId>,
    name: Ident,
    value: HirExprId,
    exported: bool,
    effects: EffectRow,
    module_target: Option<ModuleKey>,
}

fn lower_global_item(ctx: &mut LowerCtx<'_>, input: GlobalItemInput) -> IrGlobal {
    let interner = ctx.interner;
    IrGlobal {
        binding: input.binding,
        name: interner.resolve(input.name.name).into(),
        body: super::lower_expr(ctx, input.value),
        exported: input.exported,
        effects: input.effects,
        module_target: input.module_target,
    }
}

struct ExportedImportGlobalInput<'a> {
    binding: Option<NameBindingId>,
    name: Ident,
    value: HirExprId,
    exported: bool,
    effects: &'a EffectRow,
    module_target: Option<ModuleKey>,
}

fn lower_exported_import_global(
    ctx: &mut LowerCtx<'_>,
    input: ExportedImportGlobalInput<'_>,
) -> Option<IrGlobal> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let HirExprKind::Import { arg } = &sema.module().store.exprs.get(input.value).kind else {
        return None;
    };
    if !input.exported {
        return None;
    }
    let spec = super::lower_expr(ctx, *arg);
    Some(IrGlobal {
        binding: input.binding,
        name: interner.resolve(input.name.name).into(),
        body: IrExpr {
            origin: spec.origin,
            kind: IrExprKind::DynamicImport {
                spec: Box::new(spec),
            },
        },
        exported: true,
        effects: input.effects.clone(),
        module_target: input.module_target,
    })
}

fn skip_module_value(sema: &SemaModule, value: HirExprId) -> bool {
    matches!(
        sema.ty(sema
            .try_expr_ty(value)
            .unwrap_or_else(|| super::invalid_lowering_path(
                "expr type missing for top-level value"
            )))
        .kind,
        HirTyKind::Module
    )
}

fn lower_params(ctx: &LowerCtx<'_>, params: SliceRange<HirParam>) -> Box<[IrParam]> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    sema.module()
        .store
        .params
        .get(params)
        .iter()
        .map(|param| IrParam {
            binding: super::decl_binding_id(sema, param.name)
                .unwrap_or_else(|| super::invalid_lowering_path("param binding missing")),
            name: interner.resolve(param.name.name).into(),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}
