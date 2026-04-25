use super::{
    BTreeMap, DefinitionKey, Diag, DiagContext, HashMap, HashSet, Interner, IrDataDef,
    IrDataVariantDef, IrDiagKind, IrDiagList, IrEffectDef, IrEffectOpDef, IrGivenDef, IrModule,
    IrModuleParts, IrShapeDef, LowerCtx, SemaModule, TopLevelItems, bindings, meta, render_ty_name,
    toplevel, validate,
};
use std::any::Any;
use std::panic::{AssertUnwindSafe, catch_unwind, resume_unwind};

#[derive(Debug)]
pub(crate) struct LoweringInvariant {
    kind: IrDiagKind,
    context: DiagContext,
}

/// Lowers sema-owned module facts into the codegen-facing IR surface.
///
/// # Errors
///
/// Returns semantic diagnostics when exported surface types or effect rows reference invalid
/// sema-owned ids.
pub fn lower_module(sema: &SemaModule, interner: &Interner) -> Result<IrModule, IrDiagList> {
    match catch_unwind(AssertUnwindSafe(|| lower_module_impl(sema, interner))) {
        Ok(result) => result,
        Err(payload) => {
            let (kind, context) = payload.downcast_ref::<LoweringInvariant>().map_or_else(
                || {
                    (
                        IrDiagKind::LoweringInvariantViolated,
                        DiagContext::new().with("subject", panic_payload_text(payload.as_ref())),
                    )
                },
                |invariant| (invariant.kind, invariant.context.clone()),
            );
            Err(vec![
                Diag::error(kind.message_with(&context)).with_code(kind.code()),
            ])
        }
    }
}

pub(crate) fn panic_payload_text(payload: &(dyn Any + Send)) -> Box<str> {
    if let Some(text) = payload.downcast_ref::<String>() {
        return text.clone().into_boxed_str();
    }
    if let Some(text) = payload.downcast_ref::<&'static str>() {
        return (*text).into();
    }
    "<non-string panic payload>".into()
}

pub(crate) fn lower_module_impl(
    sema: &SemaModule,
    interner: &Interner,
) -> Result<IrModule, IrDiagList> {
    let mut diags = Vec::<Diag>::new();
    if !sema.diags().is_empty() {
        diags.push(
            Diag::error(IrDiagKind::LoweringRequiresSemaCleanModule.message())
                .with_code(IrDiagKind::LoweringRequiresSemaCleanModule.code())
                .with_note(format!("sema diagnostic count `{}`", sema.diags().len())),
        );
        return Err(diags);
    }
    validate::validate_surface(sema, &mut diags);
    if !diags.is_empty() {
        return Err(diags);
    }

    let module_level_bindings = bindings::collect_module_level_bindings(sema);
    let mut ctx = LowerCtx {
        sema,
        interner,
        module_key: sema.resolved().module_key.clone(),
        module_level_bindings,
        next_lambda_id: 0,
        next_temp_id: 0,
        extra_callables: Vec::new(),
        constraint_answer_bindings: Vec::new(),
        comptime_bindings: HashMap::new(),
        specialized_callables: HashSet::new(),
    };

    let mut items = TopLevelItems::default();
    toplevel::collect_top_level_items(&mut ctx, sema.module().root, false, &mut items);
    items.callables.extend(ctx.extra_callables);
    append_synthesized_sum_data_defs(sema, interner, &mut items);
    let meta = meta::collect_meta(sema);
    let surface = sema.surface();

    Ok(IrModule::new(
        sema.resolved().module_key.clone(),
        surface.static_imports().to_vec().into_boxed_slice(),
        surface.types().to_vec().into_boxed_slice(),
        IrModuleParts {
            exports: surface
                .exported_values()
                .iter()
                .cloned()
                .chain(items.exports)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            callables: items.callables.into_boxed_slice(),
            globals: items.globals.into_boxed_slice(),
            init_parts: items.init_parts.into_boxed_slice(),
            data_defs: items.data_defs.into_boxed_slice(),
            foreigns: items.foreigns.into_boxed_slice(),
            effects: build_effect_defs(sema, ctx.interner),
            shapes: surface
                .exported_shapes()
                .iter()
                .map(IrShapeDef::from)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            givens: surface
                .exported_givens()
                .iter()
                .map(IrGivenDef::from)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            meta,
        },
    ))
}

pub(crate) fn append_synthesized_sum_data_defs(
    sema: &SemaModule,
    interner: &Interner,
    items: &mut TopLevelItems,
) {
    let mut seen_data_keys = HashSet::<DefinitionKey>::new();
    for data_def in &items.data_defs {
        let _ = seen_data_keys.insert(data_def.key.clone());
    }
    for data in sema.data_defs() {
        if seen_data_keys.contains(data.key()) {
            continue;
        }
        let variants = data
            .variants()
            .map(|(name, variant)| {
                IrDataVariantDef::new(
                    name,
                    variant.tag(),
                    variant
                        .field_tys()
                        .iter()
                        .copied()
                        .map(|ty| render_ty_name(sema, ty, interner))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                )
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let mut data_def = IrDataDef::new(data.key().clone(), variants);
        debug_assert_eq!(
            data_def.variant_count,
            u32::try_from(data.variant_count()).unwrap_or(u32::MAX)
        );
        if let Some(repr_kind) = data.repr_kind() {
            data_def = data_def.with_repr_kind(repr_kind);
        }
        if let Some(layout_align) = data.layout_align() {
            data_def = data_def.with_layout_align(layout_align);
        }
        if let Some(layout_pack) = data.layout_pack() {
            data_def = data_def.with_layout_pack(layout_pack);
        }
        if data.frozen() {
            data_def = data_def.with_frozen(true);
        }
        items.data_defs.push(data_def);
    }
}

pub(crate) fn build_effect_defs(sema: &SemaModule, interner: &Interner) -> Box<[IrEffectDef]> {
    let mut seen = BTreeMap::<DefinitionKey, IrEffectDef>::new();
    for effect in sema.effect_defs() {
        let _ = seen.entry(effect.key().clone()).or_insert_with(|| {
            IrEffectDef::new(
                effect.key().clone(),
                effect
                    .ops()
                    .map(|(name, def)| {
                        IrEffectOpDef::new(
                            name,
                            def.params()
                                .iter()
                                .copied()
                                .map(|ty| render_ty_name(sema, ty, interner))
                                .collect::<Vec<_>>()
                                .into_boxed_slice(),
                            render_ty_name(sema, def.result(), interner),
                        )
                        .with_comptime_safe(def.is_comptime_safe())
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            )
        });
    }
    seen.into_values().collect::<Vec<_>>().into_boxed_slice()
}

pub(crate) fn lowering_invariant_violation(description: impl AsRef<str>) -> ! {
    lowering_invariant(
        IrDiagKind::LoweringInvariantViolated,
        DiagContext::new().with("subject", description.as_ref()),
    )
}

pub(crate) fn lowering_invariant(kind: IrDiagKind, context: DiagContext) -> ! {
    resume_unwind(Box::new(LoweringInvariant { kind, context }));
}
