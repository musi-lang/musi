use super::super::{
    decl_binding_id, hidden_constraint_answer_params_for_binding, lower_expr, lower_foreign_let,
    lowering_invariant_violation, pop_constraint_answer_bindings, push_constraint_answer_bindings,
    render_named_ty_name, render_ty_name,
};
use super::given::{GivenLetInput, collect_given_let_item};
use super::{
    DefinitionKey, EffectRow, HirExprId, HirExprKind, HirParam, HirPatKind, HirTyId, HirTyKind,
    Ident, Interner, IrCallable, IrDataDef, IrDataVariantDef, IrExpr, IrExprKind, IrForeignDef,
    IrGlobal, IrModuleInitPart, IrParam, LetItemInput, LowerCtx, ModuleKey, NameBindingId,
    SemaDataDef, SemaModule, SliceRange, TopLevelItems, profile_attrs, simple_hir_ty_display_name,
};

pub(super) fn collect_let_item(
    ctx: &mut LowerCtx<'_>,
    input: LetItemInput,
    items: &mut TopLevelItems,
) {
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
    let binding = decl_binding_id(sema, name);
    if binding.is_some_and(|binding| sema.is_gated_binding(binding)) {
        return;
    }
    let foreign_input = ForeignLetInput {
        expr_id,
        params: params.clone(),
        value,
        exported,
    };
    if collect_foreign_let_item(sema, interner, foreign_input, name, items) {
        return;
    }
    collect_bound_let_item(
        ctx,
        BoundLetItemInput {
            expr_id,
            binding,
            name,
            params,
            value,
            is_callable,
            exported,
        },
        items,
    );
}

struct BoundLetItemInput {
    expr_id: HirExprId,
    binding: Option<NameBindingId>,
    name: Ident,
    params: SliceRange<HirParam>,
    value: HirExprId,
    is_callable: bool,
    exported: bool,
}

fn collect_bound_let_item(
    ctx: &mut LowerCtx<'_>,
    input: BoundLetItemInput,
    items: &mut TopLevelItems,
) {
    let sema = ctx.sema;
    let BoundLetItemInput {
        expr_id,
        binding,
        name,
        params,
        value,
        is_callable,
        exported,
    } = input;
    let import_record_target = sema.expr_import_record_target(value).cloned().or_else(|| {
        binding.and_then(|binding| sema.binding_import_record_target(binding).cloned())
    });
    let effects = sema
        .try_expr_effects(value)
        .unwrap_or_else(|| lowering_invariant_violation("expr effects missing for top-level value"))
        .clone();
    if let Some(global) = lower_exported_import_global(
        ctx,
        ExportedImportGlobalInput {
            binding,
            name,
            value,
            exported,
            effects: &effects,
            import_record_target: import_record_target.clone(),
        },
    ) {
        push_global_item(items, global);
        return;
    }
    if skip_module_value(sema, value, binding) {
        return;
    }
    if let Some(data_def) = lower_data_item(ctx, name, value) {
        items.data_defs.push(data_def);
        return;
    }
    if matches!(
        sema.module().store.exprs.get(value).kind,
        HirExprKind::Effect { .. } | HirExprKind::Shape { .. }
    ) {
        return;
    }

    if collect_given_let_item(
        ctx,
        GivenLetInput {
            binding,
            name,
            value,
            exported,
            effects: &effects,
        },
        items,
    ) {
        return;
    }
    if collect_callable_let_item(
        ctx,
        CallableLetInput {
            expr_id,
            binding,
            name,
            params: &params,
            value,
            is_callable,
            exported,
            effects: &effects,
            import_record_target: import_record_target.as_ref(),
        },
        items,
    ) {
        return;
    }

    collect_regular_global_item(
        ctx,
        GlobalItemInput {
            binding,
            name,
            value,
            exported,
            effects,
            import_record_target,
        },
        items,
    );
}

struct ForeignLetInput {
    expr_id: HirExprId,
    params: SliceRange<HirParam>,
    value: HirExprId,
    exported: bool,
}

fn collect_foreign_let_item(
    sema: &SemaModule,
    interner: &Interner,
    input: ForeignLetInput,
    name: Ident,
    items: &mut TopLevelItems,
) -> bool {
    let Some(foreign) = lower_foreign_item(
        sema,
        interner,
        input.expr_id,
        name,
        input.params,
        input.exported,
    ) else {
        return false;
    };
    items.foreigns.push(foreign);
    matches!(
        sema.module().store.exprs.get(input.value).kind,
        HirExprKind::Error
    )
}

fn collect_regular_global_item(
    ctx: &mut LowerCtx<'_>,
    input: GlobalItemInput,
    items: &mut TopLevelItems,
) {
    let global = lower_global_item(ctx, input);
    push_global_item(items, global);
}

pub(super) fn push_global_item(items: &mut TopLevelItems, global: IrGlobal) {
    items
        .init_parts
        .push(IrModuleInitPart::global(global.name.clone()));
    items.globals.push(global);
}

#[derive(Clone, Copy)]
struct CallableLetInput<'a> {
    expr_id: HirExprId,
    binding: Option<NameBindingId>,
    name: Ident,
    params: &'a SliceRange<HirParam>,
    value: HirExprId,
    is_callable: bool,
    exported: bool,
    effects: &'a EffectRow,
    import_record_target: Option<&'a ModuleKey>,
}

fn collect_callable_let_item(
    ctx: &mut LowerCtx<'_>,
    input: CallableLetInput<'_>,
    items: &mut TopLevelItems,
) -> bool {
    if !input.is_callable {
        return false;
    }
    if has_comptime_params(ctx, input.params.clone()) {
        return true;
    }
    items.callables.push(lower_callable_item(
        ctx,
        CallableItemInput {
            expr_id: input.expr_id,
            binding: input.binding,
            name: input.name,
            params: input.params.clone(),
            value: input.value,
            exported: input.exported,
            effects: input.effects.clone(),
            import_record_target: input.import_record_target.cloned(),
        },
    ));
    true
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
        .native
        .is_some()
        .then(|| lower_foreign_let(sema, interner, expr_id, name, params, exported))
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
    let repr_kind: Option<Box<str>> = def.and_then(|data| data.repr_kind().map(Into::into));
    let layout_align = def.and_then(SemaDataDef::layout_align);
    let layout_pack = def.and_then(SemaDataDef::layout_pack);
    let frozen = def.is_some_and(SemaDataDef::frozen);
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
            vec![IrDataVariantDef::new(
                name_text.clone(),
                0,
                field_tys.into_boxed_slice(),
            )]
            .into_boxed_slice()
        },
        |data| {
            data.variants()
                .map(|(variant_name, variant)| {
                    IrDataVariantDef::new(
                        variant_name,
                        variant.tag(),
                        variant
                            .field_tys()
                            .iter()
                            .copied()
                            .map(|ty| render_hir_ty_name(sema, ty, interner))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                    )
                })
                .collect::<Vec<_>>()
                .into_boxed_slice()
        },
    );
    let mut data_def = IrDataDef::new(key, variants);
    debug_assert_eq!(
        data_def.field_count,
        u32::try_from(
            data_def
                .variants
                .iter()
                .map(|variant| variant.field_tys.len())
                .max()
                .unwrap_or_else(|| sema.module().store.fields.get(fields.clone()).len())
        )
        .unwrap_or_else(|_| lowering_invariant_violation("field count overflow"))
    );
    if let Some(repr_kind) = repr_kind {
        data_def = data_def.with_repr_kind(repr_kind);
    }
    if let Some(layout_align) = layout_align {
        data_def = data_def.with_layout_align(layout_align);
    }
    if let Some(layout_pack) = layout_pack {
        data_def = data_def.with_layout_pack(layout_pack);
    }
    if frozen {
        data_def = data_def.with_frozen(true);
    }
    Some(data_def)
}

pub(super) fn render_hir_ty_name(sema: &SemaModule, ty: HirTyId, interner: &Interner) -> Box<str> {
    let kind = &sema.ty(ty).kind;
    if let HirTyKind::NatLit(value) = kind {
        return value.to_string().into();
    }
    if let Some(name) = simple_hir_ty_display_name(kind) {
        return name.into();
    }
    match kind {
        HirTyKind::Named { name, args } => render_named_ty_name(sema, *name, *args, interner),
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
        HirTyKind::Seq { .. }
        | HirTyKind::Array { .. }
        | HirTyKind::Bits { .. }
        | HirTyKind::Range { .. }
        | HirTyKind::Handler { .. }
        | HirTyKind::Pi { .. }
        | HirTyKind::Arrow { .. }
        | HirTyKind::Sum { .. }
        | HirTyKind::Mut { .. }
        | HirTyKind::AnyShape { .. }
        | HirTyKind::SomeShape { .. }
        | HirTyKind::Record { .. } => render_ty_name(sema, ty, interner),
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
        | HirTyKind::Int8
        | HirTyKind::Int16
        | HirTyKind::Int32
        | HirTyKind::Int64
        | HirTyKind::Nat8
        | HirTyKind::Nat16
        | HirTyKind::Nat32
        | HirTyKind::Nat64
        | HirTyKind::Float
        | HirTyKind::Float32
        | HirTyKind::Float64
        | HirTyKind::String
        | HirTyKind::Rune
        | HirTyKind::CString
        | HirTyKind::CPtr
        | HirTyKind::NatLit(_) => lowering_invariant_violation("simple type should render"),
    }
}

struct CallableItemInput {
    expr_id: HirExprId,
    binding: Option<NameBindingId>,
    name: Ident,
    params: SliceRange<HirParam>,
    value: HirExprId,
    exported: bool,
    effects: EffectRow,
    import_record_target: Option<ModuleKey>,
}

fn lower_callable_item(ctx: &mut LowerCtx<'_>, input: CallableItemInput) -> IrCallable {
    let interner = ctx.interner;
    let (hidden_params, constraint_answer_bindings) = hidden_constraint_answer_params_for_binding(
        ctx.sema,
        interner.resolve(input.name.name),
        input.binding,
    );
    push_constraint_answer_bindings(ctx, constraint_answer_bindings);
    let body = lower_expr(ctx, input.value);
    pop_constraint_answer_bindings(ctx);
    let mut params = hidden_params;
    params.extend(lower_params(ctx, input.params));
    let attrs = ctx
        .sema
        .module()
        .store
        .exprs
        .get(input.expr_id)
        .mods
        .attrs
        .clone();
    let profile = profile_attrs(ctx.sema, ctx.interner, attrs);
    let mut callable = IrCallable::new(
        interner.resolve(input.name.name),
        params.into_boxed_slice(),
        body,
    )
    .with_exported(input.exported)
    .with_hot(profile.hot)
    .with_cold(profile.cold)
    .with_effects(input.effects);
    if let Some(binding) = input.binding {
        callable = callable.with_binding(binding);
    }
    if let Some(import_record_target) = input.import_record_target {
        callable = callable.with_import_record_target(import_record_target);
    }
    callable
}

struct GlobalItemInput {
    binding: Option<NameBindingId>,
    name: Ident,
    value: HirExprId,
    exported: bool,
    effects: EffectRow,
    import_record_target: Option<ModuleKey>,
}

fn lower_global_item(ctx: &mut LowerCtx<'_>, input: GlobalItemInput) -> IrGlobal {
    let interner = ctx.interner;
    let mut global = IrGlobal::new(
        interner.resolve(input.name.name),
        lower_expr(ctx, input.value),
    )
    .with_exported(input.exported)
    .with_effects(input.effects);
    if let Some(binding) = input.binding {
        global = global.with_binding(binding);
    }
    if let Some(import_record_target) = input.import_record_target {
        global = global.with_import_record_target(import_record_target);
    }
    global
}

struct ExportedImportGlobalInput<'a> {
    binding: Option<NameBindingId>,
    name: Ident,
    value: HirExprId,
    exported: bool,
    effects: &'a EffectRow,
    import_record_target: Option<ModuleKey>,
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
    let spec = lower_expr(ctx, *arg);
    Some(
        IrGlobal::new(
            interner.resolve(input.name.name),
            IrExpr::new(
                spec.origin,
                IrExprKind::ModuleLoad {
                    spec: Box::new(spec),
                },
            ),
        )
        .with_binding_opt(input.binding)
        .with_exported(true)
        .with_effects(input.effects.clone())
        .with_import_record_target_opt(input.import_record_target),
    )
}

fn skip_module_value(sema: &SemaModule, value: HirExprId, binding: Option<NameBindingId>) -> bool {
    sema.expr_import_record_target(value).is_some()
        || binding.is_some_and(|binding| sema.binding_import_record_target(binding).is_some())
}

fn lower_params(ctx: &LowerCtx<'_>, params: SliceRange<HirParam>) -> Box<[IrParam]> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    sema.module()
        .store
        .params
        .get(params)
        .iter()
        .filter(|param| !param.is_comptime)
        .map(|param| {
            IrParam::new(
                decl_binding_id(sema, param.name)
                    .unwrap_or_else(|| lowering_invariant_violation("param binding missing")),
                interner.resolve(param.name.name),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn has_comptime_params(ctx: &LowerCtx<'_>, params: SliceRange<HirParam>) -> bool {
    ctx.sema
        .module()
        .store
        .params
        .get(params)
        .iter()
        .any(|param| param.is_comptime)
}
