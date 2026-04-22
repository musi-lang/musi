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
use crate::api::{
    IrCallable, IrDataDef, IrDataVariantDef, IrExpr, IrExprKind, IrForeignDef, IrGlobal,
    IrModuleInitPart, IrNameRef, IrOrigin, IrParam, IrRecordField,
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

fn push_global_item(items: &mut TopLevelItems, global: IrGlobal) {
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

#[derive(Clone, Copy)]
struct GivenLetInput<'a> {
    binding: Option<NameBindingId>,
    name: Ident,
    value: HirExprId,
    exported: bool,
    effects: &'a EffectRow,
}

fn collect_given_let_item(
    ctx: &mut LowerCtx<'_>,
    input: GivenLetInput<'_>,
    items: &mut TopLevelItems,
) -> bool {
    let HirExprKind::Given { members, .. } = &ctx.sema.module().store.exprs.get(input.value).kind
    else {
        return false;
    };
    let provider_name = collect_given_item(ctx, input.value, members, input.exported, items);
    let global = lower_given_alias_global(
        ctx,
        GivenAliasGlobalInput {
            binding: input.binding,
            name: input.name,
            value: input.value,
            provider_name,
            exported: input.exported,
            effects: input.effects.clone(),
        },
    );
    push_global_item(items, global);
    true
}

fn collect_given_item(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    members: &SliceRange<HirMemberDef>,
    exported: bool,
    items: &mut TopLevelItems,
) -> Box<str> {
    let Some(given) = ctx.sema.given_facts(expr_id).cloned() else {
        return Box::default();
    };
    let provider_name = given_provider_name(ctx, &given);
    items
        .data_defs
        .push(lower_given_provider_data_def(ctx, &provider_name, &given));
    items.callables.push(lower_given_provider_callable(
        ctx,
        expr_id,
        members,
        &provider_name,
        &given,
    ));
    if exported {
        items.exports.push(lower_given_provider_export(
            ctx.sema,
            provider_name.clone(),
            &given,
        ));
    }
    provider_name
}

struct GivenAliasGlobalInput {
    binding: Option<NameBindingId>,
    name: Ident,
    value: HirExprId,
    provider_name: Box<str>,
    exported: bool,
    effects: EffectRow,
}

fn lower_given_alias_global(ctx: &LowerCtx<'_>, input: GivenAliasGlobalInput) -> IrGlobal {
    let interner = ctx.interner;
    let origin = ctx.sema.module().store.exprs.get(input.value).origin;
    let body = IrExpr::new(
        IrOrigin::new(origin.source_id, origin.span),
        IrExprKind::ClosureNew {
            callee: IrNameRef::new(input.provider_name),
            captures: Box::new([]),
        },
    );
    IrGlobal::new(interner.resolve(input.name.name), body)
        .with_binding_opt(input.binding)
        .with_exported(input.exported)
        .with_effects(input.effects)
}

fn lower_given_provider_callable(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    members: &SliceRange<HirMemberDef>,
    provider_name: &str,
    given: &GivenFacts,
) -> IrCallable {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let member_map = sema
        .module()
        .store
        .members
        .get(members.clone())
        .iter()
        .filter(|member| member.kind == HirMemberKind::Let)
        .map(|member| (member.name.name, member))
        .collect::<BTreeMap<_, _>>();
    let shape_members = sema
        .shape_facts_by_name(given.shape_name)
        .map(|facts| facts.members.to_vec())
        .unwrap_or_default();
    let (params, constraint_answer_bindings) =
        super::hidden_constraint_answer_params_for_keys(provider_name, &given.evidence_keys);
    super::push_constraint_answer_bindings(ctx, constraint_answer_bindings);
    let fields = shape_members
        .into_iter()
        .enumerate()
        .filter_map(|(index, shape_member)| {
            let member = member_map.get(&shape_member.name)?;
            let value = lower_given_member_value(ctx, provider_name, member, index);
            Some(IrRecordField::new(
                interner.resolve(shape_member.name),
                u16::try_from(index).unwrap_or(u16::MAX),
                value,
            ))
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    super::pop_constraint_answer_bindings(ctx);
    let field_count = u16::try_from(fields.len()).unwrap_or(u16::MAX);
    let data_key = given_provider_data_key(provider_name, given);
    IrCallable::new(
        provider_name,
        params.into_boxed_slice(),
        IrExpr::new(
            IrOrigin::new(
                sema.module().store.exprs.get(expr_id).origin.source_id,
                sema.module().store.exprs.get(expr_id).origin.span,
            ),
            IrExprKind::VariantNew {
                data_key,
                tag_index: 0,
                tag_value: 0,
                field_count,
                args: fields.into_iter().map(|field| field.expr).collect(),
            },
        ),
    )
    .with_import_record_target_opt(Some(ctx.module_key.clone()))
}

fn lower_given_member_value(
    ctx: &mut LowerCtx<'_>,
    provider_name: &str,
    member: &HirMemberDef,
    index: usize,
) -> IrExpr {
    let sema = ctx.sema;
    let body_id = member
        .value
        .unwrap_or_else(|| super::invalid_lowering_path("given member value missing"));
    let origin = IrOrigin::new(
        sema.module().store.exprs.get(body_id).origin.source_id,
        sema.module().store.exprs.get(body_id).origin.span,
    );
    let body = super::lower_expr(ctx, body_id);
    super::lower_closure_callable(
        ctx,
        super::ClosureCallableInput {
            origin,
            prefix: "dictfn",
            body_id,
            body,
            hidden_params: Vec::new(),
            hidden_param_names: Vec::new(),
            hidden_capture_exprs: Vec::new(),
            params: lower_user_params(ctx, &member.params),
            binding: None,
            name: Some(format!("{provider_name}::{index}").into_boxed_str()),
            callable_import_record_target: Some(ctx.module_key.clone()),
            rewrite_recursive_self: false,
        },
    )
}

fn lower_given_provider_data_def(
    ctx: &LowerCtx<'_>,
    provider_name: &str,
    given: &GivenFacts,
) -> IrDataDef {
    let field_tys = ctx
        .sema
        .shape_facts_by_name(given.shape_name)
        .map(|facts| facts.members.len())
        .unwrap_or_default();
    IrDataDef::new(
        given_provider_data_key(provider_name, given),
        vec![IrDataVariantDef::new(
            provider_name,
            0,
            repeat_n(Box::<str>::from("Any"), field_tys)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )]
        .into_boxed_slice(),
    )
}

fn lower_given_provider_export(
    sema: &SemaModule,
    provider_name: Box<str>,
    _given: &GivenFacts,
) -> ExportedValue {
    ExportedValue::new(provider_name, find_unit_surface_ty(sema))
        .with_opaque(true)
        .with_import_record_target(sema.resolved().module_key.clone())
}

fn find_unit_surface_ty(sema: &SemaModule) -> SurfaceTyId {
    sema.surface()
        .types()
        .iter()
        .enumerate()
        .find_map(|(index, ty)| matches!(ty.kind, SurfaceTyKind::Unit).then_some(index))
        .and_then(|index| u32::try_from(index).ok())
        .map_or_else(|| SurfaceTyId::new(0), SurfaceTyId::new)
}

fn given_provider_name(ctx: &LowerCtx<'_>, given: &GivenFacts) -> Box<str> {
    let args = given
        .shape_args
        .iter()
        .copied()
        .map(|arg| render_hir_ty_name(ctx.sema, arg, ctx.interner))
        .collect::<Vec<_>>()
        .join(",");
    format!(
        "__dict__::{}::{}[{args}]",
        given.shape_key.module.as_str(),
        given.shape_key.name
    )
    .into_boxed_str()
}

fn given_provider_data_key(provider_name: &str, given: &GivenFacts) -> DefinitionKey {
    DefinitionKey::new(
        given.shape_key.module.clone(),
        format!("{provider_name}::__dict").into_boxed_str(),
    )
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
        .unwrap_or_else(|_| super::invalid_lowering_path("field count overflow"))
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

fn render_hir_ty_name(sema: &SemaModule, ty: HirTyId, interner: &Interner) -> Box<str> {
    let kind = &sema.ty(ty).kind;
    if let HirTyKind::NatLit(value) = kind {
        return value.to_string().into();
    }
    if let Some(name) = simple_hir_ty_display_name(kind) {
        return name.into();
    }
    match kind {
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
        HirTyKind::Seq { .. }
        | HirTyKind::Array { .. }
        | HirTyKind::Range { .. }
        | HirTyKind::Handler { .. }
        | HirTyKind::Pi { .. }
        | HirTyKind::Arrow { .. }
        | HirTyKind::Sum { .. }
        | HirTyKind::Mut { .. }
        | HirTyKind::AnyShape { .. }
        | HirTyKind::SomeShape { .. }
        | HirTyKind::Record { .. } => super::render_ty_name(sema, ty, interner),
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
        | HirTyKind::NatLit(_) => super::invalid_lowering_path("simple type should render"),
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
    let (hidden_params, constraint_answer_bindings) =
        super::hidden_constraint_answer_params_for_binding(
            ctx.sema,
            interner.resolve(input.name.name),
            input.binding,
        );
    super::push_constraint_answer_bindings(ctx, constraint_answer_bindings);
    let body = super::lower_expr(ctx, input.value);
    super::pop_constraint_answer_bindings(ctx);
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
    let mut callable = IrCallable::new(
        interner.resolve(input.name.name),
        params.into_boxed_slice(),
        body,
    )
    .with_exported(input.exported)
    .with_hot(attrs_have_name(ctx.sema, interner, attrs.clone(), "hot"))
    .with_cold(attrs_have_name(ctx.sema, interner, attrs, "cold"))
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
        super::lower_expr(ctx, input.value),
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
    let spec = super::lower_expr(ctx, *arg);
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

pub(super) fn attrs_have_name(
    sema: &SemaModule,
    interner: &Interner,
    attrs: SliceRange<HirAttr>,
    name: &str,
) -> bool {
    sema.module().store.attrs.get(attrs).iter().any(|attr| {
        let parts = sema.module().store.idents.get(attr.path);
        parts.len() == 1 && interner.resolve(parts[0].name) == name
    })
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
                super::decl_binding_id(sema, param.name)
                    .unwrap_or_else(|| super::invalid_lowering_path("param binding missing")),
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
