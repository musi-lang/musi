use super::super::{
    ClosureCallableInput, hidden_constraint_answer_params_for_keys, lower_closure_callable,
    lower_expr, lowering_invariant_violation, pop_constraint_answer_bindings,
    push_constraint_answer_bindings,
};
use super::items::{push_global_item, render_hir_ty_name};
use super::{
    BTreeMap, DefinitionKey, EffectRow, ExportedValue, GivenFacts, HirExprId, HirExprKind,
    HirMemberDef, HirMemberKind, Ident, IrCallable, IrDataDef, IrDataVariantDef, IrExpr,
    IrExprKind, IrGlobal, IrNameRef, IrOrigin, IrRecordField, LowerCtx, NameBindingId, SemaModule,
    SliceRange, SurfaceTyId, SurfaceTyKind, TopLevelItems, lower_user_params, repeat_n,
};

#[derive(Clone, Copy)]
pub(super) struct GivenLetInput<'a> {
    pub(super) binding: Option<NameBindingId>,
    pub(super) name: Ident,
    pub(super) value: HirExprId,
    pub(super) exported: bool,
    pub(super) effects: &'a EffectRow,
}

pub(super) fn collect_given_let_item(
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

pub(super) fn collect_given_item(
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
        hidden_constraint_answer_params_for_keys(provider_name, &given.evidence_keys);
    push_constraint_answer_bindings(ctx, constraint_answer_bindings);
    let fields = shape_members
        .into_iter()
        .enumerate()
        .filter_map(|(index, shape_member)| {
            let member = member_map.get(&shape_member.name)?;
            let field_value = lower_given_member_value(ctx, provider_name, member, index);
            Some(IrRecordField::new(
                interner.resolve(shape_member.name),
                u16::try_from(index).unwrap_or(u16::MAX),
                field_value,
            ))
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    pop_constraint_answer_bindings(ctx);
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
        .unwrap_or_else(|| lowering_invariant_violation("given member value missing"));
    let origin = IrOrigin::new(
        sema.module().store.exprs.get(body_id).origin.source_id,
        sema.module().store.exprs.get(body_id).origin.span,
    );
    let body = lower_expr(ctx, body_id);
    lower_closure_callable(
        ctx,
        ClosureCallableInput {
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
