use music_arena::SliceRange;
use music_base::Span;
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirCaseArm, HirExprId, HirExprKind, HirFieldDef,
    HirHandleClause, HirLitKind, HirMemberDef, HirParam, HirPatId, HirPatKind, HirRecordItem,
    HirTemplatePart, HirVariantDef,
};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId, NameSite, Symbol};

use super::surface_types::{SurfaceTyBuilder, lower_surface_effect_row};
use super::{DeclState, ModuleState, TypingState};
use crate::api::{
    Attr, AttrArg, AttrRecordField, AttrValue, ClassMemberSurface, ClassSurface, ConstraintFacts,
    ConstraintSurface, DataSurface, DataVariantSurface, EffectOpSurface, EffectSurface,
    ExportedValue, InstanceSurface, SurfaceEffectRow,
};

#[derive(Debug, Clone)]
pub(super) struct ExportBinding {
    pub(super) binding: NameBindingId,
    pub(super) name: Box<str>,
    pub(super) opaque: bool,
    pub(super) attrs: Box<[HirAttr]>,
}

#[derive(Debug, Clone)]
pub(super) struct ExportInstance {
    pub(super) span: Span,
    pub(super) attrs: Box<[HirAttr]>,
}

#[derive(Debug, Default)]
pub(super) struct ModuleExports {
    pub(super) bindings: Vec<ExportBinding>,
    pub(super) instances: Vec<ExportInstance>,
}

fn attr_is_musi(path: &[Box<str>]) -> bool {
    path.first().is_some_and(|seg| seg.as_ref() == "musi")
}

fn attr_is_reserved(path: &[Box<str>]) -> bool {
    match path {
        [head] if head.as_ref() == "link" => true,
        [head] if head.as_ref() == "when" => true,
        [head] if head.as_ref() == "repr" => true,
        [head] if head.as_ref() == "layout" => true,
        [head, ..] if head.as_ref() == "diag" => true,
        _ => false,
    }
}

fn lower_attr_value(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
) -> Option<AttrValue> {
    match module.resolved.module.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Lit { lit } => match module.resolved.module.store.lits.get(lit).kind.clone() {
            HirLitKind::String { value } => Some(AttrValue::String(value)),
            HirLitKind::Int { raw } => Some(AttrValue::Int(raw)),
            HirLitKind::Rune { value } => Some(AttrValue::Rune(value)),
            HirLitKind::Float { .. } => None,
        },
        HirExprKind::Variant { tag, args } => {
            let tag: Box<str> = interner.resolve(tag.name).into();
            let mut lowered = Vec::new();
            for arg in module
                .resolved
                .module
                .store
                .expr_ids
                .get(args)
                .iter()
                .copied()
            {
                lowered.push(lower_attr_value(module, interner, arg)?);
            }
            Some(AttrValue::Variant {
                tag,
                args: lowered.into_boxed_slice(),
            })
        }
        HirExprKind::Array { items } => {
            let mut lowered = Vec::new();
            for item in module.resolved.module.store.array_items.get(items) {
                if item.spread {
                    return None;
                }
                lowered.push(lower_attr_value(module, interner, item.expr)?);
            }
            Some(AttrValue::Array {
                items: lowered.into_boxed_slice(),
            })
        }
        HirExprKind::Record { items } => {
            let mut fields = Vec::new();
            for item in module.resolved.module.store.record_items.get(items) {
                if item.spread {
                    return None;
                }
                let name = item.name?;
                let value = lower_attr_value(module, interner, item.value)?;
                fields.push(AttrRecordField {
                    name: interner.resolve(name.name).into(),
                    value,
                });
            }
            Some(AttrValue::Record {
                fields: fields.into_boxed_slice(),
            })
        }
        _ => None,
    }
}

fn lower_attr(module: &ModuleState, interner: &Interner, attr: &HirAttr) -> Option<Attr> {
    let path = module
        .resolved
        .module
        .store
        .idents
        .get(attr.path)
        .iter()
        .map(|ident| interner.resolve(ident.name).into())
        .collect::<Vec<Box<str>>>()
        .into_boxed_slice();
    let mut args = Vec::<AttrArg>::new();
    for arg in module
        .resolved
        .module
        .store
        .attr_args
        .get(attr.args.clone())
    {
        let name = arg.name.map(|ident| interner.resolve(ident.name).into());
        let value = lower_attr_value(module, interner, arg.value)?;
        args.push(AttrArg { name, value });
    }
    Some(Attr {
        path,
        args: args.into_boxed_slice(),
    })
}

fn split_export_attrs(
    module: &ModuleState,
    interner: &Interner,
    attrs: &[HirAttr],
) -> (Box<[Attr]>, Box<[Attr]>) {
    let mut inert = Vec::new();
    let mut musi = Vec::new();
    for attr in attrs {
        let Some(lowered) = lower_attr(module, interner, attr) else {
            continue;
        };
        if attr_is_reserved(&lowered.path) {
            continue;
        }
        if attr_is_musi(&lowered.path) {
            musi.push(lowered);
        } else {
            inert.push(lowered);
        }
    }
    (inert.into_boxed_slice(), musi.into_boxed_slice())
}

fn lower_type_params(symbols: &[Symbol], interner: &Interner) -> Box<[Box<str>]> {
    symbols
        .iter()
        .map(|symbol| interner.resolve(*symbol).into())
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn lower_constraints(
    constraints: &[ConstraintFacts],
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[ConstraintSurface]> {
    constraints
        .iter()
        .map(|constraint| ConstraintSurface {
            name: tys.interner.resolve(constraint.name).into(),
            kind: constraint.kind,
            value: tys.lower(constraint.value),
            class_key: constraint.class_key.clone(),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn collect_binding_exports<T, F>(
    module: &ModuleState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
    mut lower: F,
) -> Box<[T]>
where
    F: FnMut(&ExportBinding, Box<[Attr]>, Box<[Attr]>, &mut SurfaceTyBuilder<'_>) -> Option<T>,
{
    exports
        .bindings
        .iter()
        .filter_map(|export| {
            let (inert_attrs, musi_attrs) = split_export_attrs(module, tys.interner, &export.attrs);
            lower(export, inert_attrs, musi_attrs, tys)
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn collect_exported_values(
    module: &ModuleState,
    typing: &TypingState,
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[ExportedValue]> {
    collect_binding_exports(
        module,
        exports,
        tys,
        |export, inert_attrs, musi_attrs, tys| {
            if typing.is_gated_binding(export.binding) {
                return None;
            }
            let ty = typing.binding_types().get(&export.binding).copied()?;
            let symbol = module.resolved.names.bindings.get(export.binding).name;
            let scheme = typing.binding_schemes().get(&export.binding);
            Some(ExportedValue {
                name: export.name.clone(),
                ty: tys.lower(ty),
                type_params: scheme.map_or_else(Box::<[Box<str>]>::default, |scheme| {
                    lower_type_params(&scheme.type_params, tys.interner)
                }),
                constraints: scheme.map_or_else(Box::<[ConstraintSurface]>::default, |scheme| {
                    lower_constraints(&scheme.constraints, tys)
                }),
                effects: scheme.map_or_else(SurfaceEffectRow::default, |scheme| {
                    lower_surface_effect_row(tys, &scheme.effects)
                }),
                opaque: export.opaque,
                module_target: export_module_target(module, typing, export.binding),
                class_key: decls
                    .class_facts_by_name()
                    .get(&symbol)
                    .map(|facts| facts.key.clone()),
                effect_key: decls
                    .effect_def(export.name.as_ref())
                    .map(|effect| effect.key().clone()),
                data_key: decls
                    .data_def(export.name.as_ref())
                    .map(|data| data.key().clone()),
                inert_attrs,
                musi_attrs,
            })
        },
    )
}

fn export_module_target(
    module: &ModuleState,
    typing: &TypingState,
    binding: NameBindingId,
) -> Option<ModuleKey> {
    typing
        .binding_module_targets()
        .get(&binding)
        .cloned()
        .or_else(|| {
            binding_value_expr(module, module.resolved.module.root, binding)
                .and_then(|expr| expr_module_target(module, typing, expr))
        })
}

fn binding_value_expr(
    module: &ModuleState,
    expr_id: HirExprId,
    binding: NameBindingId,
) -> Option<HirExprId> {
    match module.resolved.module.store.exprs.get(expr_id).kind {
        HirExprKind::Sequence { exprs } => module
            .resolved
            .module
            .store
            .expr_ids
            .get(exprs)
            .iter()
            .copied()
            .find_map(|expr| binding_value_expr(module, expr, binding)),
        HirExprKind::Let { pat, value, .. } => pat_binds(module, pat, binding)
            .then_some(value)
            .or_else(|| binding_value_expr(module, value, binding)),
        _ => None,
    }
}

fn pat_binds(module: &ModuleState, pat_id: HirPatId, binding: NameBindingId) -> bool {
    match module.resolved.module.store.pats.get(pat_id).kind.clone() {
        HirPatKind::Bind { name } => {
            let site = NameSite::new(module.resolved.module.source_id, name.span);
            module.binding_id_at_site(site).is_some_and(|found| found == binding)
        }
        HirPatKind::Tuple { items }
        | HirPatKind::Array { items }
        | HirPatKind::Variant { args: items, .. } => module
            .resolved
            .module
            .store
            .pat_ids
            .get(items)
            .iter()
            .copied()
            .any(|item| pat_binds(module, item, binding)),
        HirPatKind::Record { fields } => module
            .resolved
            .module
            .store
            .record_pat_fields
            .get(fields)
            .iter()
            .any(|field| {
                field.value.map_or_else(
                    || {
                        let site = NameSite::new(module.resolved.module.source_id, field.name.span);
                        module.binding_id_at_site(site).is_some_and(|found| found == binding)
                    },
                    |value| pat_binds(module, value, binding),
                )
            }),
        HirPatKind::Or { left, right } => {
            pat_binds(module, left, binding) || pat_binds(module, right, binding)
        }
        HirPatKind::As { pat, name } => {
            pat_binds(module, pat, binding) || {
                let site = NameSite::new(module.resolved.module.source_id, name.span);
                module.binding_id_at_site(site).is_some_and(|found| found == binding)
            }
        }
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Lit { .. } => false,
    }
}

fn expr_module_target(
    module: &ModuleState,
    typing: &TypingState,
    expr_id: HirExprId,
) -> Option<ModuleKey> {
    match module.resolved.module.store.exprs.get(expr_id).kind {
        HirExprKind::Import { .. } => {
            let span = module.resolved.module.store.exprs.get(expr_id).origin.span;
            module
                .resolved
                .imports
                .iter()
                .find_map(|import| (import.span == span).then_some(import.to.clone()))
        }
        HirExprKind::Name { name } => {
            let site = NameSite::new(module.resolved.module.source_id, name.span);
            let binding = module.resolved.names.refs.get(&site).copied()?;
            typing.binding_module_targets().get(&binding).cloned()
        }
        _ => None,
    }
}

pub(super) fn collect_exported_data(
    module: &ModuleState,
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[DataSurface]> {
    collect_binding_exports(
        module,
        exports,
        tys,
        |export, inert_attrs, musi_attrs, tys| {
            let data = decls.data_def(export.name.as_ref())?;
            Some(DataSurface {
                key: data.key().clone(),
                variants: export
                    .opaque
                    .then(Box::<[DataVariantSurface]>::default)
                    .unwrap_or_else(|| {
                        data.variants()
                            .map(|(name, variant)| DataVariantSurface {
                                name: name.into(),
                                payload: variant.payload().map(|ty| tys.lower(ty)),
                            })
                            .collect::<Vec<_>>()
                            .into_boxed_slice()
                    }),
                repr_kind: data.repr_kind().map(Into::into),
                layout_align: data.layout_align(),
                layout_pack: data.layout_pack(),
                inert_attrs,
                musi_attrs,
            })
        },
    )
}

pub(super) fn collect_exported_classes(
    module: &ModuleState,
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[ClassSurface]> {
    collect_binding_exports(
        module,
        exports,
        tys,
        |export, inert_attrs, musi_attrs, tys| {
            let symbol = module.resolved.names.bindings.get(export.binding).name;
            let facts = decls.class_facts_by_name().get(&symbol)?;
            Some(ClassSurface {
                key: facts.key.clone(),
                constraints: lower_constraints(&facts.constraints, tys),
                members: facts
                    .members
                    .iter()
                    .map(|member| ClassMemberSurface {
                        name: tys.interner.resolve(member.name).into(),
                        params: member
                            .params
                            .iter()
                            .copied()
                            .map(|ty| tys.lower(ty))
                            .collect::<Vec<_>>()
                            .into_boxed_slice(),
                        result: tys.lower(member.result),
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                laws: facts
                    .laws
                    .iter()
                    .map(|law| tys.interner.resolve(*law).into())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                inert_attrs,
                musi_attrs,
            })
        },
    )
}

pub(super) fn collect_exported_effects(
    module: &ModuleState,
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[EffectSurface]> {
    collect_binding_exports(
        module,
        exports,
        tys,
        |export, inert_attrs, musi_attrs, tys| {
            let effect = decls.effect_def(export.name.as_ref())?;
            Some(EffectSurface {
                key: effect.key().clone(),
                ops: export
                    .opaque
                    .then(Box::<[EffectOpSurface]>::default)
                    .unwrap_or_else(|| {
                        effect
                            .ops()
                            .map(|(name, op)| EffectOpSurface {
                                name: name.into(),
                                params: op
                                    .params()
                                    .iter()
                                    .copied()
                                    .map(|ty| tys.lower(ty))
                                    .collect::<Vec<_>>()
                                    .into_boxed_slice(),
                                result: tys.lower(op.result()),
                            })
                            .collect::<Vec<_>>()
                            .into_boxed_slice()
                    }),
                laws: export
                    .opaque
                    .then(Box::<[Box<str>]>::default)
                    .unwrap_or_else(|| {
                        effect
                            .laws()
                            .iter()
                            .map(|symbol| tys.interner.resolve(*symbol).into())
                            .collect::<Vec<_>>()
                            .into_boxed_slice()
                    }),
                inert_attrs,
                musi_attrs,
            })
        },
    )
}

pub(super) fn collect_exported_instances(
    module: &ModuleState,
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[InstanceSurface]> {
    decls
        .instance_facts()
        .values()
        .filter_map(|facts| {
            let export = exports
                .instances
                .iter()
                .find(|export| export.span == facts.origin.span)?;
            let (inert_attrs, musi_attrs) = split_export_attrs(module, tys.interner, &export.attrs);
            Some(InstanceSurface {
                type_params: lower_type_params(&facts.type_params, tys.interner),
                class_key: facts.class_key.clone(),
                class_args: facts
                    .class_args
                    .iter()
                    .copied()
                    .map(|ty| tys.lower(ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                constraints: lower_constraints(&facts.constraints, tys),
                member_names: lower_type_params(&facts.member_names, tys.interner),
                inert_attrs,
                musi_attrs,
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn collect_module_exports(module: &ModuleState, interner: &Interner) -> ModuleExports {
    let mut exports = ModuleExports::default();
    let mut attr_stack = Vec::<HirAttr>::new();
    collect_expr(
        module,
        interner,
        module.resolved.module.root,
        &mut exports,
        &mut attr_stack,
    );
    exports
}

fn collect_expr(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    let expr = module.resolved.module.store.exprs.get(expr_id);
    let start = attr_stack.len();
    if !expr.mods.attrs.is_empty() {
        attr_stack.extend_from_slice(
            module
                .resolved
                .module
                .store
                .attrs
                .get(expr.mods.attrs.clone()),
        );
    }
    if let Some(export_mod) = &expr.mods.export {
        collect_direct_exports(
            module,
            interner,
            expr_id,
            export_mod.opaque,
            exports,
            attr_stack,
        );
    }

    collect_exports_from_kind(module, interner, &expr.kind, exports, attr_stack);
    attr_stack.truncate(start);
}

fn collect_exports_from_kind(
    module: &ModuleState,
    interner: &Interner,
    kind: &HirExprKind,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    match kind {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            collect_expr_id_range(module, interner, *exprs, exports, attr_stack);
        }
        HirExprKind::Array { items } => {
            collect_array_exprs(module, interner, items, exports, attr_stack);
        }
        HirExprKind::Record { items } => {
            collect_recordish_exprs(module, interner, items, None, exports, attr_stack);
        }
        HirExprKind::RecordUpdate { base, items } => {
            collect_recordish_exprs(module, interner, items, Some(*base), exports, attr_stack);
        }
        HirExprKind::Template { parts } => {
            collect_template_exprs(module, interner, parts, exports, attr_stack);
        }
        HirExprKind::Pi { binder_ty, ret, .. } => {
            collect_pair_exprs(module, interner, *binder_ty, *ret, exports, attr_stack);
        }
        HirExprKind::Lambda { body, .. }
        | HirExprKind::Import { arg: body }
        | HirExprKind::Perform { expr: body } => {
            collect_expr(module, interner, *body, exports, attr_stack);
        }
        HirExprKind::Call { callee, args } => {
            collect_expr(module, interner, *callee, exports, attr_stack);
            collect_arg_exprs(module, interner, args, exports, attr_stack);
        }
        HirExprKind::Apply { callee, args } | HirExprKind::Index { base: callee, args } => {
            collect_expr(module, interner, *callee, exports, attr_stack);
            collect_expr_id_range(module, interner, *args, exports, attr_stack);
        }
        HirExprKind::Field { base, .. }
        | HirExprKind::TypeTest { base, .. }
        | HirExprKind::TypeCast { base, .. }
        | HirExprKind::Prefix { expr: base, .. } => {
            collect_expr(module, interner, *base, exports, attr_stack);
        }
        HirExprKind::Binary { left, right, .. } => {
            collect_pair_exprs(module, interner, *left, *right, exports, attr_stack);
        }
        HirExprKind::Let { value, .. } => {
            collect_expr(module, interner, *value, exports, attr_stack);
        }
        HirExprKind::Case { scrutinee, arms } => {
            collect_case_exports(module, interner, *scrutinee, arms, exports, attr_stack);
        }
        HirExprKind::Data { variants, fields } => {
            collect_data_exports(module, interner, variants, fields, exports, attr_stack);
        }
        HirExprKind::Effect { members } | HirExprKind::Class { members, .. } => {
            collect_member_exports(module, interner, members, exports, attr_stack);
        }
        HirExprKind::Instance { class, members, .. } => {
            collect_expr(module, interner, *class, exports, attr_stack);
            collect_member_exports(module, interner, members, exports, attr_stack);
        }
        HirExprKind::Handle { expr, clauses, .. } => {
            collect_handle_exports(module, interner, *expr, clauses, exports, attr_stack);
        }
        HirExprKind::Resume { expr } => {
            collect_optional_expr(module, interner, *expr, exports, attr_stack);
        }
        HirExprKind::Quote { .. }
        | HirExprKind::Splice { .. }
        | HirExprKind::Error
        | HirExprKind::Name { .. }
        | HirExprKind::Lit { .. }
        | HirExprKind::ArrayTy { .. }
        | HirExprKind::Variant { .. } => {}
    }
}

fn collect_exprs<I>(
    module: &ModuleState,
    interner: &Interner,
    exprs: I,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) where
    I: IntoIterator<Item = HirExprId>,
{
    for expr_id in exprs {
        collect_expr(module, interner, expr_id, exports, attr_stack);
    }
}

fn collect_optional_expr(
    module: &ModuleState,
    interner: &Interner,
    expr_id: Option<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(module, interner, expr_id, exports, attr_stack);
}

fn collect_expr_id_range(
    module: &ModuleState,
    interner: &Interner,
    exprs: SliceRange<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .expr_ids
            .get(exprs)
            .iter()
            .copied(),
        exports,
        attr_stack,
    );
}

fn collect_pair_exprs(
    module: &ModuleState,
    interner: &Interner,
    left: HirExprId,
    right: HirExprId,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(module, interner, [left, right], exports, attr_stack);
}

fn collect_array_exprs(
    module: &ModuleState,
    interner: &Interner,
    items: &SliceRange<HirArrayItem>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .array_items
            .get(items.clone())
            .iter()
            .map(|item| item.expr),
        exports,
        attr_stack,
    );
}

fn collect_arg_exprs(
    module: &ModuleState,
    interner: &Interner,
    args: &SliceRange<HirArg>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .args
            .get(args.clone())
            .iter()
            .map(|arg| arg.expr),
        exports,
        attr_stack,
    );
}

fn collect_template_exprs(
    module: &ModuleState,
    interner: &Interner,
    parts: &SliceRange<HirTemplatePart>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .template_parts
            .get(parts.clone())
            .iter()
            .filter_map(|part| match part {
                HirTemplatePart::Expr { expr } => Some(*expr),
                HirTemplatePart::Text { .. } => None,
            }),
        exports,
        attr_stack,
    );
}

fn collect_record_item_exports(
    module: &ModuleState,
    interner: &Interner,
    items: &SliceRange<HirRecordItem>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for item in module.resolved.module.store.record_items.get(items.clone()) {
        collect_expr(module, interner, item.value, exports, attr_stack);
    }
}

fn collect_recordish_exprs(
    module: &ModuleState,
    interner: &Interner,
    items: &SliceRange<HirRecordItem>,
    base: Option<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_record_item_exports(module, interner, items, exports, attr_stack);
    collect_optional_expr(module, interner, base, exports, attr_stack);
}

fn collect_case_exports(
    module: &ModuleState,
    interner: &Interner,
    scrutinee: HirExprId,
    arms: &SliceRange<HirCaseArm>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_expr(module, interner, scrutinee, exports, attr_stack);
    for arm in module.resolved.module.store.case_arms.get(arms.clone()) {
        collect_optional_expr(module, interner, arm.guard, exports, attr_stack);
        collect_expr(module, interner, arm.expr, exports, attr_stack);
    }
}

fn collect_data_exports(
    module: &ModuleState,
    interner: &Interner,
    variants: &SliceRange<HirVariantDef>,
    fields: &SliceRange<HirFieldDef>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for variant in module.resolved.module.store.variants.get(variants.clone()) {
        collect_optional_expr(module, interner, variant.arg, exports, attr_stack);
        collect_optional_expr(module, interner, variant.value, exports, attr_stack);
    }
    for field in module.resolved.module.store.fields.get(fields.clone()) {
        collect_expr(module, interner, field.ty, exports, attr_stack);
        collect_optional_expr(module, interner, field.value, exports, attr_stack);
    }
}

fn collect_param_exports(
    module: &ModuleState,
    interner: &Interner,
    params: &SliceRange<HirParam>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for param in module.resolved.module.store.params.get(params.clone()) {
        collect_optional_expr(module, interner, param.ty, exports, attr_stack);
        collect_optional_expr(module, interner, param.default, exports, attr_stack);
    }
}

fn collect_member_exports(
    module: &ModuleState,
    interner: &Interner,
    members: &SliceRange<HirMemberDef>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for member in module.resolved.module.store.members.get(members.clone()) {
        collect_param_exports(module, interner, &member.params, exports, attr_stack);
        collect_optional_expr(module, interner, member.sig, exports, attr_stack);
        collect_optional_expr(module, interner, member.value, exports, attr_stack);
    }
}

fn collect_handle_exports(
    module: &ModuleState,
    interner: &Interner,
    expr: HirExprId,
    clauses: &SliceRange<HirHandleClause>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_expr(module, interner, expr, exports, attr_stack);
    for clause in module
        .resolved
        .module
        .store
        .handle_clauses
        .get(clauses.clone())
    {
        collect_expr(module, interner, clause.body, exports, attr_stack);
    }
}

fn collect_direct_exports(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    match &module.resolved.module.store.exprs.get(expr_id).kind {
        HirExprKind::Let { pat, .. } => {
            collect_export_bindings_from_pat(module, interner, *pat, opaque, exports, attr_stack);
        }
        HirExprKind::Instance { .. } => {
            let span = module.resolved.module.store.exprs.get(expr_id).origin.span;
            if opaque {
                return;
            }
            if exports.instances.iter().any(|export| export.span == span) {
                return;
            }
            exports.instances.push(ExportInstance {
                span,
                attrs: attr_stack.to_vec().into_boxed_slice(),
            });
        }
        _ => {}
    }
}

fn collect_export_bindings_from_pat(
    module: &ModuleState,
    interner: &Interner,
    pat_id: HirPatId,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    match module.resolved.module.store.pats.get(pat_id).kind.clone() {
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Lit { .. } => {}
        HirPatKind::Bind { name } => {
            push_export_binding(module, interner, name, opaque, exports, attr_stack);
        }
        HirPatKind::Tuple { items }
        | HirPatKind::Array { items }
        | HirPatKind::Variant { args: items, .. } => {
            collect_pat_id_range(module, interner, items, opaque, exports, attr_stack);
        }
        HirPatKind::Record { fields } => {
            for field in module.resolved.module.store.record_pat_fields.get(fields) {
                if let Some(value) = field.value {
                    collect_export_bindings_from_pat(
                        module, interner, value, opaque, exports, attr_stack,
                    );
                } else {
                    push_export_binding(module, interner, field.name, opaque, exports, attr_stack);
                }
            }
        }
        HirPatKind::Or { left, right } => {
            collect_export_bindings_from_pat(module, interner, left, opaque, exports, attr_stack);
            collect_export_bindings_from_pat(module, interner, right, opaque, exports, attr_stack);
        }
        HirPatKind::As { pat, name } => {
            collect_export_bindings_from_pat(module, interner, pat, opaque, exports, attr_stack);
            push_export_binding(module, interner, name, opaque, exports, attr_stack);
        }
    }
}

fn collect_pat_id_range(
    module: &ModuleState,
    interner: &Interner,
    items: SliceRange<HirPatId>,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    for item in module
        .resolved
        .module
        .store
        .pat_ids
        .get(items)
        .iter()
        .copied()
    {
        collect_export_bindings_from_pat(module, interner, item, opaque, exports, attr_stack);
    }
}

fn push_export_binding(
    module: &ModuleState,
    interner: &Interner,
    name: Ident,
    opaque: bool,
    exports: &mut ModuleExports,
    attrs: &[HirAttr],
) {
    let site = NameSite::new(module.resolved.module.source_id, name.span);
    let Some(binding) = module.binding_id_at_site(site) else {
        return;
    };
    if exports
        .bindings
        .iter()
        .any(|export| export.binding == binding)
    {
        return;
    }
    exports.bindings.push(ExportBinding {
        binding,
        name: interner.resolve(name.name).into(),
        opaque,
        attrs: attrs.to_vec().into_boxed_slice(),
    });
}
