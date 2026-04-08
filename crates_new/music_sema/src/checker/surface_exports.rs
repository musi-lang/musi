use music_arena::SliceRange;
use music_base::Span;
use music_hir::{
    HirAttr, HirAttrArg, HirCaseArm, HirExprId, HirExprKind, HirFieldDef, HirForeignDecl,
    HirHandleClause, HirLitKind, HirMemberDef, HirParam, HirPatId, HirPatKind, HirRecordItem,
    HirRecordPatField, HirTemplatePart, HirVariantDef,
};
use music_names::{Ident, Interner, NameBindingId, NameSite};

use super::surface_types::{SurfaceTyBuilder, lower_surface_effect_row};
use super::{DeclState, ModuleState, RuntimeEnv, TypingState};
use crate::api::{
    Attr, AttrArg, AttrRecordField, AttrValue, ClassMemberSurface, ClassSurface, ConstraintSurface,
    DataSurface, DataVariantSurface, EffectOpSurface, EffectSurface, ExportedValue, InstanceSurface,
    SurfaceEffectRow,
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
            for arg in module.resolved.module.store.expr_ids.get(args).iter().copied() {
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
                let Some(name) = item.name else {
                    return None;
                };
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
    let args = module
        .resolved
        .module
        .store
        .attr_args
        .get(attr.args.clone())
        .iter()
        .filter_map(|arg: &HirAttrArg| {
            let name = arg.name.map(|ident| interner.resolve(ident.name).into());
            let value = lower_attr_value(module, interner, arg.value)?;
            Some(AttrArg { name, value })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    Some(Attr { path, args })
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

pub(super) fn collect_exported_values(
    module: &ModuleState,
    typing: &TypingState,
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[ExportedValue]> {
    exports
        .bindings
        .iter()
        .filter_map(|export| {
            if typing.is_gated_binding(export.binding) {
                return None;
            }
            let ty = typing.binding_types().get(&export.binding).copied()?;
            let symbol = module.resolved.names.bindings.get(export.binding).name;
            let scheme = typing.binding_schemes().get(&export.binding);
            let (inert_attrs, musi_attrs) = split_export_attrs(module, tys.interner, &export.attrs);
            Some(ExportedValue {
                name: export.name.clone(),
                ty: tys.lower(ty),
                type_params: scheme.map_or_else(Box::<[Box<str>]>::default, |scheme| {
                    scheme
                        .type_params
                        .iter()
                        .map(|symbol| tys.interner.resolve(*symbol).into())
                        .collect::<Vec<_>>()
                        .into_boxed_slice()
                }),
                constraints: scheme.map_or_else(Box::<[ConstraintSurface]>::default, |scheme| {
                    scheme
                        .constraints
                        .iter()
                        .map(|constraint| ConstraintSurface {
                            name: tys.interner.resolve(constraint.name).into(),
                            kind: constraint.kind,
                            value: tys.lower(constraint.value),
                            class_key: constraint.class_key.clone(),
                        })
                        .collect::<Vec<_>>()
                        .into_boxed_slice()
                }),
                effects: scheme.map_or_else(SurfaceEffectRow::default, |scheme| {
                    lower_surface_effect_row(tys, &scheme.effects)
                }),
                opaque: export.opaque,
                module_target: typing
                    .binding_module_targets()
                    .get(&export.binding)
                    .cloned(),
                class_key: decls
                    .class_facts_by_name()
                    .get(&symbol)
                    .map(|facts| facts.key.clone()),
                effect_key: decls
                    .effect_def(export.name.as_ref())
                    .map(|effect| effect.key.clone()),
                data_key: decls
                    .data_def(export.name.as_ref())
                    .map(|data| data.key.clone()),
                inert_attrs,
                musi_attrs,
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn collect_exported_data(
    module: &ModuleState,
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[DataSurface]> {
    exports
        .bindings
        .iter()
        .filter_map(|export| {
            let data = decls.data_def(export.name.as_ref())?;
            let (inert_attrs, musi_attrs) = split_export_attrs(module, tys.interner, &export.attrs);
            Some(DataSurface {
                key: data.key.clone(),
                variants: export
                    .opaque
                    .then(Box::<[DataVariantSurface]>::default)
                    .unwrap_or_else(|| {
                        data.variants
                            .iter()
                            .map(|(name, variant)| DataVariantSurface {
                                name: name.clone(),
                                payload: variant.payload.map(|ty| tys.lower(ty)),
                            })
                            .collect::<Vec<_>>()
                            .into_boxed_slice()
                    }),
                repr_kind: data.repr_kind.clone(),
                layout_align: data.layout_align,
                layout_pack: data.layout_pack,
                inert_attrs,
                musi_attrs,
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn collect_exported_classes(
    module: &ModuleState,
    runtime: &RuntimeEnv<'_, '_>,
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[ClassSurface]> {
    exports
        .bindings
        .iter()
        .filter_map(|export| {
            let symbol = module.resolved.names.bindings.get(export.binding).name;
            let facts = decls.class_facts_by_name().get(&symbol)?;
            let (inert_attrs, musi_attrs) = split_export_attrs(module, tys.interner, &export.attrs);
            Some(ClassSurface {
                key: facts.key.clone(),
                constraints: facts
                    .constraints
                    .iter()
                    .map(|constraint| ConstraintSurface {
                        name: runtime.interner().resolve(constraint.name).into(),
                        kind: constraint.kind,
                        value: tys.lower(constraint.value),
                        class_key: constraint.class_key.clone(),
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                members: facts
                    .members
                    .iter()
                    .map(|member| ClassMemberSurface {
                        name: runtime.interner().resolve(member.name).into(),
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
                    .map(|law| runtime.interner().resolve(*law).into())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                inert_attrs,
                musi_attrs,
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn collect_exported_effects(
    module: &ModuleState,
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[EffectSurface]> {
    exports
        .bindings
        .iter()
        .filter_map(|export| {
            let effect = decls.effect_def(export.name.as_ref())?;
            let (inert_attrs, musi_attrs) = split_export_attrs(module, tys.interner, &export.attrs);
            Some(EffectSurface {
                key: effect.key.clone(),
                ops: export
                    .opaque
                    .then(Box::<[EffectOpSurface]>::default)
                    .unwrap_or_else(|| {
                        effect
                            .ops
                            .iter()
                            .map(|(name, op)| EffectOpSurface {
                                name: name.clone(),
                                params: op
                                    .params
                                    .iter()
                                    .copied()
                                    .map(|ty| tys.lower(ty))
                                    .collect::<Vec<_>>()
                                    .into_boxed_slice(),
                                result: tys.lower(op.result),
                            })
                            .collect::<Vec<_>>()
                            .into_boxed_slice()
                    }),
                laws: export
                    .opaque
                    .then(Box::<[Box<str>]>::default)
                    .unwrap_or_else(|| {
                        effect
                            .laws
                            .iter()
                            .map(|symbol| tys.interner.resolve(*symbol).into())
                            .collect::<Vec<_>>()
                            .into_boxed_slice()
                    }),
                inert_attrs,
                musi_attrs,
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
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
            type_params: facts
                .type_params
                .iter()
                .map(|symbol| tys.interner.resolve(*symbol).into())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            class_key: facts.class_key.clone(),
            class_args: facts
                .class_args
                .iter()
                .copied()
                .map(|ty| tys.lower(ty))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            constraints: facts
                .constraints
                .iter()
                .map(|constraint| ConstraintSurface {
                    name: tys.interner.resolve(constraint.name).into(),
                    kind: constraint.kind,
                    value: tys.lower(constraint.value),
                    class_key: constraint.class_key.clone(),
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            member_names: facts
                .member_names
                .iter()
                .map(|symbol| tys.interner.resolve(*symbol).into())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
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
    collect_exports_from_expr(
        module,
        interner,
        module.resolved.module.root,
        &mut exports,
        &mut attr_stack,
    );
    exports
}

fn collect_exports_from_expr(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    match module.resolved.module.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            collect_expr_id_range(module, interner, exprs, exports, attr_stack);
        }
        HirExprKind::Array { items } => {
            for item in module.resolved.module.store.array_items.get(items) {
                collect_exports_from_expr(module, interner, item.expr, exports, attr_stack);
            }
        }
        HirExprKind::Record { items } => {
            collect_record_item_exports(module, interner, items, exports, attr_stack);
        }
        HirExprKind::RecordUpdate { base, items } => {
            collect_record_item_exports(module, interner, items, exports, attr_stack);
            collect_exports_from_expr(module, interner, base, exports, attr_stack);
        }
        HirExprKind::Template { parts } => {
            for part in module.resolved.module.store.template_parts.get(parts) {
                if let HirTemplatePart::Expr { expr } = part {
                    collect_exports_from_expr(module, interner, *expr, exports, attr_stack);
                }
            }
        }
        HirExprKind::Pi { binder_ty, ret, .. } => {
            collect_exports_from_expr(module, interner, binder_ty, exports, attr_stack);
            collect_exports_from_expr(module, interner, ret, exports, attr_stack);
        }
        HirExprKind::Lambda { body, .. } | HirExprKind::Import { arg: body } | HirExprKind::Perform { expr: body } => {
            collect_exports_from_expr(module, interner, body, exports, attr_stack);
        }
        HirExprKind::Attributed { attrs, expr: body } => {
            let start = attr_stack.len();
            attr_stack.extend_from_slice(module.resolved.module.store.attrs.get(attrs));
            collect_exports_from_expr(module, interner, body, exports, attr_stack);
            attr_stack.truncate(start);
        }
        HirExprKind::Call { callee, args } => {
            collect_exports_from_expr(module, interner, callee, exports, attr_stack);
            for arg in module.resolved.module.store.args.get(args) {
                collect_exports_from_expr(module, interner, arg.expr, exports, attr_stack);
            }
        }
        HirExprKind::Apply { callee, args } | HirExprKind::Index { base: callee, args } => {
            collect_exports_from_expr(module, interner, callee, exports, attr_stack);
            collect_expr_id_range(module, interner, args, exports, attr_stack);
        }
        HirExprKind::Field { base, .. }
        | HirExprKind::TypeTest { base, .. }
        | HirExprKind::TypeCast { base, .. }
        | HirExprKind::Prefix { expr: base, .. } => {
            collect_exports_from_expr(module, interner, base, exports, attr_stack);
        }
        HirExprKind::Binary { left, right, .. } => {
            collect_exports_from_expr(module, interner, left, exports, attr_stack);
            collect_exports_from_expr(module, interner, right, exports, attr_stack);
        }
        HirExprKind::Let { value, .. } => {
            collect_exports_from_expr(module, interner, value, exports, attr_stack);
        }
        HirExprKind::Export { opaque, expr, .. } => {
            collect_direct_exports(module, interner, expr, opaque, exports, attr_stack);
            collect_exports_from_expr(module, interner, expr, exports, attr_stack);
        }
        HirExprKind::Case { scrutinee, arms } => {
            collect_case_exports(module, interner, scrutinee, arms, exports, attr_stack);
        }
        HirExprKind::Data { variants, fields } => {
            collect_data_exports(module, interner, variants, fields, exports, attr_stack);
        }
        HirExprKind::Effect { members } | HirExprKind::Class { members, .. } => {
            collect_member_exports(module, interner, members, exports, attr_stack);
        }
        HirExprKind::Instance { class, members, .. } => {
            collect_exports_from_expr(module, interner, class, exports, attr_stack);
            collect_member_exports(module, interner, members, exports, attr_stack);
        }
        HirExprKind::Foreign { decls, .. } => {
            collect_foreign_exports(module, interner, decls, exports, attr_stack);
        }
        HirExprKind::Handle { expr, clauses, .. } => {
            collect_handle_exports(module, interner, expr, clauses, exports, attr_stack);
        }
        HirExprKind::Resume { expr } => {
            if let Some(expr) = expr {
                collect_exports_from_expr(module, interner, expr, exports, attr_stack);
            }
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

fn collect_expr_id_range(
    module: &ModuleState,
    interner: &Interner,
    exprs: SliceRange<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for expr in module
        .resolved
        .module
        .store
        .expr_ids
        .get(exprs)
        .iter()
        .copied()
    {
        collect_exports_from_expr(module, interner, expr, exports, attr_stack);
    }
}

fn collect_record_item_exports(
    module: &ModuleState,
    interner: &Interner,
    items: SliceRange<HirRecordItem>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for item in module.resolved.module.store.record_items.get(items) {
        collect_exports_from_expr(module, interner, item.value, exports, attr_stack);
    }
}

fn collect_case_exports(
    module: &ModuleState,
    interner: &Interner,
    scrutinee: HirExprId,
    arms: SliceRange<HirCaseArm>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exports_from_expr(module, interner, scrutinee, exports, attr_stack);
    for arm in module.resolved.module.store.case_arms.get(arms) {
        if let Some(guard) = arm.guard {
            collect_exports_from_expr(module, interner, guard, exports, attr_stack);
        }
        collect_exports_from_expr(module, interner, arm.expr, exports, attr_stack);
    }
}

fn collect_data_exports(
    module: &ModuleState,
    interner: &Interner,
    variants: SliceRange<HirVariantDef>,
    fields: SliceRange<HirFieldDef>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for variant in module.resolved.module.store.variants.get(variants) {
        if let Some(arg) = variant.arg {
            collect_exports_from_expr(module, interner, arg, exports, attr_stack);
        }
        if let Some(value) = variant.value {
            collect_exports_from_expr(module, interner, value, exports, attr_stack);
        }
    }
    for field in module.resolved.module.store.fields.get(fields) {
        collect_exports_from_expr(module, interner, field.ty, exports, attr_stack);
        if let Some(value) = field.value {
            collect_exports_from_expr(module, interner, value, exports, attr_stack);
        }
    }
}

fn collect_param_exports(
    module: &ModuleState,
    interner: &Interner,
    params: SliceRange<HirParam>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for param in module.resolved.module.store.params.get(params) {
        if let Some(ty) = param.ty {
            collect_exports_from_expr(module, interner, ty, exports, attr_stack);
        }
        if let Some(default) = param.default {
            collect_exports_from_expr(module, interner, default, exports, attr_stack);
        }
    }
}

fn collect_member_exports(
    module: &ModuleState,
    interner: &Interner,
    members: SliceRange<HirMemberDef>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for member in module.resolved.module.store.members.get(members) {
        collect_param_exports(module, interner, member.params.clone(), exports, attr_stack);
        if let Some(sig) = member.sig {
            collect_exports_from_expr(module, interner, sig, exports, attr_stack);
        }
        if let Some(value) = member.value {
            collect_exports_from_expr(module, interner, value, exports, attr_stack);
        }
    }
}

fn collect_foreign_exports(
    module: &ModuleState,
    interner: &Interner,
    decls: SliceRange<HirForeignDecl>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for decl in module.resolved.module.store.foreign_decls.get(decls) {
        collect_param_exports(module, interner, decl.params.clone(), exports, attr_stack);
        if let Some(sig) = decl.sig {
            collect_exports_from_expr(module, interner, sig, exports, attr_stack);
        }
    }
}

fn collect_handle_exports(
    module: &ModuleState,
    interner: &Interner,
    expr: HirExprId,
    clauses: SliceRange<HirHandleClause>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exports_from_expr(module, interner, expr, exports, attr_stack);
    for clause in module.resolved.module.store.handle_clauses.get(clauses) {
        collect_exports_from_expr(module, interner, clause.body, exports, attr_stack);
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
    match module.resolved.module.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Let { pat, .. } => {
            collect_export_bindings_from_pat(module, interner, pat, opaque, exports, attr_stack);
        }
        HirExprKind::Foreign { decls, .. } => {
            for decl in module.resolved.module.store.foreign_decls.get(decls) {
                push_export_binding(module, interner, decl.name, opaque, exports, attr_stack);
            }
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
        HirPatKind::Record { fields } => {
            for field in module.resolved.module.store.record_pat_fields.get(fields) {
                collect_export_binding_from_record_field(
                    module,
                    interner,
                    field,
                    opaque,
                    exports,
                    attr_stack,
                );
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

fn collect_export_binding_from_record_field(
    module: &ModuleState,
    interner: &Interner,
    field: &HirRecordPatField,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    if let Some(value) = field.value {
        collect_export_bindings_from_pat(module, interner, value, opaque, exports, attr_stack);
    } else {
        push_export_binding(module, interner, field.name, opaque, exports, attr_stack);
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
