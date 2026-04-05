use music_arena::SliceRange;
use music_base::Span;
use music_hir::{
    HirCaseArm, HirExprId, HirExprKind, HirFieldDef, HirForeignDecl, HirHandleClause, HirMemberDef,
    HirParam, HirPatId, HirPatKind, HirRecordItem, HirRecordPatField, HirTemplatePart,
    HirVariantDef,
};
use music_names::{Ident, Interner, NameBindingId, NameSite};

use super::surface_types::{SurfaceTyBuilder, lower_surface_effect_row};
use super::{DeclState, ModuleState, RuntimeEnv, TypingState};
use crate::api::{
    ClassMemberSurface, ClassSurface, ConstraintSurface, DataSurface, DataVariantSurface,
    EffectOpSurface, EffectSurface, ExportedValue, InstanceSurface, SurfaceEffectRow,
};

#[derive(Debug, Clone)]
pub(super) struct ExportBinding {
    pub(super) binding: NameBindingId,
    pub(super) name: Box<str>,
    pub(super) opaque: bool,
}

#[derive(Debug, Default)]
pub(super) struct ModuleExports {
    pub(super) bindings: Vec<ExportBinding>,
    pub(super) instance_spans: Vec<Span>,
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
            let ty = typing.binding_types().get(&export.binding).copied()?;
            let symbol = module.resolved.names.bindings.get(export.binding).name;
            let scheme = typing.binding_schemes().get(&export.binding);
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
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn collect_exported_data(
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[DataSurface]> {
    exports
        .bindings
        .iter()
        .filter_map(|export| {
            let data = decls.data_def(export.name.as_ref())?;
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
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn collect_exported_effects(
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[EffectSurface]> {
    exports
        .bindings
        .iter()
        .filter_map(|export| {
            let effect = decls.effect_def(export.name.as_ref())?;
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
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn collect_exported_instances(
    decls: &DeclState,
    exports: &ModuleExports,
    tys: &mut SurfaceTyBuilder<'_>,
) -> Box<[InstanceSurface]> {
    decls
        .instance_facts()
        .values()
        .filter(|facts| exports.instance_spans.contains(&facts.origin.span))
        .map(|facts| InstanceSurface {
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
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn collect_module_exports(module: &ModuleState, interner: &Interner) -> ModuleExports {
    let mut exports = ModuleExports::default();
    collect_exports_from_expr(module, interner, module.resolved.module.root, &mut exports);
    exports
}

fn collect_exports_from_expr(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
    exports: &mut ModuleExports,
) {
    match module.resolved.module.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            collect_expr_id_range(module, interner, exprs, exports);
        }
        HirExprKind::Array { items } => {
            for item in module.resolved.module.store.array_items.get(items) {
                collect_exports_from_expr(module, interner, item.expr, exports);
            }
        }
        HirExprKind::Record { items } => {
            collect_record_item_exports(module, interner, items, exports);
        }
        HirExprKind::RecordUpdate { base, items } => {
            collect_record_item_exports(module, interner, items, exports);
            collect_exports_from_expr(module, interner, base, exports);
        }
        HirExprKind::Template { parts } => {
            for part in module.resolved.module.store.template_parts.get(parts) {
                if let HirTemplatePart::Expr { expr } = part {
                    collect_exports_from_expr(module, interner, *expr, exports);
                }
            }
        }
        HirExprKind::Pi { binder_ty, ret, .. } => {
            collect_exports_from_expr(module, interner, binder_ty, exports);
            collect_exports_from_expr(module, interner, ret, exports);
        }
        HirExprKind::Lambda { body, .. }
        | HirExprKind::Import { arg: body }
        | HirExprKind::Perform { expr: body }
        | HirExprKind::Attributed { expr: body, .. } => {
            collect_exports_from_expr(module, interner, body, exports);
        }
        HirExprKind::Call { callee, args } => {
            collect_exports_from_expr(module, interner, callee, exports);
            for arg in module.resolved.module.store.args.get(args) {
                collect_exports_from_expr(module, interner, arg.expr, exports);
            }
        }
        HirExprKind::Apply { callee, args } | HirExprKind::Index { base: callee, args } => {
            collect_exports_from_expr(module, interner, callee, exports);
            collect_expr_id_range(module, interner, args, exports);
        }
        HirExprKind::Field { base, .. }
        | HirExprKind::TypeTest { base, .. }
        | HirExprKind::TypeCast { base, .. }
        | HirExprKind::Prefix { expr: base, .. } => {
            collect_exports_from_expr(module, interner, base, exports);
        }
        HirExprKind::Binary { left, right, .. } => {
            collect_exports_from_expr(module, interner, left, exports);
            collect_exports_from_expr(module, interner, right, exports);
        }
        HirExprKind::Let { value, .. } => {
            collect_exports_from_expr(module, interner, value, exports);
        }
        HirExprKind::Export { opaque, expr, .. } => {
            collect_direct_exports(module, interner, expr, opaque, exports);
            collect_exports_from_expr(module, interner, expr, exports);
        }
        HirExprKind::Case { scrutinee, arms } => {
            collect_case_exports(module, interner, scrutinee, arms, exports);
        }
        HirExprKind::Data { variants, fields } => {
            collect_data_exports(module, interner, variants, fields, exports);
        }
        HirExprKind::Effect { members } | HirExprKind::Class { members, .. } => {
            collect_member_exports(module, interner, members, exports);
        }
        HirExprKind::Instance { class, members, .. } => {
            collect_exports_from_expr(module, interner, class, exports);
            collect_member_exports(module, interner, members, exports);
        }
        HirExprKind::Foreign { decls, .. } => {
            collect_foreign_exports(module, interner, decls, exports);
        }
        HirExprKind::Handle { expr, clauses, .. } => {
            collect_handle_exports(module, interner, expr, clauses, exports);
        }
        HirExprKind::Resume { expr } => {
            if let Some(expr) = expr {
                collect_exports_from_expr(module, interner, expr, exports);
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
        collect_exports_from_expr(module, interner, expr, exports);
    }
}

fn collect_record_item_exports(
    module: &ModuleState,
    interner: &Interner,
    items: SliceRange<HirRecordItem>,
    exports: &mut ModuleExports,
) {
    for item in module.resolved.module.store.record_items.get(items) {
        collect_exports_from_expr(module, interner, item.value, exports);
    }
}

fn collect_case_exports(
    module: &ModuleState,
    interner: &Interner,
    scrutinee: HirExprId,
    arms: SliceRange<HirCaseArm>,
    exports: &mut ModuleExports,
) {
    collect_exports_from_expr(module, interner, scrutinee, exports);
    for arm in module.resolved.module.store.case_arms.get(arms) {
        if let Some(guard) = arm.guard {
            collect_exports_from_expr(module, interner, guard, exports);
        }
        collect_exports_from_expr(module, interner, arm.expr, exports);
    }
}

fn collect_data_exports(
    module: &ModuleState,
    interner: &Interner,
    variants: SliceRange<HirVariantDef>,
    fields: SliceRange<HirFieldDef>,
    exports: &mut ModuleExports,
) {
    for variant in module.resolved.module.store.variants.get(variants) {
        if let Some(arg) = variant.arg {
            collect_exports_from_expr(module, interner, arg, exports);
        }
        if let Some(value) = variant.value {
            collect_exports_from_expr(module, interner, value, exports);
        }
    }
    for field in module.resolved.module.store.fields.get(fields) {
        collect_exports_from_expr(module, interner, field.ty, exports);
        if let Some(value) = field.value {
            collect_exports_from_expr(module, interner, value, exports);
        }
    }
}

fn collect_param_exports(
    module: &ModuleState,
    interner: &Interner,
    params: SliceRange<HirParam>,
    exports: &mut ModuleExports,
) {
    for param in module.resolved.module.store.params.get(params) {
        if let Some(ty) = param.ty {
            collect_exports_from_expr(module, interner, ty, exports);
        }
        if let Some(default) = param.default {
            collect_exports_from_expr(module, interner, default, exports);
        }
    }
}

fn collect_member_exports(
    module: &ModuleState,
    interner: &Interner,
    members: SliceRange<HirMemberDef>,
    exports: &mut ModuleExports,
) {
    for member in module.resolved.module.store.members.get(members) {
        collect_param_exports(module, interner, member.params.clone(), exports);
        if let Some(sig) = member.sig {
            collect_exports_from_expr(module, interner, sig, exports);
        }
        if let Some(value) = member.value {
            collect_exports_from_expr(module, interner, value, exports);
        }
    }
}

fn collect_foreign_exports(
    module: &ModuleState,
    interner: &Interner,
    decls: SliceRange<HirForeignDecl>,
    exports: &mut ModuleExports,
) {
    for decl in module.resolved.module.store.foreign_decls.get(decls) {
        collect_param_exports(module, interner, decl.params.clone(), exports);
        if let Some(sig) = decl.sig {
            collect_exports_from_expr(module, interner, sig, exports);
        }
    }
}

fn collect_handle_exports(
    module: &ModuleState,
    interner: &Interner,
    expr: HirExprId,
    clauses: SliceRange<HirHandleClause>,
    exports: &mut ModuleExports,
) {
    collect_exports_from_expr(module, interner, expr, exports);
    for clause in module.resolved.module.store.handle_clauses.get(clauses) {
        collect_exports_from_expr(module, interner, clause.body, exports);
    }
}

fn collect_direct_exports(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
    opaque: bool,
    exports: &mut ModuleExports,
) {
    match module.resolved.module.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Let { pat, .. } => {
            collect_export_bindings_from_pat(module, interner, pat, opaque, exports);
        }
        HirExprKind::Foreign { decls, .. } => {
            for decl in module.resolved.module.store.foreign_decls.get(decls) {
                push_export_binding(module, interner, decl.name, opaque, exports);
            }
        }
        HirExprKind::Instance { .. } => {
            let span = module.resolved.module.store.exprs.get(expr_id).origin.span;
            if opaque {
                return;
            }
            if !exports.instance_spans.contains(&span) {
                exports.instance_spans.push(span);
            }
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
) {
    match module.resolved.module.store.pats.get(pat_id).kind.clone() {
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Lit { .. } => {}
        HirPatKind::Bind { name } => push_export_binding(module, interner, name, opaque, exports),
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
                collect_export_bindings_from_pat(module, interner, item, opaque, exports);
            }
        }
        HirPatKind::Record { fields } => {
            for field in module.resolved.module.store.record_pat_fields.get(fields) {
                collect_export_binding_from_record_field(module, interner, field, opaque, exports);
            }
        }
        HirPatKind::Or { left, right } => {
            collect_export_bindings_from_pat(module, interner, left, opaque, exports);
            collect_export_bindings_from_pat(module, interner, right, opaque, exports);
        }
        HirPatKind::As { pat, name } => {
            collect_export_bindings_from_pat(module, interner, pat, opaque, exports);
            push_export_binding(module, interner, name, opaque, exports);
        }
    }
}

fn collect_export_binding_from_record_field(
    module: &ModuleState,
    interner: &Interner,
    field: &HirRecordPatField,
    opaque: bool,
    exports: &mut ModuleExports,
) {
    if let Some(value) = field.value {
        collect_export_bindings_from_pat(module, interner, value, opaque, exports);
    } else {
        push_export_binding(module, interner, field.name, opaque, exports);
    }
}

fn push_export_binding(
    module: &ModuleState,
    interner: &Interner,
    name: Ident,
    opaque: bool,
    exports: &mut ModuleExports,
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
    });
}
