use std::collections::{BTreeSet, HashMap};

use music_arena::SliceRange;
use music_base::Span;
use music_hir::{
    HirCaseArm, HirDim, HirExprId, HirExprKind, HirFieldDef, HirForeignDecl, HirHandleClause,
    HirMemberDef, HirParam, HirPatId, HirPatKind, HirRecordItem, HirRecordPatField, HirStore,
    HirTemplatePart, HirTyField, HirTyId, HirTyKind, HirVariantDef,
};
use music_module::ModuleKey;
use music_names::{Ident, Interner, NameBindingId, NameSite, Symbol};

use crate::api::{
    ClassMemberSurface, ClassSurface, ConstraintSurface, DefinitionKey, EffectOpSurface,
    EffectSurface, ExportedValue, InstanceSurface, ModuleSurface, SurfaceDim, SurfaceEffectItem,
    SurfaceEffectRow, SurfaceTy, SurfaceTyField, SurfaceTyId, SurfaceTyKind,
};
use crate::effects::EffectRow;

use super::{DeclState, ModuleState, PassBase, RuntimeEnv, TypingState};

pub fn build_module_surface(
    module: &ModuleState,
    runtime: &RuntimeEnv<'_, '_>,
    typing: &TypingState,
    decls: &DeclState,
) -> ModuleSurface {
    let mut tys = SurfaceTyBuilder::new(&module.resolved.module.store, runtime.interner());
    let exports = collect_module_exports(module, runtime.interner());
    let static_imports = module
        .resolved
        .imports
        .iter()
        .map(|import| import.to.clone())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let exported_values = collect_exported_values(module, typing, decls, &exports, &mut tys);
    let exported_classes = collect_exported_classes(module, runtime, decls, &exports, &mut tys);
    let exported_effects = collect_exported_effects(module, runtime, decls, &exports, &mut tys);
    let exported_instances = collect_exported_instances(decls, &exports, &mut tys);

    ModuleSurface {
        module_key: module.resolved.module_key.clone(),
        static_imports,
        tys: tys.finish(),
        exported_values,
        exported_classes,
        exported_effects,
        exported_instances,
    }
}

fn collect_exported_values(
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
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn collect_exported_classes(
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

fn collect_exported_effects(
    _module: &ModuleState,
    _runtime: &RuntimeEnv<'_, '_>,
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
                ops: effect
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
                    .into_boxed_slice(),
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn collect_exported_instances(
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

fn lower_surface_effect_row(tys: &mut SurfaceTyBuilder<'_>, row: &EffectRow) -> SurfaceEffectRow {
    SurfaceEffectRow {
        items: row
            .items
            .iter()
            .map(|item| SurfaceEffectItem {
                name: item.name.clone(),
                arg: item.arg.map(|ty| tys.lower(ty)),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        open: row.open.clone(),
    }
}

pub fn import_surface_ty(
    ctx: &mut PassBase<'_, '_, '_>,
    surface: &ModuleSurface,
    ty: SurfaceTyId,
) -> HirTyId {
    let mut importer = SurfaceTyImporter::new(ctx, surface);
    importer.import(ty)
}

pub fn surface_key(module_key: &ModuleKey, interner: &Interner, name: Symbol) -> DefinitionKey {
    DefinitionKey::new(module_key.clone(), interner.resolve(name))
}

pub fn canonical_surface_ty(surface: &ModuleSurface, ty: SurfaceTyId) -> String {
    match &surface.ty(ty).kind {
        SurfaceTyKind::Error => "<error>".into(),
        SurfaceTyKind::Unknown => "Unknown".into(),
        SurfaceTyKind::Type => "Type".into(),
        SurfaceTyKind::Syntax => "Syntax".into(),
        SurfaceTyKind::Any => "Any".into(),
        SurfaceTyKind::Empty => "Empty".into(),
        SurfaceTyKind::Unit => "Unit".into(),
        SurfaceTyKind::Bool => "Bool".into(),
        SurfaceTyKind::Int => "Int".into(),
        SurfaceTyKind::Float => "Float".into(),
        SurfaceTyKind::String => "String".into(),
        SurfaceTyKind::CString => "CString".into(),
        SurfaceTyKind::CPtr => "CPtr".into(),
        SurfaceTyKind::Module => "Module".into(),
        SurfaceTyKind::Named { name, args } => {
            if args.is_empty() {
                name.to_string()
            } else {
                format!(
                    "{}[{}]",
                    name,
                    args.iter()
                        .copied()
                        .map(|arg| canonical_surface_ty(surface, arg))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        SurfaceTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => {
            let params = params
                .iter()
                .copied()
                .map(|param| canonical_surface_ty(surface, param))
                .collect::<Vec<_>>();
            let left = if params.len() == 1 {
                params[0].clone()
            } else {
                format!("({})", params.join(", "))
            };
            let arrow = if *is_effectful { " ~> " } else { " -> " };
            format!("{left}{arrow}{}", canonical_surface_ty(surface, *ret))
        }
        SurfaceTyKind::Sum { left, right } => {
            format!(
                "{} + {}",
                canonical_surface_ty(surface, *left),
                canonical_surface_ty(surface, *right)
            )
        }
        SurfaceTyKind::Tuple { items } => format!(
            "({})",
            items
                .iter()
                .copied()
                .map(|item| canonical_surface_ty(surface, item))
                .collect::<Vec<_>>()
                .join(", ")
        ),
        SurfaceTyKind::Array { dims, item } => {
            let dims = dims
                .iter()
                .map(|dim| match dim {
                    SurfaceDim::Unknown => "_".into(),
                    SurfaceDim::Name(name) => name.to_string(),
                    SurfaceDim::Int(value) => value.to_string(),
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{dims}]{}", canonical_surface_ty(surface, *item))
        }
        SurfaceTyKind::Mut { inner } => {
            format!("mut {}", canonical_surface_ty(surface, *inner))
        }
        SurfaceTyKind::Record { fields } => format!(
            "{{{}}}",
            fields
                .iter()
                .map(|field| format!(
                    "{} := {}",
                    field.name,
                    canonical_surface_ty(surface, field.ty)
                ))
                .collect::<Vec<_>>()
                .join(", ")
        ),
    }
}

#[derive(Debug, Clone)]
struct ExportBinding {
    binding: NameBindingId,
    name: Box<str>,
    opaque: bool,
}

#[derive(Debug, Default)]
struct ModuleExports {
    bindings: Vec<ExportBinding>,
    instance_spans: Vec<Span>,
}

fn collect_module_exports(module: &ModuleState, interner: &Interner) -> ModuleExports {
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
    let Some(binding) = module
        .resolved
        .names
        .bindings
        .iter()
        .find_map(|(binding, info)| (info.site == site).then_some(binding))
    else {
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

struct SurfaceTyBuilder<'a> {
    hir: &'a HirStore,
    interner: &'a Interner,
    cache: HashMap<HirTyId, SurfaceTyId>,
    tys: Vec<SurfaceTy>,
}

impl<'a> SurfaceTyBuilder<'a> {
    fn new(hir: &'a HirStore, interner: &'a Interner) -> Self {
        Self {
            hir,
            interner,
            cache: HashMap::new(),
            tys: Vec::new(),
        }
    }

    fn lower(&mut self, id: HirTyId) -> SurfaceTyId {
        if let Some(id) = self.cache.get(&id).copied() {
            return id;
        }
        let kind = match &self.hir.tys.get(id).kind {
            HirTyKind::Error => SurfaceTyKind::Error,
            HirTyKind::Unknown => SurfaceTyKind::Unknown,
            HirTyKind::Type => SurfaceTyKind::Type,
            HirTyKind::Syntax => SurfaceTyKind::Syntax,
            HirTyKind::Any => SurfaceTyKind::Any,
            HirTyKind::Empty => SurfaceTyKind::Empty,
            HirTyKind::Unit => SurfaceTyKind::Unit,
            HirTyKind::Bool => SurfaceTyKind::Bool,
            HirTyKind::Int => SurfaceTyKind::Int,
            HirTyKind::Float => SurfaceTyKind::Float,
            HirTyKind::String => SurfaceTyKind::String,
            HirTyKind::CString => SurfaceTyKind::CString,
            HirTyKind::CPtr => SurfaceTyKind::CPtr,
            HirTyKind::Module => SurfaceTyKind::Module,
            HirTyKind::Named { name, args } => SurfaceTyKind::Named {
                name: self.interner.resolve(*name).into(),
                args: self
                    .hir
                    .ty_ids
                    .get(*args)
                    .iter()
                    .copied()
                    .map(|ty| self.lower(ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => SurfaceTyKind::Arrow {
                params: self
                    .hir
                    .ty_ids
                    .get(*params)
                    .iter()
                    .copied()
                    .map(|ty| self.lower(ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                ret: self.lower(*ret),
                is_effectful: *is_effectful,
            },
            HirTyKind::Sum { left, right } => SurfaceTyKind::Sum {
                left: self.lower(*left),
                right: self.lower(*right),
            },
            HirTyKind::Tuple { items } => SurfaceTyKind::Tuple {
                items: self
                    .hir
                    .ty_ids
                    .get(*items)
                    .iter()
                    .copied()
                    .map(|ty| self.lower(ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
            HirTyKind::Array { dims, item } => SurfaceTyKind::Array {
                dims: self
                    .hir
                    .dims
                    .get(dims.clone())
                    .iter()
                    .map(|dim| match dim {
                        HirDim::Unknown => SurfaceDim::Unknown,
                        HirDim::Name(name) => {
                            SurfaceDim::Name(self.interner.resolve(name.name).into())
                        }
                        HirDim::Int(value) => SurfaceDim::Int(*value),
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                item: self.lower(*item),
            },
            HirTyKind::Mut { inner } => SurfaceTyKind::Mut {
                inner: self.lower(*inner),
            },
            HirTyKind::Record { fields } => SurfaceTyKind::Record {
                fields: self
                    .hir
                    .ty_fields
                    .get(fields.clone())
                    .iter()
                    .map(|field| SurfaceTyField {
                        name: self.interner.resolve(field.name).into(),
                        ty: self.lower(field.ty),
                    })
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
        };
        let next = SurfaceTyId::new(u32::try_from(self.tys.len()).unwrap_or(u32::MAX));
        self.tys.push(SurfaceTy { kind });
        let _prev = self.cache.insert(id, next);
        next
    }

    fn finish(self) -> Box<[SurfaceTy]> {
        self.tys.into_boxed_slice()
    }
}

struct SurfaceTyImporter<'ctx, 'ctx_state, 'interner, 'env> {
    ctx: &'ctx mut PassBase<'ctx_state, 'interner, 'env>,
    surface: &'ctx ModuleSurface,
    cache: HashMap<SurfaceTyId, HirTyId>,
}

impl<'ctx, 'ctx_state, 'interner, 'env> SurfaceTyImporter<'ctx, 'ctx_state, 'interner, 'env> {
    fn new(
        ctx: &'ctx mut PassBase<'ctx_state, 'interner, 'env>,
        surface: &'ctx ModuleSurface,
    ) -> Self {
        Self {
            ctx,
            surface,
            cache: HashMap::new(),
        }
    }

    fn import(&mut self, id: SurfaceTyId) -> HirTyId {
        if let Some(id) = self.cache.get(&id).copied() {
            return id;
        }
        let kind = match &self.surface.ty(id).kind {
            SurfaceTyKind::Error => HirTyKind::Error,
            SurfaceTyKind::Unknown => HirTyKind::Unknown,
            SurfaceTyKind::Type => HirTyKind::Type,
            SurfaceTyKind::Syntax => HirTyKind::Syntax,
            SurfaceTyKind::Any => HirTyKind::Any,
            SurfaceTyKind::Empty => HirTyKind::Empty,
            SurfaceTyKind::Unit => HirTyKind::Unit,
            SurfaceTyKind::Bool => HirTyKind::Bool,
            SurfaceTyKind::Int => HirTyKind::Int,
            SurfaceTyKind::Float => HirTyKind::Float,
            SurfaceTyKind::String => HirTyKind::String,
            SurfaceTyKind::CString => HirTyKind::CString,
            SurfaceTyKind::CPtr => HirTyKind::CPtr,
            SurfaceTyKind::Module => HirTyKind::Module,
            SurfaceTyKind::Named { name, args } => {
                let args = args
                    .iter()
                    .copied()
                    .map(|ty| self.import(ty))
                    .collect::<Vec<_>>();
                let args = self.ctx.alloc_ty_list(args);
                HirTyKind::Named {
                    name: self.ctx.intern(name),
                    args,
                }
            }
            SurfaceTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => {
                let params = params
                    .iter()
                    .copied()
                    .map(|ty| self.import(ty))
                    .collect::<Vec<_>>();
                let params = self.ctx.alloc_ty_list(params);
                HirTyKind::Arrow {
                    params,
                    ret: self.import(*ret),
                    is_effectful: *is_effectful,
                }
            }
            SurfaceTyKind::Sum { left, right } => HirTyKind::Sum {
                left: self.import(*left),
                right: self.import(*right),
            },
            SurfaceTyKind::Tuple { items } => {
                let items = items
                    .iter()
                    .copied()
                    .map(|ty| self.import(ty))
                    .collect::<Vec<_>>();
                let items = self.ctx.alloc_ty_list(items);
                HirTyKind::Tuple { items }
            }
            SurfaceTyKind::Array { dims, item } => {
                let dims = dims
                    .iter()
                    .map(|dim| match dim {
                        SurfaceDim::Unknown => HirDim::Unknown,
                        SurfaceDim::Name(name) => {
                            HirDim::Name(Ident::new(self.ctx.intern(name), Span::DUMMY))
                        }
                        SurfaceDim::Int(value) => HirDim::Int(*value),
                    })
                    .collect::<Vec<_>>();
                let dims = self.ctx.alloc_dims(dims);
                HirTyKind::Array {
                    dims,
                    item: self.import(*item),
                }
            }
            SurfaceTyKind::Mut { inner } => HirTyKind::Mut {
                inner: self.import(*inner),
            },
            SurfaceTyKind::Record { fields } => {
                let fields = fields
                    .iter()
                    .map(|field| HirTyField {
                        name: self.ctx.intern(&field.name),
                        ty: self.import(field.ty),
                    })
                    .collect::<Vec<_>>();
                let fields = self.ctx.alloc_ty_fields(fields);
                HirTyKind::Record { fields }
            }
        };
        let local = self.ctx.alloc_ty(kind);
        let _prev = self.cache.insert(id, local);
        local
    }
}
