use std::collections::BTreeMap;

use music_hir::{HirExprId, HirExprKind, HirPatId, HirPatKind};
use music_module::ModuleKey;
use music_names::Ident;

use super::super::patterns::{bind_pat, bound_name_from_pat};
use super::super::schemes::{instantiate_monomorphic_scheme, scheme_from_export};
use super::super::surface::import_surface_ty;
use super::super::{CheckPass, DataDef, DataVariantDef, EffectDef, EffectOpDef};
use crate::api::{
    ClassFacts, ClassMemberFacts, ConstraintFacts, ExportedValue, ModuleSurface, PatFacts,
};
use crate::api::{ClassSurface, DataSurface, EffectSurface, ExprFacts};

pub(in super::super) fn check_import_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    arg: HirExprId,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let arg_facts = super::super::exprs::check_expr(ctx, arg);
    let origin = ctx.expr(arg).origin;
    super::super::normalize::type_mismatch(ctx, origin, builtins.string_, arg_facts.ty);
    if let Some(target) = ctx.static_import_target(ctx.expr(expr_id).origin.span) {
        ctx.set_expr_module_target(expr_id, target);
    }
    ExprFacts {
        ty: builtins.module,
        effects: arg_facts.effects,
    }
}

pub(in super::super) fn module_target_for_expr(
    ctx: &CheckPass<'_, '_, '_>,
    expr: HirExprId,
) -> Option<ModuleKey> {
    if let Some(target) = ctx.expr_module_target(expr) {
        return Some(target.clone());
    }
    match ctx.expr(expr).kind {
        HirExprKind::Name { name } => ctx
            .binding_id_for_use(name)
            .and_then(|binding| ctx.binding_module_target(binding).cloned()),
        HirExprKind::Export { expr: inner, .. } => module_target_for_expr(ctx, inner),
        _ => None,
    }
}

pub(in super::super) fn module_export_for_expr(
    ctx: &CheckPass<'_, '_, '_>,
    expr: HirExprId,
    name: Ident,
) -> Option<(ModuleSurface, ExportedValue)> {
    let target = module_target_for_expr(ctx, expr)?;
    let env = ctx.sema_env()?;
    let surface = env.module_surface(&target)?;
    let export = surface
        .exported_value(ctx.resolve_symbol(name.name))?
        .clone();
    Some((surface, export))
}

pub(super) fn bind_module_pattern(
    ctx: &mut CheckPass<'_, '_, '_>,
    pat: HirPatId,
    value: HirExprId,
) -> bool {
    let Some(target) = module_target_for_expr(ctx, value) else {
        return false;
    };
    let Some(env) = ctx.sema_env() else {
        return false;
    };
    let Some(surface) = env.module_surface(&target) else {
        return false;
    };
    let HirPatKind::Record { fields } = ctx.pat(pat).kind else {
        return false;
    };
    let module_ty = ctx.builtins().module;

    ctx.set_pat_facts(pat, PatFacts { ty: module_ty });

    for field in ctx.record_pat_fields(fields) {
        let Some(export) = surface
            .exported_value(ctx.resolve_symbol(field.name.name))
            .cloned()
        else {
            ctx.diag(field.name.span, "unknown export", "");
            continue;
        };
        let field_ty = import_surface_ty(ctx, &surface, export.ty);
        if let Some(value) = field.value {
            bind_pat(ctx, value, field_ty);
            if let Some(alias) = bound_name_from_pat(ctx, value) {
                bind_imported_module_member(ctx, alias, &surface, &export);
            }
        } else if let Some(binding) = ctx.binding_id_for_decl(field.name) {
            ctx.insert_binding_type(binding, field_ty);
            bind_imported_module_member(ctx, field.name, &surface, &export);
        }
    }
    true
}

pub(super) fn bind_imported_alias(ctx: &mut CheckPass<'_, '_, '_>, name: Ident, value: HirExprId) {
    let HirExprKind::Field {
        base, name: field, ..
    } = ctx.expr(value).kind
    else {
        return;
    };
    let Some((surface, export)) = module_export_for_expr(ctx, base, field) else {
        return;
    };
    bind_imported_module_member(ctx, name, &surface, &export);
}

fn bind_imported_module_member(
    ctx: &mut CheckPass<'_, '_, '_>,
    alias: Ident,
    surface: &ModuleSurface,
    export: &ExportedValue,
) {
    import_exported_value_binding(ctx, alias, surface, export);
    if let Some(binding) = ctx.binding_id_for_decl(alias)
        && let Some(target) = export.module_target.clone()
    {
        ctx.insert_binding_module_target(binding, target);
    }
    if let Some(class_key) = export.class_key.as_ref()
        && let Some(class) = surface.exported_class(class_key)
    {
        import_class_alias(ctx, alias, surface, class);
    }
    if let Some(effect_key) = export.effect_key.as_ref()
        && let Some(effect) = surface.exported_effect(effect_key)
    {
        import_effect_alias(ctx, alias, surface, effect);
    }
    if let Some(data_key) = export.data_key.as_ref()
        && let Some(data) = surface.exported_data(data_key)
    {
        import_data_alias(ctx, alias, surface, data);
    }
}

fn import_class_alias(
    ctx: &mut CheckPass<'_, '_, '_>,
    alias: Ident,
    module_surface: &ModuleSurface,
    surface: &ClassSurface,
) {
    let facts = ClassFacts {
        key: surface.key.clone(),
        name: alias.name,
        constraints: surface
            .constraints
            .iter()
            .map(|constraint| ConstraintFacts {
                name: ctx.intern(&constraint.name),
                kind: constraint.kind,
                value: import_surface_ty(ctx, module_surface, constraint.value),
                class_key: constraint.class_key.clone(),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        members: surface
            .members
            .iter()
            .map(|member| ClassMemberFacts {
                name: ctx.intern(&member.name),
                params: member
                    .params
                    .iter()
                    .copied()
                    .map(|ty| import_surface_ty(ctx, module_surface, ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                result: import_surface_ty(ctx, module_surface, member.result),
            })
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        laws: surface
            .laws
            .iter()
            .map(|law| ctx.intern(law))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    };
    ctx.insert_class_facts_by_name(alias.name, facts);
}

fn import_effect_alias(
    ctx: &mut CheckPass<'_, '_, '_>,
    alias: Ident,
    module_surface: &ModuleSurface,
    surface: &EffectSurface,
) {
    let ops = surface
        .ops
        .iter()
        .map(|op| {
            (
                op.name.clone(),
                EffectOpDef {
                    params: op
                        .params
                        .iter()
                        .copied()
                        .map(|ty| import_surface_ty(ctx, module_surface, ty))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                    result: import_surface_ty(ctx, module_surface, op.result),
                },
            )
        })
        .collect::<BTreeMap<_, _>>();
    let alias_name: Box<str> = ctx.resolve_symbol(alias.name).into();
    ctx.insert_effect_def(
        alias_name,
        EffectDef {
            key: surface.key.clone(),
            ops,
        },
    );
}

fn import_data_alias(
    ctx: &mut CheckPass<'_, '_, '_>,
    alias: Ident,
    module_surface: &ModuleSurface,
    surface: &DataSurface,
) {
    let variants = surface
        .variants
        .iter()
        .map(|variant| {
            (
                variant.name.clone(),
                DataVariantDef {
                    payload: variant
                        .payload
                        .map(|ty| import_surface_ty(ctx, module_surface, ty)),
                },
            )
        })
        .collect::<BTreeMap<_, _>>();
    let alias_name: Box<str> = ctx.resolve_symbol(alias.name).into();
    ctx.insert_data_def(
        alias_name,
        DataDef {
            key: surface.key.clone(),
            variants,
        },
    );
}

fn import_exported_value_binding(
    ctx: &mut CheckPass<'_, '_, '_>,
    alias: Ident,
    surface: &ModuleSurface,
    export: &ExportedValue,
) {
    let Some(binding) = ctx.binding_id_for_decl(alias) else {
        return;
    };
    let scheme = scheme_from_export(ctx, surface, export);
    let instantiated = if scheme.type_params.is_empty() {
        Some(instantiate_monomorphic_scheme(ctx, &scheme))
    } else {
        None
    };
    let imported_ty = import_surface_ty(ctx, surface, export.ty);
    ctx.insert_binding_type(binding, imported_ty);
    ctx.insert_binding_effects(
        binding,
        instantiated.map_or_else(
            || scheme.effects.clone(),
            |instantiated| instantiated.effects,
        ),
    );
    ctx.insert_binding_scheme(binding, scheme);
}
