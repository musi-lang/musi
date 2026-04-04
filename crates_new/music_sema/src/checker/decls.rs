use std::collections::{BTreeSet, HashMap};

use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirConstraint, HirEffectSet, HirExprId, HirExprKind, HirFieldDef, HirForeignDecl,
    HirHandleClause, HirLetMods, HirMemberDef, HirMemberKind, HirOrigin, HirParam, HirPatId,
    HirPatKind, HirTyId, HirTyKind, HirVariantDef,
};
use music_module::ModuleKey;
use music_names::{Ident, NameBindingId, Symbol};

use super::attrs::{validate_expr_attrs, validate_foreign_decl};
use super::exprs::check_expr;
use super::normalize::{
    lower_constraints, lower_effect_row, lower_params, lower_type_expr, render_ty, type_mismatch,
};
use super::patterns::{bind_pat, bound_name_from_pat};
use super::schemes::{BindingScheme, instantiate_monomorphic_scheme, scheme_from_export};
use super::surface::{canonical_surface_ty, import_surface_ty, surface_key};
use super::{CheckPass, EffectDef, EffectOpDef, PassBase, ResumeCtx};
use crate::api::{
    ClassFacts, ClassMemberFacts, ClassSurface, ConstraintFacts, DefinitionKey, EffectSurface,
    ExportedValue, ExprFacts, InstanceFacts, ModuleSurface, PatFacts,
};
use crate::effects::{EffectKey, EffectRow};

pub struct LetExprInput {
    pub origin: HirOrigin,
    pub mods: HirLetMods,
    pub pat: HirPatId,
    pub type_params: SliceRange<Ident>,
    pub params: SliceRange<HirParam>,
    pub constraints: SliceRange<HirConstraint>,
    pub effects: Option<HirEffectSet>,
    pub sig: Option<HirExprId>,
    pub value: HirExprId,
}

fn lower_let_type_params(
    ctx: &CheckPass<'_, '_, '_>,
    type_params: SliceRange<Ident>,
) -> Box<[Symbol]> {
    ctx.idents(type_params)
        .into_iter()
        .map(|ident| ident.name)
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn insert_let_binding_scheme(
    ctx: &mut CheckPass<'_, '_, '_>,
    binding: NameBindingId,
    ty: HirTyId,
    effects: EffectRow,
    type_params: Box<[Symbol]>,
    constraints: Box<[ConstraintFacts]>,
) {
    ctx.insert_binding_type(binding, ty);
    ctx.insert_binding_effects(binding, effects.clone());
    ctx.insert_binding_scheme(
        binding,
        BindingScheme {
            type_params,
            constraints,
            ty,
            effects,
        },
    );
}

fn check_callable_let_binding(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    params: SliceRange<HirParam>,
    effects: Option<&HirEffectSet>,
    declared_ty: Option<HirTyId>,
    value: HirExprId,
) -> (HirTyId, EffectRow) {
    let param_types = lower_params(ctx, params);
    let mut callable_effects = effects.map_or(EffectRow::empty(), |set| lower_effect_row(ctx, set));
    let body_facts = check_expr(ctx, value);
    if effects.is_none() {
        callable_effects = body_facts.effects.clone();
    } else {
        callable_effects =
            require_declared_effects(ctx, origin, &callable_effects, &body_facts.effects);
    }
    let result_ty = declared_ty.unwrap_or(body_facts.ty);
    type_mismatch(ctx, origin, result_ty, body_facts.ty);
    let params = ctx.alloc_ty_list(param_types.iter().copied());
    let ty = ctx.alloc_ty(HirTyKind::Arrow {
        params,
        ret: result_ty,
        is_effectful: !callable_effects.is_pure(),
    });
    (ty, callable_effects)
}

pub fn member_signature(
    ctx: &mut PassBase<'_, '_, '_>,
    member: &HirMemberDef,
    bind_name: bool,
) -> ClassMemberFacts {
    let builtins = ctx.builtins();
    let params = ctx
        .params(member.params.clone())
        .into_iter()
        .map(|param| {
            param.ty.map_or(builtins.unknown, |expr| {
                let origin = ctx.expr(expr).origin;
                lower_type_expr(ctx, expr, origin)
            })
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let result = member.sig.map_or(builtins.unknown, |expr| {
        let origin = ctx.expr(expr).origin;
        lower_type_expr(ctx, expr, origin)
    });
    if bind_name {
        let params_list = ctx.alloc_ty_list(params.iter().copied());
        let ty = ctx.alloc_ty(HirTyKind::Arrow {
            params: params_list,
            ret: result,
            is_effectful: false,
        });
        if let Some(binding) = ctx.binding_id_for_decl(member.name) {
            ctx.insert_binding_type(binding, ty);
        }
    }
    ClassMemberFacts {
        name: member.name.name,
        params,
        result,
    }
}

pub fn check_let_expr(ctx: &mut CheckPass<'_, '_, '_>, input: LetExprInput) -> ExprFacts {
    let builtins = ctx.builtins();
    let LetExprInput {
        origin,
        mods,
        pat,
        type_params,
        params,
        constraints,
        effects,
        sig,
        value,
    } = input;
    let bound_name = bound_name_from_pat(ctx, pat);
    let binding = bound_name.and_then(|ident| ctx.binding_id_for_decl(ident));
    let type_params = lower_let_type_params(ctx, type_params);
    let constraints = lower_constraints(ctx, constraints);
    let declared_ty = sig.map(|expr| {
        let origin = ctx.expr(expr).origin;
        lower_type_expr(ctx, expr, origin)
    });

    if mods.is_rec {
        if let Some(binding) = binding {
            ctx.insert_binding_type(binding, declared_ty.unwrap_or(builtins.unknown));
        }
    }

    let value_facts = if let Some(name) = bound_name {
        match ctx.expr(value).kind {
            HirExprKind::Data { variants, fields } => check_bound_data(ctx, name, variants, fields),
            HirExprKind::Effect { members } => check_bound_effect(ctx, value, name, members),
            HirExprKind::Class {
                constraints,
                members,
            } => check_bound_class(ctx, value, name, constraints, members),
            _ => check_expr(ctx, value),
        }
    } else {
        check_expr(ctx, value)
    };

    let has_params = !ctx.params(params.clone()).is_empty();
    let final_ty = if has_params {
        let (ty, callable_effects) =
            check_callable_let_binding(ctx, origin, params, effects.as_ref(), declared_ty, value);
        if let Some(binding) = binding {
            insert_let_binding_scheme(ctx, binding, ty, callable_effects, type_params, constraints);
        }
        ty
    } else {
        let ty = declared_ty.unwrap_or(value_facts.ty);
        type_mismatch(ctx, origin, ty, value_facts.ty);
        if let Some(binding) = binding {
            insert_let_binding_scheme(
                ctx,
                binding,
                ty,
                EffectRow::empty(),
                type_params,
                constraints,
            );
        }
        ty
    };

    if !bind_module_pattern(ctx, pat, value) {
        bind_pat(ctx, pat, final_ty);
    }
    if let Some(binding) = binding
        && let Some(target) = module_target_for_expr(ctx, value)
    {
        ctx.insert_binding_module_target(binding, target);
    }
    if let Some(name) = bound_name {
        bind_imported_alias(ctx, name, value);
    }
    ExprFacts {
        ty: builtins.unit,
        effects: EffectRow::empty(),
    }
}

pub fn check_import_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    arg: HirExprId,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let arg_facts = check_expr(ctx, arg);
    let origin = ctx.expr(arg).origin;
    type_mismatch(ctx, origin, builtins.string_, arg_facts.ty);
    if let Some(target) = ctx.static_import_target(ctx.expr(expr_id).origin.span) {
        ctx.set_expr_module_target(expr_id, target);
    }
    ExprFacts {
        ty: builtins.module,
        effects: arg_facts.effects,
    }
}

pub(super) fn module_target_for_expr(
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

pub(super) fn module_export_for_expr(
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

fn bind_module_pattern(ctx: &mut CheckPass<'_, '_, '_>, pat: HirPatId, value: HirExprId) -> bool {
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

fn bind_imported_alias(ctx: &mut CheckPass<'_, '_, '_>, name: Ident, value: HirExprId) {
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
        .collect::<HashMap<_, _>>();
    let alias_name: Box<str> = ctx.resolve_symbol(alias.name).into();
    ctx.insert_effect_def(
        alias_name,
        EffectDef {
            key: surface.key.clone(),
            ops,
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

fn class_member_map(
    ctx: &CheckPass<'_, '_, '_>,
    class_name: Symbol,
) -> HashMap<Symbol, ClassMemberFacts> {
    ctx.class_facts_by_name(class_name)
        .map(|facts| {
            facts
                .members
                .iter()
                .map(|member| (member.name, member.clone()))
                .collect::<HashMap<_, _>>()
        })
        .unwrap_or_default()
}

fn check_instance_member(
    ctx: &mut CheckPass<'_, '_, '_>,
    member: &HirMemberDef,
    expected_members: &HashMap<Symbol, ClassMemberFacts>,
) {
    let signature = member_signature(ctx, member, true);
    if let Some(expected) = expected_members.get(&member.name.name) {
        if expected.params.len() != signature.params.len() {
            ctx.diag(member.origin.span, "instance member arity mismatch", "");
        }
        for (expected_param, actual_param) in expected
            .params
            .iter()
            .copied()
            .zip(signature.params.iter().copied())
        {
            type_mismatch(ctx, member.origin, expected_param, actual_param);
        }
        type_mismatch(ctx, member.origin, expected.result, signature.result);
    } else {
        ctx.diag(member.origin.span, "unknown instance member", "");
    }
    if let Some(value) = member.value {
        let params = ctx.alloc_ty_list(signature.params.iter().copied());
        let expected = ctx.alloc_ty(HirTyKind::Arrow {
            params,
            ret: signature.result,
            is_effectful: false,
        });
        let facts = check_expr(ctx, value);
        let origin = ctx.expr(value).origin;
        type_mismatch(ctx, origin, expected, facts.ty);
    } else {
        ctx.diag(member.origin.span, "instance member value required", "");
    }
}

fn check_instance_member_set(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    members: &[HirMemberDef],
    expected_members: &HashMap<Symbol, ClassMemberFacts>,
) -> Box<[Symbol]> {
    let member_names = members
        .iter()
        .filter(|member| member.kind == HirMemberKind::Let)
        .map(|member| member.name.name)
        .collect::<Vec<_>>();
    let mut seen_members = BTreeSet::new();
    for member in members {
        if member.kind != HirMemberKind::Let {
            continue;
        }
        if !seen_members.insert(member.name.name) {
            ctx.diag(member.origin.span, "duplicate instance member", "");
        }
        check_instance_member(ctx, member, expected_members);
    }
    for expected in expected_members.keys() {
        if !seen_members.contains(expected) {
            ctx.diag(origin.span, "missing instance member", "");
        }
    }
    member_names.into_boxed_slice()
}

pub fn check_data_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    variants: SliceRange<HirVariantDef>,
    fields: SliceRange<HirFieldDef>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    for variant in ctx.variants(variants) {
        if let Some(arg) = variant.arg {
            let origin = ctx.expr(arg).origin;
            let _ = lower_type_expr(ctx, arg, origin);
        }
        if let Some(value) = variant.value {
            let _ = check_expr(ctx, value);
        }
    }
    for field in ctx.fields(fields) {
        let origin = ctx.expr(field.ty).origin;
        let _ = lower_type_expr(ctx, field.ty, origin);
        if let Some(value) = field.value {
            let _ = check_expr(ctx, value);
        }
    }
    ExprFacts {
        ty: builtins.type_,
        effects: EffectRow::empty(),
    }
}

pub fn check_effect_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    members: SliceRange<HirMemberDef>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    for member in ctx.members(members) {
        let _ = member_signature(ctx, &member, true);
        if let Some(value) = member.value {
            let _ = check_expr(ctx, value);
        }
    }
    ExprFacts {
        ty: builtins.type_,
        effects: EffectRow::empty(),
    }
}

pub fn check_class_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    constraints: SliceRange<HirConstraint>,
    members: SliceRange<HirMemberDef>,
    _bound_name: Option<Ident>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    if let Some(facts) = ctx.class_facts(expr_id).cloned() {
        for member in ctx.members(members) {
            if member.kind == HirMemberKind::Law
                && let Some(value) = member.value
            {
                let law_facts = check_expr(ctx, value);
                let origin = ctx.expr(value).origin;
                type_mismatch(ctx, origin, builtins.bool_, law_facts.ty);
            } else {
                let _ = member_signature(ctx, &member, true);
            }
        }
        let _ = lower_constraints(ctx, constraints);
        ctx.insert_class_facts(expr_id, facts);
    } else {
        for member in ctx.members(members) {
            let _ = member_signature(ctx, &member, true);
        }
    }
    ExprFacts {
        ty: builtins.type_,
        effects: EffectRow::empty(),
    }
}

pub fn check_instance_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    origin: HirOrigin,
    type_params: SliceRange<Ident>,
    constraints: SliceRange<HirConstraint>,
    class: HirExprId,
    members: &SliceRange<HirMemberDef>,
) -> ExprFacts {
    let class_origin = ctx.expr(class).origin;
    let class_ty = lower_type_expr(ctx, class, class_origin);
    let (class_name, class_args) = if let HirTyKind::Named { name, args } = ctx.ty(class_ty).kind {
        (name, ctx.ty_ids(args).into_boxed_slice())
    } else {
        ctx.diag(origin.span, "invalid instance target", "");
        (
            ctx.known().unknown,
            Vec::<HirTyId>::new().into_boxed_slice(),
        )
    };

    let class_key = ctx.class_facts_by_name(class_name).map_or_else(
        || surface_key(ctx.module_key(), ctx.interner(), class_name),
        |facts| facts.key.clone(),
    );

    if ctx.class_id(class_name).is_none() && ctx.class_facts_by_name(class_name).is_none() {
        ctx.diag(origin.span, "unknown class", "");
    }

    let members_vec = ctx.members((*members).clone());
    let expected_members = class_member_map(ctx, class_name);
    let member_names = check_instance_member_set(ctx, origin, &members_vec, &expected_members);

    let type_params = ctx
        .idents(type_params)
        .into_iter()
        .map(|ident| ident.name)
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let constraints = lower_constraints(ctx, constraints);
    ctx.insert_instance_facts(
        expr_id,
        InstanceFacts {
            origin,
            type_params,
            class_key,
            class_name,
            class_args,
            constraints,
            member_names,
        },
    );
    ExprFacts {
        ty: class_ty,
        effects: EffectRow::empty(),
    }
}

pub fn check_foreign_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    abi: Option<Box<str>>,
    decls: SliceRange<HirForeignDecl>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let abi = abi.unwrap_or_else(|| Box::<str>::from("c"));
    for decl in ctx.foreign_decls(decls) {
        let params = lower_params(ctx, decl.params.clone());
        let result = decl.sig.map_or(builtins.unknown, |sig| {
            let origin = ctx.expr(sig).origin;
            lower_type_expr(ctx, sig, origin)
        });
        let params = ctx.alloc_ty_list(params.iter().copied());
        let ty = ctx.alloc_ty(HirTyKind::Arrow {
            params,
            ret: result,
            is_effectful: false,
        });
        if let Some(binding) = ctx.binding_id_for_decl(decl.name) {
            ctx.insert_binding_type(binding, ty);
        }
        validate_foreign_decl(ctx, &decl, &abi);
    }
    ExprFacts {
        ty: builtins.unit,
        effects: EffectRow::empty(),
    }
}

pub fn check_perform_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    expr: HirExprId,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let inner = check_expr(ctx, expr);
    let mut effects = inner.effects;
    let Some((effect_name, op_def)) = effect_op_call(ctx, expr) else {
        ctx.diag(origin.span, "invalid perform target", "");
        return ExprFacts {
            ty: builtins.unknown,
            effects,
        };
    };
    effects.add(EffectKey {
        name: effect_name,
        arg: None,
    });
    if let HirExprKind::Call { args, .. } = ctx.expr(expr).kind {
        for (arg, expected) in ctx
            .args(args)
            .into_iter()
            .map(|arg| arg.expr)
            .zip(op_def.params.iter().copied())
        {
            let facts = check_expr(ctx, arg);
            let origin = ctx.expr(arg).origin;
            type_mismatch(ctx, origin, expected, facts.ty);
            effects.union_with(&facts.effects);
        }
    }
    ExprFacts {
        ty: op_def.result,
        effects,
    }
}

pub fn check_handle_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    expr: HirExprId,
    handler: Ident,
    clauses: SliceRange<HirHandleClause>,
) -> ExprFacts {
    let handled_facts = check_expr(ctx, expr);
    let handler_name: Box<str> = ctx.resolve_symbol(handler.name).into();
    let Some(effect) = ctx.effect_def(&handler_name).cloned() else {
        ctx.diag(origin.span, "unknown effect", "");
        return handled_facts;
    };

    let value_name = "value";
    let mut result_ty = handled_facts.ty;
    let mut clause_effects = EffectRow::empty();
    let mut seen_value = 0usize;
    let mut seen_ops = BTreeSet::new();

    for clause in ctx.handle_clauses(clauses) {
        let clause_name: Box<str> = ctx.resolve_symbol(clause.op.name).into();
        if clause_name.as_ref() == value_name {
            seen_value = seen_value.saturating_add(1);
            let facts = check_expr(ctx, clause.body);
            let origin = ctx.expr(clause.body).origin;
            type_mismatch(ctx, origin, handled_facts.ty, facts.ty);
            clause_effects.union_with(&facts.effects);
            result_ty = facts.ty;
            continue;
        }

        let did_insert = seen_ops.insert(clause_name.clone());
        if !did_insert {
            ctx.diag(origin.span, "duplicate handler clause", "");
        }
        let Some(op_def) = effect.ops.get(clause_name.as_ref()).cloned() else {
            ctx.diag(origin.span, "unknown effect op", "");
            continue;
        };

        let params = ctx.idents(clause.params);
        if params.len() != op_def.params.len() {
            ctx.diag(origin.span, "handler clause arity mismatch", "");
        }
        for (ident, ty) in params.into_iter().zip(op_def.params.iter().copied()) {
            if let Some(binding) = ctx.binding_id_for_decl(ident) {
                ctx.insert_binding_type(binding, ty);
            }
        }
        if let Some(result) = clause
            .result
            .and_then(|ident| ctx.binding_id_for_decl(ident))
        {
            ctx.insert_binding_type(result, op_def.result);
        }
        ctx.push_resume(ResumeCtx {
            arg: op_def.result,
            result: handled_facts.ty,
        });
        let body = check_expr(ctx, clause.body);
        let _ = ctx.pop_resume();
        let origin = ctx.expr(clause.body).origin;
        type_mismatch(ctx, origin, handled_facts.ty, body.ty);
        clause_effects.union_with(&body.effects);
        result_ty = body.ty;
    }

    if seen_value != 1 {
        ctx.diag(origin.span, "handle requires exactly one value clause", "");
    }
    for op in effect.ops.keys() {
        if !seen_ops.contains(op) {
            ctx.diag(origin.span, "handler missing operation clause", "");
        }
    }

    let mut effects = handled_facts.effects;
    effects.remove_by_name(&handler_name);
    effects.union_with(&clause_effects);
    ExprFacts {
        ty: result_ty,
        effects,
    }
}

pub fn check_resume_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    expr: Option<HirExprId>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let Some(resume) = ctx.resume_top() else {
        ctx.diag(origin.span, "resume outside handler clause", "");
        return ExprFacts {
            ty: builtins.unknown,
            effects: EffectRow::empty(),
        };
    };
    let mut effects = EffectRow::empty();
    if let Some(expr) = expr {
        let facts = check_expr(ctx, expr);
        let origin = ctx.expr(expr).origin;
        type_mismatch(ctx, origin, resume.arg, facts.ty);
        effects.union_with(&facts.effects);
    }
    ExprFacts {
        ty: resume.result,
        effects,
    }
}

pub fn call_effects_for_expr(ctx: &CheckPass<'_, '_, '_>, expr: HirExprId) -> Option<EffectRow> {
    match ctx.expr(expr).kind {
        HirExprKind::Name { name } => ctx
            .binding_id_for_use(name)
            .and_then(|binding| ctx.binding_effects(binding)),
        HirExprKind::Apply { .. } => ctx.expr_callable_effects(expr),
        _ => None,
    }
}

pub fn require_declared_effects(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    declared: &EffectRow,
    actual: &EffectRow,
) -> EffectRow {
    let mut final_row = declared.clone();
    for item in &actual.items {
        if declared.items.contains(item) {
            continue;
        }
        if declared.open.is_some() {
            final_row.add(item.clone());
            continue;
        }
        ctx.diag(origin.span, "effect not declared", "");
    }
    if actual.open.is_some() && declared.open.is_none() {
        ctx.diag(origin.span, "effect not declared", "");
    }
    final_row
}

fn check_bound_data(
    ctx: &mut CheckPass<'_, '_, '_>,
    _name: Ident,
    variants: SliceRange<HirVariantDef>,
    fields: SliceRange<HirFieldDef>,
) -> ExprFacts {
    check_data_expr(ctx, variants, fields)
}

fn check_bound_effect(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    name: Ident,
    members: SliceRange<HirMemberDef>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let effect_name: Box<str> = ctx.resolve_symbol(name.name).into();
    if ctx.effect_def(&effect_name).is_none() {
        let ops = ctx
            .members(members.clone())
            .into_iter()
            .filter(|member| member.kind == HirMemberKind::Let)
            .map(|member| {
                let facts = member_signature(ctx, &member, false);
                (
                    Box::<str>::from(ctx.resolve_symbol(member.name.name)),
                    EffectOpDef {
                        params: facts.params.clone(),
                        result: facts.result,
                    },
                )
            })
            .collect::<HashMap<_, _>>();
        let key = surface_key(ctx.module_key(), ctx.interner(), name.name);
        ctx.insert_effect_def(effect_name, EffectDef { key, ops });
    }
    let _ = expr_id;
    for member in ctx.members(members) {
        let _ = member_signature(ctx, &member, true);
        if let Some(value) = member.value {
            let _ = check_expr(ctx, value);
        }
    }
    ExprFacts {
        ty: builtins.type_,
        effects: EffectRow::empty(),
    }
}

fn check_bound_class(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    name: Ident,
    constraints: SliceRange<HirConstraint>,
    members: SliceRange<HirMemberDef>,
) -> ExprFacts {
    if ctx.class_id(name.name).is_none() {
        ctx.insert_class_id(name.name, expr_id);
        let members_vec = ctx.members(members.clone());
        let class_members = members_vec
            .iter()
            .filter(|member| member.kind == HirMemberKind::Let)
            .map(|member| member_signature(ctx, member, false))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let laws = members_vec
            .iter()
            .filter(|member| member.kind == HirMemberKind::Law)
            .map(|member| member.name.name)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let constraints_facts = lower_constraints(ctx, constraints.clone());
        let facts = ClassFacts {
            key: surface_key(ctx.module_key(), ctx.interner(), name.name),
            name: name.name,
            constraints: constraints_facts,
            members: class_members,
            laws,
        };
        ctx.insert_class_facts(expr_id, facts.clone());
        ctx.insert_class_facts_by_name(name.name, facts);
    }
    check_class_expr(ctx, expr_id, constraints, members, Some(name))
}

fn effect_op_call(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr: HirExprId,
) -> Option<(Box<str>, EffectOpDef)> {
    let HirExprKind::Call { callee, args } = ctx.expr(expr).kind else {
        return None;
    };
    let HirExprKind::Field { base, name, .. } = ctx.expr(callee).kind else {
        return None;
    };
    let HirExprKind::Name { name: effect_name } = ctx.expr(base).kind else {
        return None;
    };
    let effect_name_text: Box<str> = ctx.resolve_symbol(effect_name.name).into();
    let op_name = ctx.resolve_symbol(name.name);
    let op = ctx
        .effect_def(&effect_name_text)
        .and_then(|effect| effect.ops.get(op_name))
        .cloned()?;
    let found = ctx.arg_count(args);
    if found != op.params.len() {
        let span = ctx.expr(expr).origin.span;
        ctx.diag(span, "perform arity mismatch", "");
    }
    Some((effect_name_text, op))
}

pub fn check_attributed_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    attrs: SliceRange<HirAttr>,
    inner: HirExprId,
) -> ExprFacts {
    validate_expr_attrs(ctx, origin, attrs, inner);
    check_expr(ctx, inner)
}

pub fn check_instance_coherence(ctx: &mut CheckPass<'_, '_, '_>) {
    let Some(env) = ctx.sema_env() else {
        return;
    };
    let mut seen = HashMap::<(DefinitionKey, Box<[String]>), ModuleKey>::new();
    let local_instances = ctx.instance_facts().values().cloned().collect::<Vec<_>>();

    for facts in local_instances {
        let args = facts
            .class_args
            .iter()
            .copied()
            .map(|ty| render_ty(ctx, ty))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let key = (facts.class_key.clone(), args);
        if seen.insert(key, ctx.module_key().clone()).is_some() {
            ctx.diag(facts.origin.span, "duplicate instance", "");
        }
    }

    let mut visited = BTreeSet::new();
    let mut stack = ctx.static_imports();
    let root_span = ctx.expr(ctx.root_expr_id()).origin.span;
    while let Some(module) = stack.pop() {
        if !visited.insert(module.clone()) {
            continue;
        }
        let Some(surface) = env.module_surface(&module) else {
            continue;
        };
        stack.extend(surface.static_imports.iter().cloned());
        for instance in &surface.exported_instances {
            let args = instance
                .class_args
                .iter()
                .copied()
                .map(|ty| canonical_surface_ty(&surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let key = (instance.class_key.clone(), args);
            if seen.insert(key, module.clone()).is_some() {
                ctx.diag(root_span, "duplicate instance", "");
            }
        }
    }
}
