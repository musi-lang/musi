use std::collections::{BTreeSet, HashMap};

use music_arena::SliceRange;
use music_hir::{HirOrigin, HirTyField, HirTyId, HirTyKind};
use music_names::Symbol;

use crate::api::{
    ConstraintFacts, ConstraintKind, ConstraintSurface, DefinitionKey, ExportedValue,
    InstanceFacts, InstanceSurface, ModuleSurface, SurfaceEffectRow, SurfaceTyId,
};
use crate::effects::{EffectKey, EffectRow};

use super::normalize::{named_type_for_symbol, render_ty, ty_matches};
use super::surface::{import_surface_ty, surface_key};
use super::{CheckPass, DiagKind, PassBase};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingScheme {
    pub type_params: Box<[Symbol]>,
    pub constraints: Box<[ConstraintFacts]>,
    pub ty: HirTyId,
    pub effects: EffectRow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintObligation {
    pub kind: ConstraintKind,
    pub subject: HirTyId,
    pub value: HirTyId,
    pub class_key: Option<DefinitionKey>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstantiatedBinding {
    pub ty: HirTyId,
    pub effects: EffectRow,
    pub obligations: Box<[ConstraintObligation]>,
}

pub fn scheme_from_export(
    ctx: &mut PassBase<'_, '_, '_>,
    surface: &ModuleSurface,
    export: &ExportedValue,
) -> BindingScheme {
    BindingScheme {
        type_params: export
            .type_params
            .iter()
            .map(|name| ctx.intern(name))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        constraints: export
            .constraints
            .iter()
            .map(|constraint| import_constraint_surface(ctx, surface, constraint))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        ty: import_surface_ty(ctx, surface, export.ty),
        effects: import_surface_effect_row(ctx, surface, &export.effects),
    }
}

pub fn instance_facts_from_surface(
    ctx: &mut PassBase<'_, '_, '_>,
    surface: &ModuleSurface,
    instance: &InstanceSurface,
) -> InstanceFacts {
    let class_name = class_name_from_surface(
        ctx,
        surface,
        instance.class_args.as_ref(),
        &instance.class_key,
    );
    InstanceFacts {
        origin: HirOrigin::dummy(),
        type_params: instance
            .type_params
            .iter()
            .map(|name| ctx.intern(name))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        class_key: instance.class_key.clone(),
        class_name,
        class_args: instance
            .class_args
            .iter()
            .copied()
            .map(|ty| import_surface_ty(ctx, surface, ty))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        constraints: instance
            .constraints
            .iter()
            .map(|constraint| import_constraint_surface(ctx, surface, constraint))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        member_names: Box::new([]),
    }
}

pub fn instantiate_binding_scheme(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    scheme: &BindingScheme,
    args: &[HirTyId],
) -> Option<InstantiatedBinding> {
    if scheme.type_params.len() != args.len() {
        ctx.diag(origin.span, DiagKind::TypeApplicationArityMismatch, "");
        return None;
    }
    let subst = scheme
        .type_params
        .iter()
        .copied()
        .zip(args.iter().copied())
        .collect::<HashMap<_, _>>();
    Some(instantiate_binding_with_subst(ctx, scheme, &subst))
}

pub fn instantiate_monomorphic_scheme(
    ctx: &mut CheckPass<'_, '_, '_>,
    scheme: &BindingScheme,
) -> InstantiatedBinding {
    instantiate_binding_with_subst(ctx, scheme, &HashMap::new())
}

pub fn solve_obligations(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    obligations: &[ConstraintObligation],
) {
    let mut stack = Vec::<String>::new();
    for obligation in obligations {
        let _ = solve_obligation(ctx, origin, obligation, &mut stack);
    }
}

fn instantiate_binding_with_subst(
    ctx: &mut CheckPass<'_, '_, '_>,
    scheme: &BindingScheme,
    subst: &HashMap<Symbol, HirTyId>,
) -> InstantiatedBinding {
    let ty = substitute_ty(ctx, scheme.ty, subst);
    let effects = substitute_effect_row(ctx, &scheme.effects, subst);
    let obligations = scheme
        .constraints
        .iter()
        .map(|constraint| instantiate_obligation(ctx, constraint, subst))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    InstantiatedBinding {
        ty,
        effects,
        obligations,
    }
}

fn instantiate_obligation(
    ctx: &mut PassBase<'_, '_, '_>,
    constraint: &ConstraintFacts,
    subst: &HashMap<Symbol, HirTyId>,
) -> ConstraintObligation {
    ConstraintObligation {
        kind: constraint.kind,
        subject: subst
            .get(&constraint.name)
            .copied()
            .unwrap_or_else(|| named_type_for_symbol(ctx, constraint.name)),
        value: substitute_ty(ctx, constraint.value, subst),
        class_key: constraint.class_key.clone(),
    }
}

pub fn substitute_ty(
    ctx: &mut PassBase<'_, '_, '_>,
    ty: HirTyId,
    subst: &HashMap<Symbol, HirTyId>,
) -> HirTyId {
    let kind = ctx.ty(ty).kind;
    match kind {
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
        | HirTyKind::Float
        | HirTyKind::String
        | HirTyKind::CString
        | HirTyKind::CPtr
        | HirTyKind::Module
        | HirTyKind::NatLit(_) => ty,
        HirTyKind::Named { name, args } => {
            if ctx.ty_ids(args).is_empty()
                && let Some(found) = subst.get(&name).copied()
            {
                return found;
            }
            let args = substitute_ty_list(ctx, args, subst);
            ctx.alloc_ty(HirTyKind::Named { name, args })
        }
        HirTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => {
            let binder_ty = substitute_ty(ctx, binder_ty, subst);
            let mut next = subst.clone();
            let _ = next.remove(&binder);
            let body = substitute_ty(ctx, body, &next);
            ctx.alloc_ty(HirTyKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            })
        }
        HirTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => {
            let params = substitute_ty_list(ctx, params, subst);
            let ret = substitute_ty(ctx, ret, subst);
            ctx.alloc_ty(HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            })
        }
        HirTyKind::Sum { left, right } => {
            let left = substitute_ty(ctx, left, subst);
            let right = substitute_ty(ctx, right, subst);
            ctx.alloc_ty(HirTyKind::Sum { left, right })
        }
        HirTyKind::Tuple { items } => {
            let items = substitute_ty_list(ctx, items, subst);
            ctx.alloc_ty(HirTyKind::Tuple { items })
        }
        HirTyKind::Array { dims, item } => {
            let item = substitute_ty(ctx, item, subst);
            ctx.alloc_ty(HirTyKind::Array { dims, item })
        }
        HirTyKind::Mut { inner } => {
            let inner = substitute_ty(ctx, inner, subst);
            ctx.alloc_ty(HirTyKind::Mut { inner })
        }
        HirTyKind::Record { fields } => {
            let fields = ctx
                .ty_fields(fields)
                .into_iter()
                .map(|field| HirTyField {
                    name: field.name,
                    ty: substitute_ty(ctx, field.ty, subst),
                })
                .collect::<Vec<_>>();
            let fields = ctx.alloc_ty_fields(fields);
            ctx.alloc_ty(HirTyKind::Record { fields })
        }
    }
}

fn substitute_ty_list(
    ctx: &mut PassBase<'_, '_, '_>,
    tys: SliceRange<HirTyId>,
    subst: &HashMap<Symbol, HirTyId>,
) -> SliceRange<HirTyId> {
    let tys = ctx
        .ty_ids(tys)
        .into_iter()
        .map(|ty| substitute_ty(ctx, ty, subst))
        .collect::<Vec<_>>();
    ctx.alloc_ty_list(tys)
}

pub fn substitute_effect_row(
    ctx: &mut PassBase<'_, '_, '_>,
    row: &EffectRow,
    subst: &HashMap<Symbol, HirTyId>,
) -> EffectRow {
    let mut items = BTreeSet::new();
    for item in &row.items {
        let _ = items.insert(EffectKey {
            name: item.name.clone(),
            arg: item.arg.map(|arg| substitute_ty(ctx, arg, subst)),
        });
    }
    EffectRow {
        items,
        open: row
            .open
            .as_deref()
            .map(|name| ctx.fresh_open_row_name(name)),
    }
}

fn import_constraint_surface(
    ctx: &mut PassBase<'_, '_, '_>,
    surface: &ModuleSurface,
    constraint: &ConstraintSurface,
) -> ConstraintFacts {
    ConstraintFacts {
        name: ctx.intern(&constraint.name),
        kind: constraint.kind,
        value: import_surface_ty(ctx, surface, constraint.value),
        class_key: constraint.class_key.clone(),
    }
}

fn import_surface_effect_row(
    ctx: &mut PassBase<'_, '_, '_>,
    surface: &ModuleSurface,
    row: &SurfaceEffectRow,
) -> EffectRow {
    let mut out = EffectRow::empty();
    for item in &row.items {
        out.add(EffectKey {
            name: item.name.clone(),
            arg: item.arg.map(|arg| import_surface_ty(ctx, surface, arg)),
        });
    }
    out.open = row
        .open
        .as_deref()
        .map(|name| ctx.fresh_open_row_name(name));
    out
}

fn solve_obligation(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    obligation: &ConstraintObligation,
    stack: &mut Vec<String>,
) -> bool {
    match obligation.kind {
        ConstraintKind::Subtype => {
            if ty_matches(ctx, obligation.value, obligation.subject) {
                return true;
            }
            ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            false
        }
        ConstraintKind::Implements => solve_implements(ctx, origin, obligation, stack),
    }
}

fn solve_implements(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    obligation: &ConstraintObligation,
    stack: &mut Vec<String>,
) -> bool {
    let Some((class_key, class_args)) = obligation_class_target(ctx, obligation) else {
        ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
        return false;
    };
    let frame = format!(
        "{}:{}",
        class_key.name,
        class_args
            .iter()
            .copied()
            .map(|arg| render_ty(ctx, arg))
            .collect::<Vec<_>>()
            .join(", ")
    );
    if stack.contains(&frame) {
        return true;
    }
    stack.push(frame);

    let mut matches = 0usize;
    let local_instances = ctx.instance_facts().values().cloned().collect::<Vec<_>>();
    for instance in &local_instances {
        if instance.class_key != class_key {
            continue;
        }
        if instance_matches(ctx, origin, instance, &class_args, stack) {
            matches = matches.saturating_add(1);
        }
    }

    let Some(env) = ctx.sema_env() else {
        let _ = stack.pop();
        if matches == 0 {
            ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            return false;
        }
        if matches > 1 {
            ctx.diag(origin.span, DiagKind::AmbiguousInstanceMatch, "");
            return false;
        }
        return true;
    };

    let mut visited = BTreeSet::new();
    let mut pending = ctx.static_imports();
    while let Some(module_key) = pending.pop() {
        if !visited.insert(module_key.clone()) {
            continue;
        }
        let Some(surface) = env.module_surface(&module_key) else {
            continue;
        };
        pending.extend(surface.static_imports().iter().cloned());
        for instance in surface.exported_instances() {
            if instance.class_key != class_key {
                continue;
            }
            let imported = instance_facts_from_surface(ctx, &surface, instance);
            if instance_matches(ctx, origin, &imported, &class_args, stack) {
                matches = matches.saturating_add(1);
            }
        }
    }

    let _ = stack.pop();
    if matches == 0 {
        ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
        return false;
    }
    if matches > 1 {
        ctx.diag(origin.span, DiagKind::AmbiguousInstanceMatch, "");
        return false;
    }
    true
}

fn obligation_class_target(
    ctx: &CheckPass<'_, '_, '_>,
    obligation: &ConstraintObligation,
) -> Option<(DefinitionKey, Box<[HirTyId]>)> {
    let HirTyKind::Named { name, args } = ctx.ty(obligation.value).kind else {
        return None;
    };
    let class_key = obligation.class_key.clone().unwrap_or_else(|| {
        ctx.class_facts_by_name(name).map_or_else(
            || surface_key(ctx.module_key(), ctx.interner(), name),
            |facts| facts.key.clone(),
        )
    });
    let class_args = if ctx.ty_ids(args).is_empty() {
        vec![obligation.subject]
    } else {
        ctx.ty_ids(args)
    };
    Some((class_key, class_args.into_boxed_slice()))
}

fn instance_matches(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    instance: &InstanceFacts,
    class_args: &[HirTyId],
    stack: &mut Vec<String>,
) -> bool {
    let Some(subst) =
        unify_instance_args(ctx, &instance.type_params, &instance.class_args, class_args)
    else {
        return false;
    };
    let obligations = instance
        .constraints
        .iter()
        .map(|constraint| instantiate_obligation(ctx, constraint, &subst))
        .collect::<Vec<_>>();
    obligations
        .iter()
        .all(|obligation| solve_obligation(ctx, origin, obligation, stack))
}

fn unify_instance_args(
    ctx: &mut CheckPass<'_, '_, '_>,
    type_params: &[Symbol],
    pattern_args: &[HirTyId],
    actual_args: &[HirTyId],
) -> Option<HashMap<Symbol, HirTyId>> {
    if pattern_args.len() != actual_args.len() {
        return None;
    }
    let mut subst = HashMap::new();
    for (pattern, actual) in pattern_args
        .iter()
        .copied()
        .zip(actual_args.iter().copied())
    {
        if !unify_ty(ctx, type_params, pattern, actual, &mut subst) {
            return None;
        }
    }
    Some(subst)
}

fn unify_ty(
    ctx: &mut CheckPass<'_, '_, '_>,
    type_params: &[Symbol],
    pattern: HirTyId,
    actual: HirTyId,
    subst: &mut HashMap<Symbol, HirTyId>,
) -> bool {
    match ctx.ty(pattern).kind {
        HirTyKind::Named { name, args }
            if ctx.ty_ids(args).is_empty() && type_params.contains(&name) =>
        {
            unify_type_param(ctx, name, actual, subst)
        }
        HirTyKind::Named {
            name: left_name,
            args: left_args,
        } => unify_named_ty(ctx, type_params, actual, subst, left_name, left_args),
        HirTyKind::Arrow {
            params: left_params,
            ret: left_ret,
            is_effectful: left_effectful,
        } => unify_arrow_ty(
            ctx,
            type_params,
            actual,
            subst,
            left_params,
            left_ret,
            left_effectful,
        ),
        HirTyKind::Sum { left, right } => {
            let HirTyKind::Sum {
                left: actual_left,
                right: actual_right,
            } = ctx.ty(actual).kind
            else {
                return false;
            };
            unify_ty(ctx, type_params, left, actual_left, subst)
                && unify_ty(ctx, type_params, right, actual_right, subst)
        }
        HirTyKind::Tuple { items } => {
            let HirTyKind::Tuple {
                items: actual_items,
            } = ctx.ty(actual).kind
            else {
                return false;
            };
            unify_ty_lists(ctx, type_params, items, actual_items, subst)
        }
        HirTyKind::Array { dims, item } => {
            let HirTyKind::Array {
                dims: actual_dims,
                item: actual_item,
            } = ctx.ty(actual).kind
            else {
                return false;
            };
            ctx.dims(dims) == ctx.dims(actual_dims)
                && unify_ty(ctx, type_params, item, actual_item, subst)
        }
        HirTyKind::Mut { inner } => {
            let HirTyKind::Mut {
                inner: actual_inner,
            } = ctx.ty(actual).kind
            else {
                return false;
            };
            unify_ty(ctx, type_params, inner, actual_inner, subst)
        }
        HirTyKind::Record { fields } => unify_record_ty(ctx, type_params, actual, subst, fields),
        _ => ty_matches(ctx, pattern, actual),
    }
}

fn unify_type_param(
    ctx: &CheckPass<'_, '_, '_>,
    name: Symbol,
    actual: HirTyId,
    subst: &mut HashMap<Symbol, HirTyId>,
) -> bool {
    if let Some(bound) = subst.get(&name).copied() {
        return ty_matches(ctx, bound, actual);
    }
    let _prev = subst.insert(name, actual);
    true
}

fn unify_named_ty(
    ctx: &mut CheckPass<'_, '_, '_>,
    type_params: &[Symbol],
    actual: HirTyId,
    subst: &mut HashMap<Symbol, HirTyId>,
    left_name: Symbol,
    left_args: SliceRange<HirTyId>,
) -> bool {
    let HirTyKind::Named {
        name: right_name,
        args: right_args,
    } = ctx.ty(actual).kind
    else {
        return false;
    };
    left_name == right_name && unify_ty_lists(ctx, type_params, left_args, right_args, subst)
}

fn unify_arrow_ty(
    ctx: &mut CheckPass<'_, '_, '_>,
    type_params: &[Symbol],
    actual: HirTyId,
    subst: &mut HashMap<Symbol, HirTyId>,
    left_params: SliceRange<HirTyId>,
    left_ret: HirTyId,
    left_effectful: bool,
) -> bool {
    let HirTyKind::Arrow {
        params: right_params,
        ret: right_ret,
        is_effectful: right_effectful,
    } = ctx.ty(actual).kind
    else {
        return false;
    };
    left_effectful == right_effectful
        && unify_ty_lists(ctx, type_params, left_params, right_params, subst)
        && unify_ty(ctx, type_params, left_ret, right_ret, subst)
}

fn unify_record_ty(
    ctx: &mut CheckPass<'_, '_, '_>,
    type_params: &[Symbol],
    actual: HirTyId,
    subst: &mut HashMap<Symbol, HirTyId>,
    fields: SliceRange<HirTyField>,
) -> bool {
    let HirTyKind::Record {
        fields: actual_fields,
    } = ctx.ty(actual).kind
    else {
        return false;
    };
    let pattern_fields = ctx.ty_fields(fields);
    let actual_fields = ctx.ty_fields(actual_fields);
    if pattern_fields.len() != actual_fields.len() {
        return false;
    }
    let actual_fields = actual_fields
        .into_iter()
        .map(|field| (field.name, field.ty))
        .collect::<HashMap<_, _>>();
    pattern_fields.into_iter().all(|field| {
        actual_fields
            .get(&field.name)
            .is_some_and(|ty| unify_ty(ctx, type_params, field.ty, *ty, subst))
    })
}

fn unify_ty_lists(
    ctx: &mut CheckPass<'_, '_, '_>,
    type_params: &[Symbol],
    pattern: SliceRange<HirTyId>,
    actual: SliceRange<HirTyId>,
    subst: &mut HashMap<Symbol, HirTyId>,
) -> bool {
    let pattern = ctx.ty_ids(pattern);
    let actual = ctx.ty_ids(actual);
    pattern.len() == actual.len()
        && pattern
            .into_iter()
            .zip(actual)
            .all(|(pattern, actual)| unify_ty(ctx, type_params, pattern, actual, subst))
}

fn class_name_from_surface(
    ctx: &mut PassBase<'_, '_, '_>,
    surface: &ModuleSurface,
    class_args: &[SurfaceTyId],
    class_key: &DefinitionKey,
) -> Symbol {
    for class in surface.exported_classes() {
        if &class.key == class_key {
            return ctx.intern(&class.key.name);
        }
    }
    let _ = class_args;
    ctx.intern(&class_key.name)
}
