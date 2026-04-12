use std::collections::{BTreeSet, HashMap};

use music_arena::SliceRange;
use music_hir::{
    HirBinder, HirConstraint, HirExprId, HirMemberDef, HirMemberKind, HirOrigin, HirTyId, HirTyKind,
};
use music_module::ModuleKey;
use music_names::Symbol;

use super::super::exprs::check_expr;
use super::super::normalize::{lower_constraints, render_ty, type_mismatch};
use super::super::surface::{canonical_surface_ty, surface_key};
use super::super::{CheckPass, DiagKind};
use super::declarations::member_signature;
use crate::api::{ClassMemberFacts, DefinitionKey, ExprFacts, InstanceFacts};
use crate::effects::EffectRow;

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
            ctx.diag(
                member.origin.span,
                DiagKind::InstanceMemberArityMismatch,
                "",
            );
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
        ctx.diag(member.origin.span, DiagKind::UnknownInstanceMember, "");
    }
    if let Some(value) = member.value {
        let facts = check_expr(ctx, value);
        let origin = ctx.expr(value).origin;
        type_mismatch(ctx, origin, signature.result, facts.ty);
    } else {
        ctx.diag(
            member.origin.span,
            DiagKind::InstanceMemberValueRequired,
            "",
        );
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
            ctx.diag(member.origin.span, DiagKind::DuplicateInstanceMember, "");
        }
        check_instance_member(ctx, member, expected_members);
    }
    for expected in expected_members.keys() {
        if !seen_members.contains(expected) {
            ctx.diag(origin.span, DiagKind::MissingInstanceMember, "");
        }
    }
    member_names.into_boxed_slice()
}

pub(in super::super) fn check_instance_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    origin: HirOrigin,
    type_params: SliceRange<HirBinder>,
    constraints: SliceRange<HirConstraint>,
    class: HirExprId,
    members: &SliceRange<HirMemberDef>,
) -> ExprFacts {
    let class_origin = ctx.expr(class).origin;
    let class_ty = super::super::normalize::lower_type_expr(ctx, class, class_origin);
    let (class_name, class_args) = if let HirTyKind::Named { name, args } = ctx.ty(class_ty).kind {
        (name, ctx.ty_ids(args).into_boxed_slice())
    } else {
        ctx.diag(origin.span, DiagKind::InvalidInstanceTarget, "");
        (
            ctx.known().unknown,
            Vec::<HirTyId>::new().into_boxed_slice(),
        )
    };

    let class_key = ctx.class_facts_by_name(class_name).map_or_else(
        || surface_key(ctx.module_key(), ctx.interner(), class_name),
        |facts| facts.key.clone(),
    );
    if ctx.is_sealed_class(&class_key) && class_key.module != *ctx.module_key() {
        ctx.diag(origin.span, DiagKind::SealedClass, "");
    }

    if ctx.class_id(class_name).is_none() && ctx.class_facts_by_name(class_name).is_none() {
        ctx.diag(origin.span, DiagKind::UnknownClass, "");
    }

    let members_vec = ctx.members((*members).clone());
    let expected_members = class_member_map(ctx, class_name);
    let member_names = check_instance_member_set(ctx, origin, &members_vec, &expected_members);

    let type_params = ctx
        .binders(type_params)
        .into_iter()
        .map(|binder| binder.name.name)
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

pub(in super::super) fn check_instance_coherence(ctx: &mut CheckPass<'_, '_, '_>) {
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
            ctx.diag(facts.origin.span, DiagKind::DuplicateInstance, "");
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
        stack.extend(surface.static_imports().iter().cloned());
        for instance in surface.exported_instances() {
            let args = instance
                .class_args
                .iter()
                .copied()
                .map(|ty| canonical_surface_ty(&surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let key = (instance.class_key.clone(), args);
            if seen.insert(key, module.clone()).is_some() {
                ctx.diag(root_span, DiagKind::DuplicateInstance, "");
            }
        }
    }
}
