use std::collections::HashMap;

use music_arena::SliceRange;
use music_hir::{
    HirArg, HirArrayItem, HirCaseArm, HirConstraint, HirExprId, HirExprKind, HirFieldDef,
    HirForeignDecl, HirMemberDef, HirMemberKind, HirTemplatePart, HirVariantDef,
};
use music_names::Ident;

use crate::api::ClassFacts;
use crate::context::{CollectPass, EffectDef, EffectOpDef};
use crate::decls::member_signature;
use crate::normalize::lower_constraints;
use crate::patterns::bound_name_from_pat;

pub fn collect_module(ctx: &mut CollectPass<'_, '_>) {
    visit_expr(ctx, ctx.root_expr_id());
}

fn visit_expr(ctx: &mut CollectPass<'_, '_>, id: HirExprId) {
    match ctx.expr(id).kind {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            visit_expr_ids(ctx, exprs);
        }
        HirExprKind::Array { items } => visit_array_items(ctx, items),
        HirExprKind::Record { items } | HirExprKind::RecordUpdate { items, .. } => {
            for item in ctx.record_items(items) {
                visit_expr(ctx, item.value);
            }
            if let HirExprKind::RecordUpdate { base, .. } = ctx.expr(id).kind {
                visit_expr(ctx, base);
            }
        }
        HirExprKind::Template { parts } => visit_template_parts(ctx, parts),
        HirExprKind::Pi { binder_ty, ret, .. } => {
            visit_expr(ctx, binder_ty);
            visit_expr(ctx, ret);
        }
        HirExprKind::Lambda { body, .. }
        | HirExprKind::Import { arg: body }
        | HirExprKind::Perform { expr: body }
        | HirExprKind::Export { expr: body, .. } => visit_expr(ctx, body),
        HirExprKind::Call { callee, args } => visit_call(ctx, callee, args),
        HirExprKind::Apply { callee, args } | HirExprKind::Index { base: callee, args } => {
            visit_expr(ctx, callee);
            visit_expr_ids(ctx, args);
        }
        HirExprKind::Field { base, .. } => visit_expr(ctx, base),
        HirExprKind::TypeTest { base, ty, .. } | HirExprKind::TypeCast { base, ty } => {
            visit_expr(ctx, base);
            visit_expr(ctx, ty);
        }
        HirExprKind::Prefix { expr, .. } | HirExprKind::Attributed { expr, .. } => {
            visit_expr(ctx, expr);
        }
        HirExprKind::Binary { left, right, .. } => {
            visit_expr(ctx, left);
            visit_expr(ctx, right);
        }
        HirExprKind::Let { pat, value, .. } => {
            if let Some(name) = bound_name_from_pat(ctx, pat) {
                collect_bound_decl(ctx, value, name);
            }
            visit_expr(ctx, value);
        }
        HirExprKind::Case { scrutinee, arms } => visit_case(ctx, scrutinee, arms),
        HirExprKind::Data { variants, fields } => visit_data(ctx, variants, fields),
        HirExprKind::Effect { members } | HirExprKind::Class { members, .. } => {
            for member in ctx.members(members) {
                visit_member(ctx, &member);
            }
        }
        HirExprKind::Instance { class, members, .. } => {
            visit_expr(ctx, class);
            for member in ctx.members(members) {
                visit_member(ctx, &member);
            }
        }
        HirExprKind::Foreign { decls, .. } => visit_foreign(ctx, decls),
        HirExprKind::Handle { expr, clauses, .. } => {
            visit_expr(ctx, expr);
            for clause in ctx.handle_clauses(clauses) {
                visit_expr(ctx, clause.body);
            }
        }
        HirExprKind::Resume { expr } => {
            if let Some(expr) = expr {
                visit_expr(ctx, expr);
            }
        }
        HirExprKind::Error
        | HirExprKind::Name { .. }
        | HirExprKind::Lit { .. }
        | HirExprKind::ArrayTy { .. }
        | HirExprKind::Variant { .. }
        | HirExprKind::Quote { .. }
        | HirExprKind::Splice { .. } => {}
    }
}

fn visit_expr_ids(ctx: &mut CollectPass<'_, '_>, exprs: SliceRange<HirExprId>) {
    for expr in ctx.expr_ids(exprs) {
        visit_expr(ctx, expr);
    }
}

fn visit_array_items(ctx: &mut CollectPass<'_, '_>, items: SliceRange<HirArrayItem>) {
    for item in ctx.array_items(items) {
        visit_expr(ctx, item.expr);
    }
}

fn visit_template_parts(ctx: &mut CollectPass<'_, '_>, parts: SliceRange<HirTemplatePart>) {
    for part in ctx.template_parts(parts) {
        if let HirTemplatePart::Expr { expr } = part {
            visit_expr(ctx, expr);
        }
    }
}

fn visit_call(ctx: &mut CollectPass<'_, '_>, callee: HirExprId, args: SliceRange<HirArg>) {
    visit_expr(ctx, callee);
    for arg in ctx.args(args) {
        visit_expr(ctx, arg.expr);
    }
}

fn visit_case(ctx: &mut CollectPass<'_, '_>, scrutinee: HirExprId, arms: SliceRange<HirCaseArm>) {
    visit_expr(ctx, scrutinee);
    for arm in ctx.case_arms(arms) {
        if let Some(guard) = arm.guard {
            visit_expr(ctx, guard);
        }
        visit_expr(ctx, arm.expr);
    }
}

fn visit_data(
    ctx: &mut CollectPass<'_, '_>,
    variants: SliceRange<HirVariantDef>,
    fields: SliceRange<HirFieldDef>,
) {
    for variant in ctx.variants(variants) {
        if let Some(arg) = variant.arg {
            visit_expr(ctx, arg);
        }
        if let Some(value) = variant.value {
            visit_expr(ctx, value);
        }
    }
    for field in ctx.fields(fields) {
        visit_expr(ctx, field.ty);
        if let Some(value) = field.value {
            visit_expr(ctx, value);
        }
    }
}

fn visit_foreign(ctx: &mut CollectPass<'_, '_>, decls: SliceRange<HirForeignDecl>) {
    for decl in ctx.foreign_decls(decls) {
        for param in ctx.params(decl.params.clone()) {
            if let Some(ty) = param.ty {
                visit_expr(ctx, ty);
            }
            if let Some(default) = param.default {
                visit_expr(ctx, default);
            }
        }
        if let Some(sig) = decl.sig {
            visit_expr(ctx, sig);
        }
    }
}

fn collect_bound_decl(ctx: &mut CollectPass<'_, '_>, value: HirExprId, name: Ident) {
    match ctx.expr(value).kind {
        HirExprKind::Effect { members } => collect_effect_decl(ctx, name, members),
        HirExprKind::Class {
            constraints,
            members,
        } => collect_class_decl(ctx, value, name, constraints, members),
        _ => {}
    }
}

fn collect_effect_decl(
    ctx: &mut CollectPass<'_, '_>,
    name: Ident,
    members: SliceRange<HirMemberDef>,
) {
    if ctx.effect_def(name.name).is_some() {
        return;
    }
    let ops = ctx
        .members(members)
        .into_iter()
        .filter(|member| member.kind == HirMemberKind::Let)
        .map(|member| {
            let facts = member_signature(ctx, &member, false);
            (
                member.name.name,
                EffectOpDef {
                    params: facts.params.clone(),
                    result: facts.result,
                },
            )
        })
        .collect::<HashMap<_, _>>();
    ctx.insert_effect_def(name.name, EffectDef { ops });
}

fn collect_class_decl(
    ctx: &mut CollectPass<'_, '_>,
    value: HirExprId,
    name: Ident,
    constraints: SliceRange<HirConstraint>,
    members: SliceRange<HirMemberDef>,
) {
    if ctx.class_id(name.name).is_some() {
        return;
    }
    let members_vec = ctx.members(members);
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
    ctx.insert_class_id(name.name, value);
    let constraints = lower_constraints(ctx, constraints);
    ctx.insert_class_facts(
        value,
        ClassFacts {
            name: name.name,
            constraints,
            members: class_members,
            laws,
        },
    );
}

fn visit_member(ctx: &mut CollectPass<'_, '_>, member: &HirMemberDef) {
    for param in ctx.params(member.params.clone()) {
        if let Some(ty) = param.ty {
            visit_expr(ctx, ty);
        }
        if let Some(default) = param.default {
            visit_expr(ctx, default);
        }
    }
    if let Some(sig) = member.sig {
        visit_expr(ctx, sig);
    }
    if let Some(value) = member.value {
        visit_expr(ctx, value);
    }
}
