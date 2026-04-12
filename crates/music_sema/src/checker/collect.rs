use std::collections::{BTreeMap, HashMap};

use music_arena::SliceRange;
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirCaseArm, HirConstraint, HirExprId, HirExprKind, HirFieldDef,
    HirMemberDef, HirMemberKind, HirOrigin, HirTemplatePart, HirTyId, HirTyKind, HirVariantDef,
};
use music_names::Ident;

use crate::api::ClassFacts;

use super::attrs::extract_data_layout_hints;
use super::decls::{member_law_facts, member_signature};
use super::normalize::{lower_constraints, lower_type_expr};
use super::patterns::bound_name_from_pat;
use super::surface::surface_key;
use super::{CollectPass, DataDef, DataVariantDef, DiagKind, EffectDef, EffectOpDef};

pub fn collect_module(ctx: &mut CollectPass<'_, '_, '_>) {
    visit_expr(ctx, ctx.root_expr_id());
}

fn visit_expr(ctx: &mut CollectPass<'_, '_, '_>, id: HirExprId) {
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
        | HirExprKind::Perform { expr: body } => visit_expr(ctx, body),
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
        HirExprKind::Prefix { expr, .. } => visit_expr(ctx, expr),
        HirExprKind::Binary { left, right, .. } => {
            visit_expr(ctx, left);
            visit_expr(ctx, right);
        }
        HirExprKind::Let { pat, value, .. } => {
            if let Some(name) = bound_name_from_pat(ctx, pat) {
                let attrs = ctx.expr(id).mods.attrs;
                let outer_attrs = (!attrs.is_empty()).then_some((ctx.expr(id).origin, attrs));
                collect_bound_decl(ctx, value, name, outer_attrs.as_ref());
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

fn visit_expr_ids(ctx: &mut CollectPass<'_, '_, '_>, exprs: SliceRange<HirExprId>) {
    for expr in ctx.expr_ids(exprs) {
        visit_expr(ctx, expr);
    }
}

fn visit_array_items(ctx: &mut CollectPass<'_, '_, '_>, items: SliceRange<HirArrayItem>) {
    for item in ctx.array_items(items) {
        visit_expr(ctx, item.expr);
    }
}

fn visit_template_parts(ctx: &mut CollectPass<'_, '_, '_>, parts: SliceRange<HirTemplatePart>) {
    for part in ctx.template_parts(parts) {
        if let HirTemplatePart::Expr { expr } = part {
            visit_expr(ctx, expr);
        }
    }
}

fn visit_call(ctx: &mut CollectPass<'_, '_, '_>, callee: HirExprId, args: SliceRange<HirArg>) {
    visit_expr(ctx, callee);
    for arg in ctx.args(args) {
        visit_expr(ctx, arg.expr);
    }
}

fn visit_case(
    ctx: &mut CollectPass<'_, '_, '_>,
    scrutinee: HirExprId,
    arms: SliceRange<HirCaseArm>,
) {
    visit_expr(ctx, scrutinee);
    for arm in ctx.case_arms(arms) {
        if let Some(guard) = arm.guard {
            visit_expr(ctx, guard);
        }
        visit_expr(ctx, arm.expr);
    }
}

fn visit_data(
    ctx: &mut CollectPass<'_, '_, '_>,
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

fn collect_bound_decl(
    ctx: &mut CollectPass<'_, '_, '_>,
    value: HirExprId,
    name: Ident,
    outer_attrs: Option<&(HirOrigin, SliceRange<HirAttr>)>,
) {
    let origin = outer_attrs.map_or_else(|| ctx.expr(value).origin, |(o, _)| *o);
    let attrs = outer_attrs.map_or_else(Vec::new, |(_, range)| vec![range.clone()]);
    match ctx.expr(value).kind {
        HirExprKind::Data { variants, fields } => {
            collect_data_decl(ctx, origin, &attrs, name, variants, fields);
        }
        HirExprKind::Effect { members } => collect_effect_decl(ctx, name, members),
        HirExprKind::Class {
            constraints,
            members,
        } => collect_class_decl(ctx, value, name, constraints, members),
        _ => {}
    }
}

fn collect_data_decl(
    ctx: &mut CollectPass<'_, '_, '_>,
    origin: HirOrigin,
    attrs: &[SliceRange<HirAttr>],
    name: Ident,
    variants: SliceRange<HirVariantDef>,
    fields: SliceRange<HirFieldDef>,
) {
    let data_name: Box<str> = ctx.resolve_symbol(name.name).into();
    if ctx.data_def(&data_name).is_some() {
        return;
    }

    let (repr_kind, layout_align, layout_pack) = extract_data_layout_hints(ctx, origin, attrs);
    let mut variant_map = BTreeMap::<Box<str>, DataVariantDef>::new();
    for variant in ctx.variants(variants) {
        let tag: Box<str> = ctx.resolve_symbol(variant.name.name).into();
        let payload = variant.arg.map(|expr| {
            let origin = ctx.expr(expr).origin;
            lower_type_expr(ctx, expr, origin)
        });
        let field_tys =
            payload.map_or_else(Box::<[_]>::default, |ty| flatten_data_field_tys(ctx, ty));
        let prev = variant_map.insert(tag, DataVariantDef::new(payload, field_tys));
        if prev.is_some() {
            ctx.diag(
                variant.origin.span,
                DiagKind::CollectDuplicateDataVariant,
                "",
            );
        }
    }
    if variant_map.is_empty() {
        let field_tys = ctx
            .fields(fields)
            .into_iter()
            .map(|field| {
                let origin = ctx.expr(field.ty).origin;
                lower_type_expr(ctx, field.ty, origin)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        if !field_tys.is_empty() {
            let _ = variant_map.insert(data_name.clone(), DataVariantDef::new(None, field_tys));
        }
    }

    let key = surface_key(ctx.module_key(), ctx.interner(), name.name);
    ctx.insert_data_def(
        data_name,
        DataDef::new(key, variant_map, repr_kind, layout_align, layout_pack),
    );
}

fn flatten_data_field_tys(ctx: &CollectPass<'_, '_, '_>, ty: HirTyId) -> Box<[HirTyId]> {
    match &ctx.ty(ty).kind {
        HirTyKind::Tuple { items } => ctx.ty_ids(*items).into_boxed_slice(),
        _ => vec![ty].into_boxed_slice(),
    }
}

fn collect_effect_decl(
    ctx: &mut CollectPass<'_, '_, '_>,
    name: Ident,
    members: SliceRange<HirMemberDef>,
) {
    let effect_name: Box<str> = ctx.resolve_symbol(name.name).into();
    if ctx.effect_def(&effect_name).is_some() {
        return;
    }
    let members_vec = ctx.members(members);
    let mut seen_ops = HashMap::new();
    let mut seen_laws = HashMap::new();
    for member in &members_vec {
        match member.kind {
            HirMemberKind::Let => {
                let op_name: Box<str> = ctx.resolve_symbol(member.name.name).into();
                if seen_ops.insert(op_name, member.origin).is_some() {
                    ctx.diag(member.origin.span, DiagKind::CollectDuplicateEffectOp, "");
                }
            }
            HirMemberKind::Law => {
                if seen_laws.insert(member.name.name, member.origin).is_some() {
                    ctx.diag(member.origin.span, DiagKind::CollectDuplicateEffectLaw, "");
                }
            }
        }
    }
    let ops = members_vec
        .iter()
        .filter(|member| member.kind == HirMemberKind::Let)
        .map(|member| {
            let facts = member_signature(ctx, member, false);
            (
                Box::<str>::from(ctx.resolve_symbol(member.name.name)),
                EffectOpDef::new(facts.params.clone(), facts.result),
            )
        })
        .collect::<BTreeMap<_, _>>();
    let laws = members_vec
        .iter()
        .filter(|member| member.kind == HirMemberKind::Law)
        .map(|member| member_law_facts(ctx, member))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let key = surface_key(ctx.module_key(), ctx.interner(), name.name);
    ctx.insert_effect_def(effect_name, EffectDef::new(key, ops, laws));
}

fn collect_class_decl(
    ctx: &mut CollectPass<'_, '_, '_>,
    value: HirExprId,
    name: Ident,
    constraints: SliceRange<HirConstraint>,
    members: SliceRange<HirMemberDef>,
) {
    if ctx.class_id(name.name).is_some() {
        return;
    }
    let members_vec = ctx.members(members);
    let mut seen_members = HashMap::new();
    let mut seen_laws = HashMap::new();
    for member in &members_vec {
        match member.kind {
            HirMemberKind::Let
                if seen_members
                    .insert(member.name.name, member.origin)
                    .is_some() =>
            {
                ctx.diag(
                    member.origin.span,
                    DiagKind::CollectDuplicateClassMember,
                    "",
                );
            }
            HirMemberKind::Law if seen_laws.insert(member.name.name, member.origin).is_some() => {
                ctx.diag(member.origin.span, DiagKind::CollectDuplicateClassLaw, "");
            }
            _ => {}
        }
    }
    let class_members = members_vec
        .iter()
        .filter(|member| member.kind == HirMemberKind::Let)
        .map(|member| member_signature(ctx, member, false))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let laws = members_vec
        .iter()
        .filter(|member| member.kind == HirMemberKind::Law)
        .map(|member| member_law_facts(ctx, member))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    ctx.insert_class_id(name.name, value);
    let constraints = lower_constraints(ctx, constraints);
    let facts = ClassFacts::new(
        surface_key(ctx.module_key(), ctx.interner(), name.name),
        name.name,
        class_members,
        laws,
    )
    .with_constraints(constraints);
    ctx.insert_class_facts(value, facts.clone());
    ctx.insert_class_facts_by_name(name.name, facts);
}

fn visit_member(ctx: &mut CollectPass<'_, '_, '_>, member: &HirMemberDef) {
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
