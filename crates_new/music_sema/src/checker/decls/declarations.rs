use std::collections::BTreeMap;

use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirConstraint, HirExprId, HirExprKind, HirFieldDef, HirForeignDecl, HirMemberDef,
    HirMemberKind, HirOrigin, HirTyKind, HirVariantDef,
};
use music_names::Ident;

use super::super::attrs::{validate_expr_attrs, validate_foreign_decl};
use super::super::attrs::{validate_link_attr, validate_when_attr};
use super::super::exprs::check_expr;
use super::super::normalize::{lower_constraints, lower_params, lower_type_expr, type_mismatch};
use super::super::surface::surface_key;
use super::super::{CheckPass, EffectDef, EffectOpDef, PassBase};
use crate::api::{ClassFacts, ClassMemberFacts, ExprFacts, ForeignLinkInfo, TargetInfo};
use crate::effects::EffectRow;

pub(in super::super) fn member_signature(
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

pub(in super::super) fn check_data_expr(
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

pub(in super::super) fn check_class_expr(
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

pub(in super::super) fn check_foreign_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    abi: Option<Box<str>>,
    decls: SliceRange<HirForeignDecl>,
    group_attrs: Option<SliceRange<HirAttr>>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let abi = abi.unwrap_or_else(|| Box::<str>::from("c"));
    let group_attrs = group_attrs
        .map(|range| ctx.attrs(range))
        .unwrap_or_default();
    for attr in &group_attrs {
        let path = super::super::attrs::attr_path(ctx, attr);
        match path.as_slice() {
            ["link"] => validate_link_attr(ctx, attr, origin),
            ["when"] => validate_when_attr(ctx, attr, origin),
            _ => {}
        }
    }
    let group_link = link_info_from_attrs(ctx, &group_attrs);
    for decl in ctx.foreign_decls(decls) {
        let decl_attrs = ctx.attrs(decl.attrs.clone());
        let mut all_attrs = Vec::<HirAttr>::with_capacity(group_attrs.len() + decl_attrs.len());
        all_attrs.extend_from_slice(&group_attrs);
        all_attrs.extend_from_slice(&decl_attrs);

        if !when_attrs_match(ctx, &all_attrs) {
            if let Some(binding) = ctx.binding_id_for_decl(decl.name) {
                ctx.mark_gated_binding(binding);
            }
            continue;
        }
        if let Some(binding) = ctx.binding_id_for_decl(decl.name) {
            let merged = merge_link_info(&group_link, &link_info_from_attrs(ctx, &decl_attrs));
            if merged.name.is_some() || merged.symbol.is_some() {
                ctx.set_foreign_link(binding, merged);
            }
        }
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

fn when_attrs_match(ctx: &CheckPass<'_, '_, '_>, attrs: &[HirAttr]) -> bool {
    let target = ctx.target();
    for attr in attrs {
        let path = super::super::attrs::attr_path(ctx, attr);
        if path.as_slice() != ["when"] {
            continue;
        }
        if !when_attr_matches(ctx, target, attr) {
            return false;
        }
    }
    true
}

fn when_attr_matches(
    ctx: &CheckPass<'_, '_, '_>,
    target: Option<&TargetInfo>,
    attr: &HirAttr,
) -> bool {
    let Some(target) = target else {
        return false;
    };

    for arg in ctx.attr_args(attr.args.clone()) {
        let Some(name) = arg.name.map(|ident| ctx.resolve_symbol(ident.name)) else {
            continue;
        };
        let values = when_values(ctx, arg.value);
        let Some(values) = values else {
            continue;
        };

        let matched = match name {
            "os" => target.os.as_deref().is_some_and(|t| values.iter().any(|v| v == t)),
            "arch" => target.arch.as_deref().is_some_and(|t| values.iter().any(|v| v == t)),
            "env" => target.env.as_deref().is_some_and(|t| values.iter().any(|v| v == t)),
            "abi" => target.abi.as_deref().is_some_and(|t| values.iter().any(|v| v == t)),
            "vendor" => target
                .vendor
                .as_deref()
                .is_some_and(|t| values.iter().any(|v| v == t)),
            "feature" => values
                .iter()
                .any(|v| target.features.contains(v.as_str())),
            _ => true,
        };
        if !matched {
            return false;
        }
    }
    true
}

fn when_values(ctx: &CheckPass<'_, '_, '_>, expr: HirExprId) -> Option<Vec<String>> {
    match ctx.expr(expr).kind {
        HirExprKind::Lit { lit } => ctx.lit_string_value(lit).map(|s| vec![s]),
        HirExprKind::Array { items } => {
            let mut out = Vec::<String>::new();
            for item in ctx.array_items(items) {
                if let HirExprKind::Lit { lit } = ctx.expr(item.expr).kind {
                    if let Some(value) = ctx.lit_string_value(lit) {
                        out.push(value);
                    }
                }
            }
            Some(out)
        }
        _ => None,
    }
}

fn link_info_from_attrs(ctx: &CheckPass<'_, '_, '_>, attrs: &[HirAttr]) -> ForeignLinkInfo {
    let mut out = ForeignLinkInfo::default();
    for attr in attrs {
        let path = super::super::attrs::attr_path(ctx, attr);
        if path.as_slice() != ["link"] {
            continue;
        }
        let mut positional = Vec::<String>::new();
        for arg in ctx.attr_args(attr.args.clone()) {
            let Some(value) = string_lit_value(ctx, arg.value) else {
                continue;
            };
            if let Some(name) = arg.name.map(|ident| ctx.resolve_symbol(ident.name)) {
                match name {
                    "name" => out.name = Some(value.into_boxed_str()),
                    "symbol" => out.symbol = Some(value.into_boxed_str()),
                    _ => {}
                }
            } else {
                positional.push(value);
            }
        }
        if out.name.is_none() {
            if let Some(value) = positional.first().cloned() {
                out.name = Some(value.into_boxed_str());
            }
        }
        if out.symbol.is_none() {
            if let Some(value) = positional.get(1).cloned() {
                out.symbol = Some(value.into_boxed_str());
            }
        }
    }
    out
}

fn string_lit_value(ctx: &CheckPass<'_, '_, '_>, expr: HirExprId) -> Option<String> {
    match ctx.expr(expr).kind {
        HirExprKind::Lit { lit } => ctx.lit_string_value(lit),
        _ => None,
    }
}

fn merge_link_info(group: &ForeignLinkInfo, decl: &ForeignLinkInfo) -> ForeignLinkInfo {
    ForeignLinkInfo {
        name: decl.name.clone().or_else(|| group.name.clone()),
        symbol: decl
            .symbol
            .clone()
            .or_else(|| group.symbol.clone()),
    }
}

pub(super) fn check_bound_data(
    ctx: &mut CheckPass<'_, '_, '_>,
    name: Ident,
    variants: SliceRange<HirVariantDef>,
    fields: SliceRange<HirFieldDef>,
) -> ExprFacts {
    let data_name: Box<str> = ctx.resolve_symbol(name.name).into();
    if ctx.data_def(&data_name).is_none() {
        let mut variant_map = BTreeMap::<Box<str>, super::super::DataVariantDef>::new();
        for variant in ctx.variants(variants.clone()) {
            let tag: Box<str> = ctx.resolve_symbol(variant.name.name).into();
            let payload = variant.arg.map(|expr| {
                let origin = ctx.expr(expr).origin;
                lower_type_expr(ctx, expr, origin)
            });
            let prev = variant_map.insert(tag, super::super::DataVariantDef { payload });
            if prev.is_some() {
                ctx.diag(variant.origin.span, "duplicate data variant", "");
            }
        }
        let key = surface_key(ctx.module_key(), ctx.interner(), name.name);
        ctx.insert_data_def(
            data_name,
            super::super::DataDef {
                key,
                variants: variant_map,
            },
        );
    }
    check_data_expr(ctx, variants, fields)
}

pub(super) fn check_bound_effect(
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
            .collect::<BTreeMap<_, _>>();
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

pub(super) fn check_bound_class(
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

pub(in super::super) fn check_attributed_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    attrs: SliceRange<HirAttr>,
    inner: HirExprId,
) -> ExprFacts {
    validate_expr_attrs(ctx, origin, attrs.clone(), inner);
    match ctx.expr(inner).kind {
        HirExprKind::Foreign { abi, decls } => {
            let facts = check_foreign_expr(ctx, origin, abi, decls, Some(attrs));
            ctx.set_expr_facts(inner, facts.clone());
            facts
        }
        HirExprKind::Export { expr, .. } => match ctx.expr(expr).kind {
            HirExprKind::Foreign { abi, decls } => {
                let facts = check_foreign_expr(ctx, origin, abi, decls, Some(attrs));
                ctx.set_expr_facts(expr, facts.clone());
                ctx.set_expr_facts(inner, facts.clone());
                facts
            }
            _ => check_expr(ctx, inner),
        },
        _ => check_expr(ctx, inner),
    }
}
