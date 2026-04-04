use std::collections::{BTreeSet, HashMap};

use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirConstraint, HirEffectSet, HirExprId, HirExprKind, HirFieldDef, HirForeignDecl,
    HirHandleClause, HirLetMods, HirMemberDef, HirMemberKind, HirOrigin, HirParam, HirPatId,
    HirTyId, HirTyKind, HirVariantDef,
};
use music_names::{Ident, Symbol};

use crate::api::{ClassFacts, ClassMemberFacts, ExprFacts, InstanceFacts};
use crate::attrs::{validate_expr_attrs, validate_foreign_decl};
use crate::context::{CheckPass, EffectDef, EffectOpDef, PassBase, ResumeCtx};
use crate::effects::{EffectKey, EffectRow};
use crate::exprs::check_expr;
use crate::normalize::{
    lower_constraints, lower_effect_row, lower_params, lower_type_expr, type_mismatch,
};
use crate::patterns::{bind_pat, bound_name_from_pat};

pub struct LetExprInput {
    pub origin: HirOrigin,
    pub mods: HirLetMods,
    pub pat: HirPatId,
    pub params: SliceRange<HirParam>,
    pub constraints: SliceRange<HirConstraint>,
    pub effects: Option<HirEffectSet>,
    pub sig: Option<HirExprId>,
    pub value: HirExprId,
}

pub fn member_signature(
    ctx: &mut PassBase<'_, '_>,
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

pub fn check_let_expr(ctx: &mut CheckPass<'_, '_>, input: LetExprInput) -> ExprFacts {
    let builtins = ctx.builtins();
    let LetExprInput {
        origin,
        mods,
        pat,
        params,
        constraints,
        effects,
        sig,
        value,
    } = input;
    let bound_name = bound_name_from_pat(ctx, pat);
    let declared_ty = sig.map(|expr| {
        let origin = ctx.expr(expr).origin;
        lower_type_expr(ctx, expr, origin)
    });

    if mods.is_rec {
        if let Some(name) = bound_name {
            if let Some(binding) = ctx.binding_id_for_decl(name) {
                ctx.insert_binding_type(binding, declared_ty.unwrap_or(builtins.unknown));
            }
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
        let param_types = lower_params(ctx, params);
        let mut callable_effects = effects
            .as_ref()
            .map_or(EffectRow::empty(), |set| lower_effect_row(ctx, set));
        let body_facts = check_expr(ctx, value);
        if effects.is_none() {
            callable_effects = body_facts.effects.clone();
        } else {
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
        if let Some(binding) = bound_name.and_then(|ident| ctx.binding_id_for_decl(ident)) {
            ctx.insert_binding_type(binding, ty);
            ctx.insert_binding_effects(binding, callable_effects);
        }
        ty
    } else {
        let ty = declared_ty.unwrap_or(value_facts.ty);
        type_mismatch(ctx, origin, ty, value_facts.ty);
        ty
    };

    let _constraints = lower_constraints(ctx, constraints);
    bind_pat(ctx, pat, final_ty);
    ExprFacts {
        ty: builtins.unit,
        effects: EffectRow::empty(),
    }
}

pub fn check_import_expr(ctx: &mut CheckPass<'_, '_>, arg: HirExprId) -> ExprFacts {
    let builtins = ctx.builtins();
    let arg_facts = check_expr(ctx, arg);
    let origin = ctx.expr(arg).origin;
    type_mismatch(ctx, origin, builtins.string_, arg_facts.ty);
    ExprFacts {
        ty: builtins.module,
        effects: arg_facts.effects,
    }
}

pub fn check_data_expr(
    ctx: &mut CheckPass<'_, '_>,
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
    ctx: &mut CheckPass<'_, '_>,
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
    ctx: &mut CheckPass<'_, '_>,
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
    ctx: &mut CheckPass<'_, '_>,
    expr_id: HirExprId,
    origin: HirOrigin,
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

    if ctx.class_id(class_name).is_none() {
        ctx.diag(origin.span, "unknown class", "");
    }

    let members_vec = ctx.members((*members).clone());
    let member_names = members_vec
        .iter()
        .filter(|member| member.kind == HirMemberKind::Let)
        .map(|member| member.name.name)
        .collect::<Vec<_>>();
    for member in &members_vec {
        if member.kind != HirMemberKind::Let {
            continue;
        }
        let signature = member_signature(ctx, member, true);
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

    let constraints = lower_constraints(ctx, constraints);
    ctx.insert_instance_facts(
        expr_id,
        InstanceFacts {
            origin,
            class_name,
            class_args,
            constraints,
            member_names: member_names.into_boxed_slice(),
        },
    );
    ExprFacts {
        ty: class_ty,
        effects: EffectRow::empty(),
    }
}

pub fn check_foreign_expr(
    ctx: &mut CheckPass<'_, '_>,
    abi: Option<Box<str>>,
    decls: SliceRange<HirForeignDecl>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    let abi = abi.unwrap_or_else(|| Box::<str>::from("c"));
    for decl in ctx.foreign_decls(decls) {
        validate_foreign_decl(ctx, &decl, &abi);
    }
    ExprFacts {
        ty: builtins.unit,
        effects: EffectRow::empty(),
    }
}

pub fn check_perform_expr(
    ctx: &mut CheckPass<'_, '_>,
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
    ExprFacts {
        ty: op_def.result,
        effects,
    }
}

pub fn check_handle_expr(
    ctx: &mut CheckPass<'_, '_>,
    origin: HirOrigin,
    expr: HirExprId,
    handler: Ident,
    clauses: SliceRange<HirHandleClause>,
) -> ExprFacts {
    let handled_facts = check_expr(ctx, expr);
    let Some(effect) = ctx.effect_def(handler.name).cloned() else {
        ctx.diag(origin.span, "unknown effect", "");
        return handled_facts;
    };

    let value_symbol = ctx.intern("value");
    let mut result_ty = handled_facts.ty;
    let mut clause_effects = EffectRow::empty();
    let mut seen_value = 0usize;
    let mut seen_ops = BTreeSet::new();

    for clause in ctx.handle_clauses(clauses) {
        if clause.op.name == value_symbol {
            seen_value = seen_value.saturating_add(1);
            let facts = check_expr(ctx, clause.body);
            let origin = ctx.expr(clause.body).origin;
            type_mismatch(ctx, origin, handled_facts.ty, facts.ty);
            clause_effects.union_with(&facts.effects);
            result_ty = facts.ty;
            continue;
        }

        let _did_insert = seen_ops.insert(clause.op.name);
        let Some(op_def) = effect.ops.get(&clause.op.name).cloned() else {
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
    effects.remove_by_name(handler.name);
    effects.union_with(&clause_effects);
    ExprFacts {
        ty: result_ty,
        effects,
    }
}

pub fn check_resume_expr(
    ctx: &mut CheckPass<'_, '_>,
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

pub fn call_effects_for_expr(ctx: &CheckPass<'_, '_>, expr: HirExprId) -> Option<EffectRow> {
    match ctx.expr(expr).kind {
        HirExprKind::Name { name } => ctx
            .binding_id_for_use(name)
            .and_then(|binding| ctx.binding_effects(binding)),
        _ => None,
    }
}

pub fn require_declared_effects(
    ctx: &mut CheckPass<'_, '_>,
    origin: HirOrigin,
    declared: &EffectRow,
    actual: &EffectRow,
) {
    if declared.open.is_some() {
        return;
    }
    for item in &actual.items {
        if declared.items.contains(item) {
            continue;
        }
        ctx.diag(origin.span, "effect not declared", "");
    }
}

fn check_bound_data(
    ctx: &mut CheckPass<'_, '_>,
    _name: Ident,
    variants: SliceRange<HirVariantDef>,
    fields: SliceRange<HirFieldDef>,
) -> ExprFacts {
    check_data_expr(ctx, variants, fields)
}

fn check_bound_effect(
    ctx: &mut CheckPass<'_, '_>,
    expr_id: HirExprId,
    name: Ident,
    members: SliceRange<HirMemberDef>,
) -> ExprFacts {
    let builtins = ctx.builtins();
    if ctx.effect_def(name.name).is_none() {
        let ops = ctx
            .members(members.clone())
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
    ctx: &mut CheckPass<'_, '_>,
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
        ctx.insert_class_facts(
            expr_id,
            ClassFacts {
                name: name.name,
                constraints: constraints_facts,
                members: class_members,
                laws,
            },
        );
    }
    check_class_expr(ctx, expr_id, constraints, members, Some(name))
}

fn effect_op_call(ctx: &mut CheckPass<'_, '_>, expr: HirExprId) -> Option<(Symbol, EffectOpDef)> {
    let HirExprKind::Call { callee, args } = ctx.expr(expr).kind else {
        return None;
    };
    let HirExprKind::Field { base, name, .. } = ctx.expr(callee).kind else {
        return None;
    };
    let HirExprKind::Name { name: effect_name } = ctx.expr(base).kind else {
        return None;
    };
    let op = ctx
        .effect_def(effect_name.name)
        .and_then(|effect| effect.ops.get(&name.name))
        .cloned()?;
    let found = ctx.arg_count(args);
    if found != op.params.len() {
        let span = ctx.expr(expr).origin.span;
        ctx.diag(span, "perform arity mismatch", "");
    }
    Some((effect_name.name, op))
}

pub fn check_attributed_expr(
    ctx: &mut CheckPass<'_, '_>,
    origin: HirOrigin,
    attrs: SliceRange<HirAttr>,
    inner: HirExprId,
) -> ExprFacts {
    validate_expr_attrs(ctx, origin, attrs, inner);
    check_expr(ctx, inner)
}
