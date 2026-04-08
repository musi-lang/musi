use std::collections::BTreeSet;

use music_arena::SliceRange;
use music_hir::{HirExprId, HirExprKind, HirHandleClause, HirOrigin, HirTyKind};
use music_names::Ident;

use super::super::exprs::check_expr;
use super::super::normalize::type_mismatch;
use super::super::{CheckPass, ResumeCtx};
use crate::api::ExprFacts;
use crate::effects::{EffectKey, EffectRow};

pub(in super::super) fn check_perform_expr(
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
    ExprFacts {
        ty: op_def.result,
        effects,
    }
}

pub(in super::super) fn check_handle_expr(
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
    let mut result_ty = ctx.builtins().unknown;
    let mut clause_effects = EffectRow::empty();
    let mut seen_value = 0usize;
    let mut seen_ops = BTreeSet::new();

    let clauses_vec = ctx.handle_clauses(clauses);

    for clause in &clauses_vec {
        let clause_name: Box<str> = ctx.resolve_symbol(clause.op.name).into();
        if clause_name.as_ref() == value_name {
            seen_value = seen_value.saturating_add(1);
            if let Some(binding) = ctx.binding_id_for_decl(clause.op) {
                ctx.insert_binding_type(binding, handled_facts.ty);
            }
            let facts = check_expr(ctx, clause.body);
            clause_effects.union_with(&facts.effects);
            result_ty = facts.ty;
        }
    }

    for clause in clauses_vec {
        let clause_name: Box<str> = ctx.resolve_symbol(clause.op.name).into();
        if clause_name.as_ref() == value_name {
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
        if params.len() != op_def.params.len().saturating_add(1) {
            ctx.diag(origin.span, "handler clause arity mismatch", "");
        }
        let (args, cont) = if params.is_empty() {
            (Vec::<Ident>::new(), None)
        } else {
            let split = params.len().saturating_sub(1);
            (params[0..split].to_vec(), params.last().copied())
        };
        for (ident, ty) in args.into_iter().zip(op_def.params.iter().copied()) {
            if let Some(binding) = ctx.binding_id_for_decl(ident) {
                ctx.insert_binding_type(binding, ty);
            }
        }
        if let Some(cont) = cont
            && let Some(binding) = ctx.binding_id_for_decl(cont)
        {
            let params = ctx.alloc_ty_list([op_def.result]);
            let cont_ty = ctx.alloc_ty(HirTyKind::Arrow {
                params,
                ret: result_ty,
                is_effectful: true,
            });
            ctx.insert_binding_type(binding, cont_ty);
        }
        ctx.push_resume(ResumeCtx {
            arg: op_def.result,
            result: result_ty,
        });
        let body = check_expr(ctx, clause.body);
        let _ = ctx.pop_resume();
        let origin = ctx.expr(clause.body).origin;
        type_mismatch(ctx, origin, result_ty, body.ty);
        clause_effects.union_with(&body.effects);
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

pub(in super::super) fn check_resume_expr(
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

pub(in super::super) fn call_effects_for_expr(
    ctx: &CheckPass<'_, '_, '_>,
    expr: HirExprId,
) -> Option<EffectRow> {
    match ctx.expr(expr).kind {
        HirExprKind::Name { name } => ctx
            .binding_id_for_use(name)
            .and_then(|binding| ctx.binding_effects(binding)),
        HirExprKind::Apply { .. } => ctx.expr_callable_effects(expr),
        _ => None,
    }
}

pub(super) fn require_declared_effects(
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

fn effect_op_call(
    ctx: &CheckPass<'_, '_, '_>,
    expr: HirExprId,
) -> Option<(Box<str>, super::super::EffectOpDef)> {
    let HirExprKind::Call { callee, args: _ } = ctx.expr(expr).kind else {
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
    Some((effect_name_text, op))
}
