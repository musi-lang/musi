use std::collections::BTreeSet;

use music_arena::SliceRange;
use music_hir::{HirExprId, HirExprKind, HirHandleClause, HirOrigin, HirTyKind};
use music_names::Ident;

use super::super::exprs::check_expr;
use super::super::{CheckPass, DiagKind, ResumeCtx};
use crate::api::ExprFacts;
use crate::effects::{EffectKey, EffectRow};

type CheckPassRef<'ctx, 'interner, 'env> = CheckPass<'ctx, 'interner, 'env>;

impl CheckPass<'_, '_, '_> {
    pub(in super::super) fn check_perform_expr(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let inner = check_expr(self, expr);
        let mut effects = inner.effects;
        let Some((effect_name, op_def)) = effect_op_call(self, expr) else {
            self.diag(origin.span, DiagKind::InvalidPerformTarget, "");
            return ExprFacts::new(builtins.unknown, effects);
        };
        effects.add(EffectKey {
            name: effect_name,
            arg: None,
        });
        ExprFacts::new(op_def.result(), effects)
    }

    pub(in super::super) fn check_handle_expr(
        &mut self,
        origin: HirOrigin,
        expr: HirExprId,
        handler: Ident,
        clauses: SliceRange<HirHandleClause>,
    ) -> ExprFacts {
        let handled_facts = check_expr(self, expr);
        let handler_name: Box<str> = self.resolve_symbol(handler.name).into();
        let Some(effect) = self.effect_def(&handler_name).cloned() else {
            self.diag(origin.span, DiagKind::UnknownEffect, "");
            return handled_facts;
        };

        let value_name = "value";
        let mut result_ty = self.builtins().unknown;
        let mut clause_effects = EffectRow::empty();
        let mut seen_value = 0usize;
        let mut seen_ops = BTreeSet::new();
        let clauses_vec = self.handle_clauses(clauses);

        for clause in &clauses_vec {
            let clause_name: Box<str> = self.resolve_symbol(clause.op.name).into();
            if clause_name.as_ref() == value_name {
                seen_value = seen_value.saturating_add(1);
                if let Some(binding) = self.binding_id_for_decl(clause.op) {
                    self.insert_binding_type(binding, handled_facts.ty);
                }
                let facts = check_expr(self, clause.body);
                clause_effects.union_with(&facts.effects);
                result_ty = facts.ty;
            }
        }

        for clause in clauses_vec {
            let clause_name: Box<str> = self.resolve_symbol(clause.op.name).into();
            if clause_name.as_ref() == value_name {
                continue;
            }

            let did_insert = seen_ops.insert(clause_name.clone());
            if !did_insert {
                self.diag(origin.span, DiagKind::DuplicateHandlerClause, "");
            }
            let Some(op_def) = effect.op(clause_name.as_ref()).cloned() else {
                self.diag(origin.span, DiagKind::UnknownEffectOp, "");
                continue;
            };

            let params = self.idents(clause.params);
            if params.len() != op_def.params().len().saturating_add(1) {
                self.diag(origin.span, DiagKind::HandlerClauseArityMismatch, "");
            }
            let (args, cont) = if params.is_empty() {
                (Vec::<Ident>::new(), None)
            } else {
                let split = params.len().saturating_sub(1);
                (params[0..split].to_vec(), params.last().copied())
            };
            for (ident, ty) in args.into_iter().zip(op_def.params().iter().copied()) {
                if let Some(binding) = self.binding_id_for_decl(ident) {
                    self.insert_binding_type(binding, ty);
                }
            }
            if let Some(cont) = cont
                && let Some(binding) = self.binding_id_for_decl(cont)
            {
                let params = self.alloc_ty_list([op_def.result()]);
                let cont_ty = self.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret: result_ty,
                    is_effectful: true,
                });
                self.insert_binding_type(binding, cont_ty);
            }
            self.push_resume(ResumeCtx::new(op_def.result(), result_ty));
            let body = check_expr(self, clause.body);
            let _ = self.pop_resume();
            let origin = self.expr(clause.body).origin;
            self.type_mismatch(origin, result_ty, body.ty);
            clause_effects.union_with(&body.effects);
        }

        if seen_value != 1 {
            self.diag(origin.span, DiagKind::HandleRequiresSingleValueClause, "");
        }
        for (op, _) in effect.ops() {
            if !seen_ops.contains(op) {
                self.diag(origin.span, DiagKind::HandlerMissingOperationClause, "");
            }
        }

        let mut effects = handled_facts.effects;
        effects.remove_by_name(&handler_name);
        effects.union_with(&clause_effects);
        ExprFacts::new(result_ty, effects)
    }

    pub(in super::super) fn check_resume_expr(
        &mut self,
        origin: HirOrigin,
        expr: Option<HirExprId>,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let Some(resume) = self.resume_top() else {
            self.diag(origin.span, DiagKind::ResumeOutsideHandlerClause, "");
            return ExprFacts::new(builtins.unknown, EffectRow::empty());
        };
        let mut effects = EffectRow::empty();
        if let Some(expr) = expr {
            let facts = check_expr(self, expr);
            let origin = self.expr(expr).origin;
            self.type_mismatch(origin, resume.arg, facts.ty);
            effects.union_with(&facts.effects);
        }
        ExprFacts::new(resume.result, effects)
    }
}

pub(in super::super) fn call_effects_for_expr(
    ctx: &CheckPassRef<'_, '_, '_>,
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
        ctx.diag(origin.span, DiagKind::EffectNotDeclared, "");
    }
    if actual.open.is_some() && declared.open.is_none() {
        ctx.diag(origin.span, DiagKind::EffectNotDeclared, "");
    }
    final_row
}

fn effect_op_call(
    ctx: &CheckPassRef<'_, '_, '_>,
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
        .and_then(|effect| effect.op(op_name).cloned())?;
    Some((effect_name_text, op))
}
