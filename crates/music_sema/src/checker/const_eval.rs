use std::collections::HashSet;

use music_base::{diag::DiagContext, parse_i64_literal};
use music_hir::{
    HirBinaryOp, HirExprId, HirExprKind, HirLitKind, HirPatId, HirPatKind, HirPrefixOp,
};
use music_names::{Ident, NameBindingId};

use super::{DiagKind, PassBase};
use crate::api::ComptimeValue;

#[derive(Clone, Copy, PartialEq, Eq)]
enum ConstEvalError {
    Invalid,
    Cycle,
}

pub(super) fn record_data_variant_tag(seen: &mut HashSet<i64>, tag: i64) -> bool {
    seen.insert(tag)
}

pub(super) fn data_variant_tag(
    ctx: &mut PassBase<'_, '_, '_>,
    expr: Option<HirExprId>,
    implicit: i64,
) -> i64 {
    let Some(expr) = expr else {
        return implicit;
    };
    match ConstIntEvaluator::new(ctx).eval(expr) {
        Ok(ComptimeValue::Int(value)) => value,
        Ok(_) => {
            let discriminant = ctx.expr_subject(expr);
            ctx.diag_with(
                ctx.expr(expr).origin.span,
                DiagKind::InvalidDataVariantDiscriminant,
                DiagContext::new().with("discriminant", discriminant),
            );
            implicit
        }
        Err(error) => {
            let kind = match error {
                ConstEvalError::Invalid => DiagKind::InvalidDataVariantDiscriminant,
                ConstEvalError::Cycle => DiagKind::CyclicDataVariantDiscriminant,
            };
            if matches!(kind, DiagKind::InvalidDataVariantDiscriminant) {
                ctx.diag_with(
                    ctx.expr(expr).origin.span,
                    kind,
                    DiagContext::new().with("discriminant", ctx.expr_subject(expr)),
                );
            } else {
                ctx.diag(ctx.expr(expr).origin.span, kind, "");
            }
            implicit
        }
    }
}

pub(super) fn try_comptime_value(
    ctx: &mut PassBase<'_, '_, '_>,
    expr: HirExprId,
) -> Option<ComptimeValue> {
    ConstIntEvaluator::new(ctx).eval(expr).ok()
}

struct ConstIntEvaluator<'ctx, 'a, 'b, 'c> {
    ctx: &'ctx mut PassBase<'a, 'b, 'c>,
    seen: HashSet<NameBindingId>,
}

impl<'ctx, 'a, 'b, 'c> ConstIntEvaluator<'ctx, 'a, 'b, 'c> {
    fn new(ctx: &'ctx mut PassBase<'a, 'b, 'c>) -> Self {
        Self {
            ctx,
            seen: HashSet::new(),
        }
    }

    fn eval(&mut self, expr: HirExprId) -> Result<ComptimeValue, ConstEvalError> {
        match self.ctx.expr(expr).kind {
            HirExprKind::Lit { lit } => Self::eval_lit(self.ctx.lit_kind(lit)),
            HirExprKind::Name { name } => self.eval_name(name),
            HirExprKind::Prefix { op, expr } => self.eval_prefix(&op, expr),
            HirExprKind::Binary { op, left, right } => self.eval_binary(&op, left, right),
            HirExprKind::Sequence { exprs } => {
                let exprs = self.ctx.expr_ids(exprs);
                if exprs.len() == 1 {
                    self.eval(exprs[0])
                } else {
                    Err(ConstEvalError::Invalid)
                }
            }
            HirExprKind::Tuple { items } => {
                let items = self.ctx.expr_ids(items);
                match items.as_slice() {
                    [] => Ok(ComptimeValue::Unit),
                    [item] => self.eval(*item),
                    _ => Err(ConstEvalError::Invalid),
                }
            }
            _ => Err(ConstEvalError::Invalid),
        }
    }

    fn eval_lit(lit: HirLitKind) -> Result<ComptimeValue, ConstEvalError> {
        match lit {
            HirLitKind::Int { raw } => parse_i64_literal(&raw)
                .map(ComptimeValue::Int)
                .ok_or(ConstEvalError::Invalid),
            HirLitKind::Float { raw } => Ok(ComptimeValue::Float(raw)),
            HirLitKind::String { value } => Ok(ComptimeValue::String(value)),
            HirLitKind::Rune { value } => Ok(ComptimeValue::Rune(value)),
        }
    }

    fn eval_name(&mut self, name: Ident) -> Result<ComptimeValue, ConstEvalError> {
        let binding = self
            .ctx
            .binding_id_for_use(name)
            .ok_or(ConstEvalError::Invalid)?;
        if let Some(value) = self.ctx.binding_comptime_value(binding) {
            return Ok(value.clone());
        }
        if !self.seen.insert(binding) {
            return Err(ConstEvalError::Cycle);
        }
        let root = self.ctx.root_expr_id();
        let value_expr = self
            .binding_value_expr(root, binding)
            .ok_or(ConstEvalError::Invalid)?;
        if !self.is_explicit_comptime_expr(value_expr) {
            let _ = self.seen.remove(&binding);
            return Err(ConstEvalError::Invalid);
        }
        let evaluated_value = self.eval(value_expr);
        let _ = self.seen.remove(&binding);
        evaluated_value
    }

    fn eval_prefix(
        &mut self,
        op: &HirPrefixOp,
        expr: HirExprId,
    ) -> Result<ComptimeValue, ConstEvalError> {
        match op {
            HirPrefixOp::Neg => self
                .eval_int(expr)?
                .checked_neg()
                .map(ComptimeValue::Int)
                .ok_or(ConstEvalError::Invalid),
            HirPrefixOp::Known => self.eval(expr),
            HirPrefixOp::Not | HirPrefixOp::Mut => Err(ConstEvalError::Invalid),
        }
    }

    fn eval_binary(
        &mut self,
        op: &HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    ) -> Result<ComptimeValue, ConstEvalError> {
        let left = self.eval_int(left)?;
        let right = self.eval_int(right)?;
        match op {
            HirBinaryOp::Add => left.checked_add(right),
            HirBinaryOp::Sub => left.checked_sub(right),
            HirBinaryOp::Mul => left.checked_mul(right),
            HirBinaryOp::Div if right != 0 => left.checked_div(right),
            HirBinaryOp::Rem if right != 0 => left.checked_rem(right),
            HirBinaryOp::UserOp(ident) => match self.ctx.resolve_symbol(ident.name) {
                "+" => left.checked_add(right),
                "-" => left.checked_sub(right),
                "*" => left.checked_mul(right),
                "/" if right != 0 => left.checked_div(right),
                "%" if right != 0 => left.checked_rem(right),
                _ => None,
            },
            _ => None,
        }
        .map(ComptimeValue::Int)
        .ok_or(ConstEvalError::Invalid)
    }

    fn eval_int(&mut self, expr: HirExprId) -> Result<i64, ConstEvalError> {
        match self.eval(expr)? {
            ComptimeValue::Int(value) => Ok(value),
            _ => Err(ConstEvalError::Invalid),
        }
    }

    fn is_explicit_comptime_expr(&self, expr: HirExprId) -> bool {
        matches!(
            self.ctx.expr(expr).kind,
            HirExprKind::Prefix {
                op: HirPrefixOp::Known,
                ..
            }
        )
    }

    fn binding_value_expr(&self, expr_id: HirExprId, binding: NameBindingId) -> Option<HirExprId> {
        match self.ctx.expr(expr_id).kind {
            HirExprKind::Sequence { exprs } => self
                .ctx
                .expr_ids(exprs)
                .into_iter()
                .find_map(|expr| self.binding_value_expr(expr, binding)),
            HirExprKind::Let { pat, value, .. } => self
                .pat_binds(pat, binding)
                .then_some(value)
                .or_else(|| self.binding_value_expr(value, binding)),
            _ => None,
        }
    }

    fn pat_binds(&self, pat_id: HirPatId, binding: NameBindingId) -> bool {
        match self.ctx.pat(pat_id).kind {
            HirPatKind::Bind { name } => self.ctx.binding_id_for_decl(name) == Some(binding),
            _ => false,
        }
    }
}
