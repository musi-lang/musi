use std::collections::HashSet;

use music_base::parse_i64_literal;
use music_hir::{
    HirBinaryOp, HirExprId, HirExprKind, HirLitKind, HirPatId, HirPatKind, HirPrefixOp,
    HirQuoteKind, HirSpliceKind,
};
use music_names::{Ident, NameBindingId};
use music_term::{SyntaxShape, SyntaxTerm};

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
            ctx.diag(
                ctx.expr(expr).origin.span,
                DiagKind::InvalidDataVariantDiscriminant,
                "",
            );
            implicit
        }
        Err(error) => {
            let kind = match error {
                ConstEvalError::Invalid => DiagKind::InvalidDataVariantDiscriminant,
                ConstEvalError::Cycle => DiagKind::CyclicDataVariantDiscriminant,
            };
            ctx.diag(ctx.expr(expr).origin.span, kind, "");
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
            HirExprKind::Quote { kind } => self.eval_quote(&kind),
            HirExprKind::Splice { kind } => self.eval_splice(&kind),
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

    fn eval_quote(&mut self, kind: &HirQuoteKind) -> Result<ComptimeValue, ConstEvalError> {
        let (shape, raw) = match kind {
            HirQuoteKind::Expr { expr, raw } => {
                let raw = self.replace_expr_splices(*expr, raw)?;
                (SyntaxShape::Expr, raw)
            }
            HirQuoteKind::Block { exprs, raw } => {
                let mut raw = raw.to_string();
                for expr in self.ctx.expr_ids(*exprs) {
                    raw = self.replace_splices(expr, &raw)?.to_string();
                }
                (SyntaxShape::Module, raw.into_boxed_str())
            }
        };
        SyntaxTerm::from_quote_source(&raw)
            .or_else(|_| SyntaxTerm::parse(shape, raw.as_ref()))
            .map(ComptimeValue::Syntax)
            .map_err(|_| ConstEvalError::Invalid)
    }

    fn replace_expr_splices(
        &mut self,
        expr: HirExprId,
        raw: &str,
    ) -> Result<Box<str>, ConstEvalError> {
        self.replace_splices(expr, raw)
    }

    fn replace_splices(&mut self, expr: HirExprId, raw: &str) -> Result<Box<str>, ConstEvalError> {
        let mut rendered = raw.to_owned();
        self.replace_splices_in_expr(expr, &mut rendered)?;
        Ok(rendered.into_boxed_str())
    }

    fn replace_splices_in_expr(
        &mut self,
        expr: HirExprId,
        raw: &mut String,
    ) -> Result<(), ConstEvalError> {
        match self.ctx.expr(expr).kind {
            HirExprKind::Splice { kind } => {
                let replacement = self.eval_splice(&kind).and_then(Self::value_to_syntax)?;
                let splice_raw = match kind {
                    HirSpliceKind::Name { raw, .. }
                    | HirSpliceKind::Expr { raw, .. }
                    | HirSpliceKind::Exprs { raw, .. } => raw,
                };
                *raw = raw.replace(splice_raw.as_ref(), replacement.as_ref());
            }
            HirExprKind::Binary { left, right, .. } => {
                self.replace_splices_in_expr(left, raw)?;
                self.replace_splices_in_expr(right, raw)?;
            }
            HirExprKind::Prefix { expr, .. }
            | HirExprKind::Field { base: expr, .. }
            | HirExprKind::Index { base: expr, .. }
            | HirExprKind::TypeTest { base: expr, .. }
            | HirExprKind::TypeCast { base: expr, .. } => {
                self.replace_splices_in_expr(expr, raw)?;
            }
            HirExprKind::Tuple { items } => {
                for item in self.ctx.expr_ids(items) {
                    self.replace_splices_in_expr(item, raw)?;
                }
            }
            HirExprKind::Array { items } => {
                for item in self.ctx.array_items(items) {
                    self.replace_splices_in_expr(item.expr, raw)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn eval_splice(&mut self, kind: &HirSpliceKind) -> Result<ComptimeValue, ConstEvalError> {
        match kind {
            HirSpliceKind::Name { name, .. } => self.eval_name(*name),
            HirSpliceKind::Expr { expr, .. } => self.eval(*expr),
            HirSpliceKind::Exprs { .. } => Err(ConstEvalError::Invalid),
        }
    }

    fn value_to_syntax(value: ComptimeValue) -> Result<Box<str>, ConstEvalError> {
        match value {
            ComptimeValue::Int(value) => Ok(value.to_string().into_boxed_str()),
            ComptimeValue::Nat(value) => Ok(value.to_string().into_boxed_str()),
            ComptimeValue::Float(raw) => Ok(raw),
            ComptimeValue::String(value) => Ok(format!("{value:?}").into_boxed_str()),
            ComptimeValue::Rune(value) => {
                let rune = char::from_u32(value).ok_or(ConstEvalError::Invalid)?;
                Ok(format!("{rune:?}").into_boxed_str())
            }
            ComptimeValue::CPtr(value) => Ok(value.to_string().into_boxed_str()),
            ComptimeValue::Syntax(term) => Ok(term.text().into()),
            ComptimeValue::Unit => Ok("()".into()),
            ComptimeValue::Seq(value) => {
                let items = value
                    .items
                    .iter()
                    .map(|item| Self::value_to_syntax(item.clone()))
                    .collect::<Result<Vec<_>, _>>()?
                    .join(", ");
                Ok(format!("[{items}]").into_boxed_str())
            }
            ComptimeValue::Data(_)
            | ComptimeValue::Closure(_)
            | ComptimeValue::Continuation(_)
            | ComptimeValue::Type(_)
            | ComptimeValue::Module(_)
            | ComptimeValue::Foreign(_)
            | ComptimeValue::Effect(_)
            | ComptimeValue::Class(_) => Err(ConstEvalError::Invalid),
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
        let value = self.eval(value_expr);
        let _ = self.seen.remove(&binding);
        value
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
            HirPrefixOp::Comptime => self.eval(expr),
            HirPrefixOp::Not | HirPrefixOp::Mut | HirPrefixOp::Any | HirPrefixOp::Some => {
                Err(ConstEvalError::Invalid)
            }
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
                op: HirPrefixOp::Comptime,
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
