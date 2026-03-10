//! Desugaring pass: short-circuit `and`/`or`, `assign` (`<-`).
//!
//! These operators cannot be lowered as simple binary operations because
//! they require control flow (short-circuit) or mutation semantics.
//! This module provides helpers called from the main expression lowering.

use music_ast::Expr;
use music_ast::ExprIdx;
use music_shared::Span;

use crate::constant::IrConstValue;
use crate::error::IrError;
use crate::func::IrLocal;
use crate::inst::{IrInst, IrOperand, IrPlace, IrRvalue};
use crate::types::IrType;

use super::expr::{FnLowerCtx, lower_expr};

/// Lowers `a and b` as `(let t := a; if t then b else false)`.
pub(super) fn lower_and(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    left: ExprIdx,
    right: ExprIdx,
    span: Span,
) -> Result<IrLocal, IrError> {
    let bool_ty = fn_cx.cx.ir.types.alloc(IrType::Bool);
    let result = fn_cx.fresh_local(bool_ty, true, span);

    let left_local = lower_expr(fn_cx, left)?;

    let then_lbl = fn_cx.fresh_label();
    let else_lbl = fn_cx.fresh_label();
    let merge_lbl = fn_cx.fresh_label();

    fn_cx.emit(IrInst::Branch {
        cond: IrOperand::Local(left_local),
        then_label: then_lbl,
        else_label: else_lbl,
        span,
    });

    fn_cx.emit(IrInst::Label(then_lbl));
    let right_local = lower_expr(fn_cx, right)?;
    fn_cx.emit(IrInst::Store {
        dst: IrPlace::Local(result),
        value: IrOperand::Local(right_local),
        span,
    });
    fn_cx.emit(IrInst::Goto(merge_lbl));

    fn_cx.emit(IrInst::Label(else_lbl));
    let false_local = fn_cx.emit_assign(bool_ty, IrRvalue::Const(IrConstValue::Bool(false)), span);
    fn_cx.emit(IrInst::Store {
        dst: IrPlace::Local(result),
        value: IrOperand::Local(false_local),
        span,
    });
    fn_cx.emit(IrInst::Goto(merge_lbl));

    fn_cx.emit(IrInst::Label(merge_lbl));
    Ok(result)
}

/// Lowers `a or b` as `(let t := a; if t then true else b)`.
pub(super) fn lower_or(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    left: ExprIdx,
    right: ExprIdx,
    span: Span,
) -> Result<IrLocal, IrError> {
    let bool_ty = fn_cx.cx.ir.types.alloc(IrType::Bool);
    let result = fn_cx.fresh_local(bool_ty, true, span);

    let left_local = lower_expr(fn_cx, left)?;

    let then_lbl = fn_cx.fresh_label();
    let else_lbl = fn_cx.fresh_label();
    let merge_lbl = fn_cx.fresh_label();

    fn_cx.emit(IrInst::Branch {
        cond: IrOperand::Local(left_local),
        then_label: then_lbl,
        else_label: else_lbl,
        span,
    });

    fn_cx.emit(IrInst::Label(then_lbl));
    let true_local = fn_cx.emit_assign(bool_ty, IrRvalue::Const(IrConstValue::Bool(true)), span);
    fn_cx.emit(IrInst::Store {
        dst: IrPlace::Local(result),
        value: IrOperand::Local(true_local),
        span,
    });
    fn_cx.emit(IrInst::Goto(merge_lbl));

    fn_cx.emit(IrInst::Label(else_lbl));
    let right_local = lower_expr(fn_cx, right)?;
    fn_cx.emit(IrInst::Store {
        dst: IrPlace::Local(result),
        value: IrOperand::Local(right_local),
        span,
    });
    fn_cx.emit(IrInst::Goto(merge_lbl));

    fn_cx.emit(IrInst::Label(merge_lbl));
    Ok(result)
}

/// Lowers `a <- b` (mutation/assignment).
pub(super) fn lower_assign(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    target: ExprIdx,
    value: ExprIdx,
    span: Span,
) -> Result<IrLocal, IrError> {
    let value_local = lower_expr(fn_cx, value)?;
    let place = lower_place(fn_cx, target)?;

    fn_cx.emit(IrInst::Store {
        dst: place,
        value: IrOperand::Local(value_local),
        span,
    });

    let unit_ty = fn_cx.cx.ir.types.alloc(IrType::Unit);
    Ok(fn_cx.emit_assign(unit_ty, IrRvalue::Const(IrConstValue::Unit), span))
}

/// Resolves an expression into an `IrPlace` for mutation.
fn lower_place(fn_cx: &mut FnLowerCtx<'_, '_>, expr_idx: ExprIdx) -> Result<IrPlace, IrError> {
    let expr = fn_cx.cx.ast.exprs[expr_idx].clone();
    match expr {
        Expr::Name { span, .. } => {
            let Some(&def_id) = fn_cx.cx.sema.resolution.expr_defs.get(&expr_idx) else {
                return Err(IrError::UnsupportedExpr);
            };
            let Some(&local) = fn_cx.local_map.get(&def_id) else {
                return Err(IrError::UnsupportedExpr);
            };
            let _ = span;
            Ok(IrPlace::Local(local))
        }
        Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => lower_place(fn_cx, inner),
        _ => Err(IrError::UnsupportedExpr),
    }
}
