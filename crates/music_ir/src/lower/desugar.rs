//! Desugaring pass: short-circuit `and`/`or`, `assign` (`<-`), `pipe`, f-string.
//!
//! These operators cannot be lowered as simple binary operations because
//! they require control flow (short-circuit) or mutation semantics.
//! This module provides helpers called from the main expression lowering.

use music_ast::Expr;
use music_ast::ExprIdx;
use music_ast::lit::FStrPart;
use music_shared::Span;

use crate::constant::IrConstValue;
use crate::error::{IrError, SpannedIrError};
use crate::func::IrLocal;
use crate::inst::{IrCallee, IrInst, IrOperand, IrPlace, IrRvalue};
use crate::types::IrType;

use super::expr::{FnLowerCtx, expr_span, lower_expr};
use super::ty::lower_ty;

/// Lowers `a and b` as `(let t := a; if t then b else false)`.
pub(super) fn lower_and(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    left: ExprIdx,
    right: ExprIdx,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
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
) -> Result<IrLocal, SpannedIrError> {
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
) -> Result<IrLocal, SpannedIrError> {
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
fn lower_place(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
) -> Result<IrPlace, SpannedIrError> {
    let expr = fn_cx.cx.ast.exprs[expr_idx].clone();
    let span = expr_span(&expr);
    match expr {
        Expr::Name { .. } => {
            let Some(&def_id) = fn_cx.cx.expr_defs.get(&expr_idx) else {
                return Err(IrError::UnresolvedName.at(span));
            };
            let Some(&local) = fn_cx.local_map.get(&def_id) else {
                return Err(IrError::UnresolvedName.at(span));
            };
            Ok(IrPlace::Local(local))
        }
        Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => lower_place(fn_cx, inner),
        _ => Err(IrError::UnsupportedExpr {
            kind: "non-name assignment target",
        }
        .at(span)),
    }
}

/// Lowers `a |> f` as `f(a)`.
pub(super) fn lower_pipe(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    left: ExprIdx,
    right: ExprIdx,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let arg_local = lower_expr(fn_cx, left)?;

    let result_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_result_ty =
        lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    // Check if RHS is a named function reference.
    if let Some(&callee_def_id) = fn_cx.cx.expr_defs.get(&right) {
        if let Some(&ffi_idx) = fn_cx.cx.foreign_fn_map.get(&callee_def_id) {
            return Ok(fn_cx.emit_assign(
                ir_result_ty,
                IrRvalue::ForeignCall {
                    fn_idx: ffi_idx,
                    args: vec![IrOperand::Local(arg_local)],
                },
                span,
            ));
        }
        if let Some(&ir_fn_idx) = fn_cx.cx.fn_map.get(&callee_def_id) {
            return Ok(fn_cx.emit_assign(
                ir_result_ty,
                IrRvalue::Call {
                    callee: IrCallee::Direct(ir_fn_idx),
                    args: vec![IrOperand::Local(arg_local)],
                    tail: false,
                },
                span,
            ));
        }
    }

    // Otherwise lower RHS as an expression and do indirect call.
    let callee_local = lower_expr(fn_cx, right)?;
    Ok(fn_cx.emit_assign(
        ir_result_ty,
        IrRvalue::Call {
            callee: IrCallee::Indirect(callee_local),
            args: vec![IrOperand::Local(arg_local)],
            tail: false,
        },
        span,
    ))
}

/// Lowers an f-string `f"text {expr} text"` into `show` + `str_cat` calls.
pub(super) fn lower_fstr(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    parts: &[FStrPart],
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let show_idx =
        super::stmt::show_ffi_idx(fn_cx.cx).ok_or_else(|| IrError::UnresolvedName.at(span))?;
    let str_cat_idx =
        super::stmt::str_cat_ffi_idx(fn_cx.cx).ok_or_else(|| IrError::UnresolvedName.at(span))?;

    let any_ty = fn_cx.cx.ir.types.alloc(IrType::Any);

    let mut acc: Option<IrLocal> = None;

    for part in parts {
        let part_local = match part {
            FStrPart::Text { raw, .. } => {
                fn_cx.emit_assign(any_ty, IrRvalue::Const(IrConstValue::Str(*raw)), span)
            }
            FStrPart::Interpolated { expr, .. } => {
                let expr_local = lower_expr(fn_cx, *expr)?;
                fn_cx.emit_assign(
                    any_ty,
                    IrRvalue::ForeignCall {
                        fn_idx: show_idx,
                        args: vec![IrOperand::Local(expr_local)],
                    },
                    span,
                )
            }
        };

        acc = Some(acc.map_or(part_local, |prev| {
            fn_cx.emit_assign(
                any_ty,
                IrRvalue::ForeignCall {
                    fn_idx: str_cat_idx,
                    args: vec![IrOperand::Local(prev), IrOperand::Local(part_local)],
                },
                span,
            )
        }));
    }

    acc.ok_or_else(|| {
        IrError::UnsupportedExpr {
            kind: "empty interpolated string",
        }
        .at(span)
    })
}
