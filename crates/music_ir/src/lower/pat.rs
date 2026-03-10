//! Pattern lowering: `match`/piecewise -> `Switch`/`Branch` chains.
//!
//! Translates `Expr::Match` into a decision tree of `Branch`/`Switch`
//! instructions. Each pattern arm becomes a sequence of tests followed by
//! bindings and then the arm body.
//!
//! Supported patterns:
//! - `Pat::Wild` — always matches
//! - `Pat::Bind` — binds the scrutinee (or subpart) to a local
//! - `Pat::Lit` — compares scrutinee against a constant
//! - `Pat::Tuple` — destructures a product value by field index
//! - `Pat::Or` — tries left, falls through to right on failure

use music_ast::expr::MatchArm;
use music_ast::lit::Lit;
use music_ast::pat::Pat;
use music_ast::{ExprIdx, PatIdx};
use music_shared::Span;

use crate::constant::IrConstValue;
use crate::error::IrError;
use crate::func::IrLocal;
use crate::inst::{IrBinOp, IrInst, IrLabel, IrOperand, IrPlace, IrRvalue};
use crate::types::{IrType, IrTypeIdx};

use super::expr::{FnLowerCtx, lower_expr};
use super::ty::lower_ty;

/// Lowers a `match` expression into `Branch`/`Switch` chains.
///
/// Evaluates the scrutinee once, then emits a decision tree testing each arm
/// in order. The result of the matched arm is stored into a mutable result
/// local.
pub(super) fn lower_match(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    scrutinee: ExprIdx,
    arms: &[MatchArm],
    span: Span,
) -> Result<IrLocal, IrError> {
    if arms.is_empty() {
        return Err(IrError::UnsupportedExpr);
    }

    let scrut_local = lower_expr(fn_cx, scrutinee)?;

    let result_ty_sema = fn_cx
        .cx
        .sema
        .expr_types
        .get(&expr_idx)
        .copied()
        .ok_or(IrError::UnsupportedExpr)?;
    let ir_result_ty = lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types)?;
    let result_local = fn_cx.fresh_local(ir_result_ty, true, span);
    let merge_lbl = fn_cx.fresh_label();

    let n = arms.len();
    for (i, arm) in arms.iter().enumerate() {
        let is_last = i == n - 1;
        let next_lbl = if is_last {
            merge_lbl
        } else {
            fn_cx.fresh_label()
        };

        emit_match_arm(fn_cx, scrut_local, arm, result_local, merge_lbl, next_lbl)?;

        if !is_last {
            fn_cx.emit(IrInst::Label(next_lbl));
        }
    }

    fn_cx.emit(IrInst::Label(merge_lbl));
    Ok(result_local)
}

/// Emits one match arm: test the pattern, bind variables, evaluate guard,
/// store result, jump to merge.
fn emit_match_arm(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    scrut_local: IrLocal,
    arm: &MatchArm,
    result_local: IrLocal,
    merge_lbl: IrLabel,
    fail_lbl: IrLabel,
) -> Result<(), IrError> {
    emit_pat_test(fn_cx, arm.pat, scrut_local, fail_lbl)?;
    emit_pat_bind(fn_cx, arm.pat, scrut_local)?;

    if let Some(guard_expr) = arm.guard {
        let guard_local = lower_expr(fn_cx, guard_expr)?;
        let then_lbl = fn_cx.fresh_label();
        fn_cx.emit(IrInst::Branch {
            cond: IrOperand::Local(guard_local),
            then_label: then_lbl,
            else_label: fail_lbl,
            span: arm.span,
        });
        fn_cx.emit(IrInst::Label(then_lbl));
    }

    let arm_local = lower_expr(fn_cx, arm.result)?;
    fn_cx.emit(IrInst::Store {
        dst: IrPlace::Local(result_local),
        value: IrOperand::Local(arm_local),
        span: arm.span,
    });
    fn_cx.emit(IrInst::Goto(merge_lbl));
    Ok(())
}

/// Emits tests for a pattern — jumps to `fail_lbl` if the pattern does not
/// match. Does NOT bind any variables (that is `emit_pat_bind`).
fn emit_pat_test(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    pat_idx: PatIdx,
    scrut: IrLocal,
    fail_lbl: IrLabel,
) -> Result<(), IrError> {
    let pat = fn_cx.cx.ast.pats[pat_idx].clone();
    match pat {
        Pat::Wild { .. } | Pat::Bind { inner: None, .. } => {}
        Pat::Bind {
            inner: Some(inner), ..
        } => {
            emit_pat_test(fn_cx, inner, scrut, fail_lbl)?;
        }
        Pat::Lit { lit, span } => {
            emit_lit_test(fn_cx, &lit, scrut, fail_lbl, span)?;
        }
        Pat::Tuple { elems, span } => {
            for (i, &elem_pat) in elems.iter().enumerate() {
                let idx = u32::try_from(i).map_err(|_| IrError::UnsupportedExpr)?;
                let elem_ty = infer_pat_ir_type(fn_cx, elem_pat)?;
                let elem_local = fn_cx.emit_assign(
                    elem_ty,
                    IrRvalue::FieldGet {
                        object: IrOperand::Local(scrut),
                        index: idx,
                    },
                    span,
                );
                emit_pat_test(fn_cx, elem_pat, elem_local, fail_lbl)?;
            }
        }
        Pat::Or { left, right, .. } => {
            let right_lbl = fn_cx.fresh_label();
            let ok_lbl = fn_cx.fresh_label();

            emit_pat_test(fn_cx, left, scrut, right_lbl)?;
            fn_cx.emit(IrInst::Goto(ok_lbl));

            fn_cx.emit(IrInst::Label(right_lbl));
            emit_pat_test(fn_cx, right, scrut, fail_lbl)?;

            fn_cx.emit(IrInst::Label(ok_lbl));
        }
        _ => return Err(IrError::UnsupportedExpr),
    }
    Ok(())
}

/// Emits variable bindings for a pattern. Called after `emit_pat_test`
/// has confirmed the pattern matches.
fn emit_pat_bind(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    pat_idx: PatIdx,
    scrut: IrLocal,
) -> Result<(), IrError> {
    let pat = fn_cx.cx.ast.pats[pat_idx].clone();
    match pat {
        Pat::Wild { .. } | Pat::Lit { .. } => {}
        Pat::Bind { span, inner, .. } => {
            if let Some(&def_id) = fn_cx.cx.sema.resolution.pat_defs.get(&span) {
                let _ = fn_cx.local_map.insert(def_id, scrut);
            }
            if let Some(inner) = inner {
                emit_pat_bind(fn_cx, inner, scrut)?;
            }
        }
        Pat::Tuple { elems, span } => {
            for (i, &elem_pat) in elems.iter().enumerate() {
                let idx = u32::try_from(i).map_err(|_| IrError::UnsupportedExpr)?;
                let elem_ty = infer_pat_ir_type(fn_cx, elem_pat)?;
                let elem_local = fn_cx.emit_assign(
                    elem_ty,
                    IrRvalue::FieldGet {
                        object: IrOperand::Local(scrut),
                        index: idx,
                    },
                    span,
                );
                emit_pat_bind(fn_cx, elem_pat, elem_local)?;
            }
        }
        Pat::Or { left, .. } => {
            emit_pat_bind(fn_cx, left, scrut)?;
        }
        _ => return Err(IrError::UnsupportedExpr),
    }
    Ok(())
}

/// Emits a comparison of the scrutinee against a literal constant.
fn emit_lit_test(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    lit: &Lit,
    scrut: IrLocal,
    fail_lbl: IrLabel,
    span: Span,
) -> Result<(), IrError> {
    let (const_val, cmp_op) = match lit {
        Lit::Int { value, .. } => (IrConstValue::Int(*value), IrBinOp::IEq),
        Lit::Float { value, .. } => (IrConstValue::Float(*value), IrBinOp::FEq),
        Lit::Rune { codepoint, .. } => (IrConstValue::Rune(*codepoint), IrBinOp::IEq),
        Lit::Unit { .. } => return Ok(()),
        Lit::Str { .. } | Lit::FStr { .. } => return Err(IrError::UnsupportedExpr),
    };

    let bool_ty = fn_cx.cx.ir.types.alloc(IrType::Bool);
    let eq_local = fn_cx.emit_assign(
        bool_ty,
        IrRvalue::BinOp {
            op: cmp_op,
            left: IrOperand::Local(scrut),
            right: IrOperand::Const(const_val),
        },
        span,
    );

    let then_lbl = fn_cx.fresh_label();
    fn_cx.emit(IrInst::Branch {
        cond: IrOperand::Local(eq_local),
        then_label: then_lbl,
        else_label: fail_lbl,
        span,
    });
    fn_cx.emit(IrInst::Label(then_lbl));
    Ok(())
}

/// Infers the IR type for the value matched by a pattern.
///
/// Uses the scrutinee's type context to determine element types for
/// destructuring patterns.
fn infer_pat_ir_type(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    pat_idx: PatIdx,
) -> Result<IrTypeIdx, IrError> {
    let pat = fn_cx.cx.ast.pats[pat_idx].clone();
    match pat {
        Pat::Bind { span, .. } => {
            if let Some(&def_id) = fn_cx.cx.sema.resolution.pat_defs.get(&span) {
                let def_idx = usize::try_from(def_id.0).map_err(|_| IrError::UnsupportedExpr)?;
                if let Some(ty) = fn_cx.cx.sema.defs[def_idx].ty_info.ty {
                    return lower_ty(ty, fn_cx.cx.sema, &mut fn_cx.cx.ir.types);
                }
            }
            Err(IrError::UnsupportedExpr)
        }
        Pat::Wild { .. } => {
            let unit_ty = fn_cx.cx.ir.types.alloc(IrType::Unit);
            Ok(unit_ty)
        }
        Pat::Lit { lit, .. } => {
            let ir_ty = match &lit {
                Lit::Int { .. } => IrType::Int64,
                Lit::Float { .. } => IrType::Float64,
                Lit::Rune { .. } => IrType::UInt32,
                Lit::Unit { .. } => IrType::Unit,
                Lit::Str { .. } | Lit::FStr { .. } => IrType::UInt64,
            };
            Ok(fn_cx.cx.ir.types.alloc(ir_ty))
        }
        _ => Err(IrError::UnsupportedExpr),
    }
}
