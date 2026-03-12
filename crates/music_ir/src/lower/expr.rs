//! Expression lowering: AST `Expr` nodes -> IR instructions in ANF.

use std::collections::HashMap;

use music_ast::expr::{
    Arg, ArrayElem, BinOp, Expr, FieldKey, LetFields, Param, PwArm, PwGuard, RecField, UnaryOp,
};
use music_ast::lit::Lit;
use music_ast::pat::Pat;
use music_ast::{ExprIdx, PatIdx};
use music_sema::types::Type;
use music_sema::{DefId, SemaResult, TypeIdx};
use music_shared::{Span, Symbol};

use crate::constant::IrConstValue;
use crate::error::{IrError, SpannedIrError};
use crate::func::{IrFnId, IrFunction, IrLocal, IrLocalDecl, IrParam, IrParamMode};
use crate::inst::{IrBinOp, IrCallee, IrInst, IrLabel, IrOperand, IrPlace, IrRvalue, IrUnaryOp};
use crate::types::{IrType, IrTypeIdx};

use super::stmt::LowerCtx;
use super::ty::lower_ty;

pub(super) struct FnLowerCtx<'cx, 'src: 'cx> {
    pub cx: &'cx mut LowerCtx<'src>,
    pub local_map: HashMap<DefId, IrLocal>,
    pub locals: Vec<IrLocalDecl>,
    pub body: Vec<IrInst>,
    pub next_local: u32,
    pub next_label: u32,
}

impl FnLowerCtx<'_, '_> {
    pub(super) fn fresh_local(&mut self, ty: IrTypeIdx, mutable: bool, span: Span) -> IrLocal {
        let local = IrLocal(self.next_local);
        self.next_local += 1;
        self.locals.push(IrLocalDecl {
            local,
            ty,
            mutable,
            span,
        });
        local
    }

    pub(super) const fn fresh_label(&mut self) -> IrLabel {
        let label = IrLabel(self.next_label);
        self.next_label += 1;
        label
    }

    pub(super) fn emit(&mut self, inst: IrInst) {
        self.body.push(inst);
    }

    pub(super) fn emit_assign(&mut self, ty: IrTypeIdx, rvalue: IrRvalue, span: Span) -> IrLocal {
        let local = self.fresh_local(ty, false, span);
        self.body.push(IrInst::Assign {
            dst: local,
            rvalue,
            span,
        });
        local
    }
}

pub(super) const fn expr_span(expr: &Expr) -> Span {
    match expr {
        Expr::Lit { span, .. }
        | Expr::Name { span, .. }
        | Expr::Paren { span, .. }
        | Expr::Tuple { span, .. }
        | Expr::Block { span, .. }
        | Expr::Let { span, .. }
        | Expr::Fn { span, .. }
        | Expr::Call { span, .. }
        | Expr::Field { span, .. }
        | Expr::Index { span, .. }
        | Expr::Update { span, .. }
        | Expr::Record { span, .. }
        | Expr::Array { span, .. }
        | Expr::Variant { span, .. }
        | Expr::Choice { span, .. }
        | Expr::BinOp { span, .. }
        | Expr::UnaryOp { span, .. }
        | Expr::Piecewise { span, .. }
        | Expr::Match { span, .. }
        | Expr::Return { span, .. }
        | Expr::Quantified { span, .. }
        | Expr::Import { span, .. }
        | Expr::Export { span, .. }
        | Expr::Annotated { span, .. }
        | Expr::Binding { span, .. }
        | Expr::Class { span, .. }
        | Expr::Given { span, .. }
        | Expr::Effect { span, .. }
        | Expr::Foreign { span, .. }
        | Expr::Error { span, .. } => *span,
    }
}

pub(super) fn lower_expr(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
) -> Result<IrLocal, SpannedIrError> {
    let expr = fn_cx.cx.ast.exprs[expr_idx].clone();
    let span = expr_span(&expr);
    match expr {
        Expr::Lit { lit, span } => lower_lit(fn_cx, expr_idx, &lit, span),
        Expr::Name { name, span } => lower_name(fn_cx, expr_idx, name, span),
        Expr::Paren { inner, .. } | Expr::Annotated { inner, .. } => lower_expr(fn_cx, inner),
        Expr::Block { stmts, tail, span } => lower_block(fn_cx, &stmts, tail, span),
        Expr::Let { fields, body, span } => lower_let(fn_cx, &fields, body, span),
        Expr::Binding { fields, span, .. } => lower_binding_expr(fn_cx, &fields, span),
        Expr::BinOp {
            op,
            left,
            right,
            span,
        } => lower_binop(fn_cx, expr_idx, op, left, right, span),
        Expr::UnaryOp { op, operand, span } => lower_unaryop(fn_cx, op, operand, span),
        Expr::Call { callee, args, span } => lower_call(fn_cx, expr_idx, callee, &args, span),
        Expr::Piecewise { arms, span } => lower_piecewise(fn_cx, expr_idx, &arms, span),
        Expr::Match {
            scrutinee,
            arms,
            span,
        } => super::pat::lower_match(fn_cx, expr_idx, scrutinee, &arms, span),
        Expr::Return { value, span } => lower_return(fn_cx, value, span),
        Expr::Tuple { elems, span } => lower_tuple(fn_cx, expr_idx, &elems, span),
        Expr::Field {
            object,
            field,
            span,
            ..
        } => lower_field(fn_cx, expr_idx, object, &field, span),
        Expr::Index {
            object,
            index,
            span,
        } => lower_index(fn_cx, expr_idx, object, index, span),
        Expr::Record { fields, span, .. } => lower_record(fn_cx, expr_idx, &fields, span),
        Expr::Array { elems, span } => lower_array(fn_cx, expr_idx, &elems, span),
        Expr::Variant { name, args, span } => lower_variant(fn_cx, expr_idx, name, &args, span),
        Expr::Choice { .. } => Err(IrError::UnsupportedExpr {
            kind: "choice expression",
        }
        .at(span)),
        Expr::Fn {
            params,
            body: fn_body,
            span,
            ..
        } => lower_fn_literal(fn_cx, expr_idx, &params, fn_body, span),
        Expr::Quantified { .. } => Err(IrError::UnsupportedExpr {
            kind: "generic expression",
        }
        .at(span)),
        Expr::Import { .. } => Err(IrError::UnsupportedExpr {
            kind: "import expression",
        }
        .at(span)),
        Expr::Export { .. } => Err(IrError::UnsupportedExpr {
            kind: "export expression",
        }
        .at(span)),
        Expr::Class { .. } => Err(IrError::UnsupportedExpr {
            kind: "class declaration",
        }
        .at(span)),
        Expr::Given { .. } => Err(IrError::UnsupportedExpr {
            kind: "given declaration",
        }
        .at(span)),
        Expr::Effect { .. } => Err(IrError::UnsupportedExpr {
            kind: "effect declaration",
        }
        .at(span)),
        Expr::Foreign { .. } => Err(IrError::UnsupportedExpr {
            kind: "foreign declaration",
        }
        .at(span)),
        Expr::Update { .. } => Err(IrError::UnsupportedExpr {
            kind: "record update",
        }
        .at(span)),
        Expr::Error { .. } => Err(IrError::UnsupportedExpr {
            kind: "error recovery node",
        }
        .at(span)),
    }
}

fn lower_lit(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    lit: &Lit,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_ty = lower_ty(ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;
    let const_val = match lit {
        Lit::Int { value, .. } => IrConstValue::Int(*value),
        Lit::Float { value, .. } => IrConstValue::Float(*value),
        Lit::Rune { codepoint, .. } => IrConstValue::Rune(*codepoint),
        Lit::Unit { .. } => IrConstValue::Unit,
        Lit::Str { value, .. } => IrConstValue::Str(*value),
        Lit::FStr { parts, .. } => {
            return super::desugar::lower_fstr(fn_cx, parts, span);
        }
    };
    Ok(fn_cx.emit_assign(ir_ty, IrRvalue::Const(const_val), span))
}

fn lower_name(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    name: Symbol,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    // Check for boolean literals spelled as names.
    let name_str = fn_cx.cx.interner.resolve(name);
    if name_str == "true" {
        let ir_ty = fn_cx.cx.ir.types.alloc(IrType::Bool);
        return Ok(fn_cx.emit_assign(ir_ty, IrRvalue::Const(IrConstValue::Bool(true)), span));
    }
    if name_str == "false" {
        let ir_ty = fn_cx.cx.ir.types.alloc(IrType::Bool);
        return Ok(fn_cx.emit_assign(ir_ty, IrRvalue::Const(IrConstValue::Bool(false)), span));
    }

    let Some(&def_id) = fn_cx.cx.expr_defs.get(&expr_idx) else {
        return Err(IrError::UnresolvedName.at(span));
    };

    // Check function map first (function references).
    if let Some(&ir_fn_idx) = fn_cx.cx.fn_map.get(&def_id) {
        let ty_sema = fn_cx
            .cx
            .module_expr_types
            .get(&expr_idx)
            .copied()
            .ok_or_else(|| IrError::MissingExprType.at(span))?;
        let ir_ty =
            lower_ty(ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;
        let fn_ref = IrConstValue::FnRef(ir_fn_idx.raw());
        return Ok(fn_cx.emit_assign(ir_ty, IrRvalue::Const(fn_ref), span));
    }

    // Local variable reference.
    if let Some(&local) = fn_cx.local_map.get(&def_id) {
        return Ok(local);
    }

    Err(IrError::UnresolvedName.at(span))
}

fn lower_block(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    stmts: &[ExprIdx],
    tail: Option<ExprIdx>,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    for &stmt in stmts {
        let _local = lower_expr(fn_cx, stmt)?;
    }
    if let Some(tail_expr) = tail {
        lower_expr(fn_cx, tail_expr)
    } else {
        let unit_ty = fn_cx.cx.ir.types.alloc(IrType::Unit);
        Ok(fn_cx.emit_assign(unit_ty, IrRvalue::Const(IrConstValue::Unit), span))
    }
}

fn lower_let(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    fields: &LetFields,
    body: Option<ExprIdx>,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    if let Some(val) = fields.value {
        let value_local = lower_expr(fn_cx, val)?;
        bind_let_pat(fn_cx, fields.pat, value_local, span)?;
    }
    if let Some(body_expr) = body {
        lower_expr(fn_cx, body_expr)
    } else {
        let unit_ty = fn_cx.cx.ir.types.alloc(IrType::Unit);
        Ok(fn_cx.emit_assign(unit_ty, IrRvalue::Const(IrConstValue::Unit), span))
    }
}

fn lower_binding_expr(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    fields: &LetFields,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    if let Some(val) = fields.value {
        let value_local = lower_expr(fn_cx, val)?;
        bind_let_pat(fn_cx, fields.pat, value_local, span)?;
    }
    let unit_ty = fn_cx.cx.ir.types.alloc(IrType::Unit);
    Ok(fn_cx.emit_assign(unit_ty, IrRvalue::Const(IrConstValue::Unit), span))
}

fn lower_binop(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    op: BinOp,
    left: ExprIdx,
    right: ExprIdx,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    match op {
        BinOp::And => return super::desugar::lower_and(fn_cx, left, right, span),
        BinOp::Or => return super::desugar::lower_or(fn_cx, left, right, span),
        BinOp::Assign => return super::desugar::lower_assign(fn_cx, left, right, span),
        BinOp::Pipe => return super::desugar::lower_pipe(fn_cx, expr_idx, left, right, span),
        BinOp::In => return Err(IrError::UnsupportedOp { op: "in" }.at(span)),
        BinOp::Cons => return Err(IrError::UnsupportedOp { op: "::" }.at(span)),
        BinOp::NilCoal => return Err(IrError::UnsupportedOp { op: "??" }.at(span)),
        BinOp::RangeInc => return Err(IrError::UnsupportedOp { op: "..=" }.at(span)),
        BinOp::RangeExc => return Err(IrError::UnsupportedOp { op: ".." }.at(span)),
        _ => {}
    }

    let left_local = lower_expr(fn_cx, left)?;
    let right_local = lower_expr(fn_cx, right)?;

    let left_ty = fn_cx
        .cx
        .module_expr_types
        .get(&left)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let family = classify_type_family(left_ty, fn_cx.cx.sema)
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_op = map_binop(op, family).map_err(|e| e.at(span))?;

    let result_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_result_ty =
        lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    Ok(fn_cx.emit_assign(
        ir_result_ty,
        IrRvalue::BinOp {
            op: ir_op,
            left: IrOperand::Local(left_local),
            right: IrOperand::Local(right_local),
        },
        span,
    ))
}

fn lower_unaryop(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    op: UnaryOp,
    operand: ExprIdx,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    match op {
        UnaryOp::Neg => {
            let operand_local = lower_expr(fn_cx, operand)?;
            let op_ty = fn_cx
                .cx
                .module_expr_types
                .get(&operand)
                .copied()
                .ok_or_else(|| IrError::MissingExprType.at(span))?;
            let family = classify_type_family(op_ty, fn_cx.cx.sema)
                .ok_or_else(|| IrError::MissingExprType.at(span))?;
            let ir_op = match family {
                TypeFamily::Signed => IrUnaryOp::INeg,
                TypeFamily::Float => IrUnaryOp::FNeg,
                TypeFamily::Unsigned | TypeFamily::Bool => {
                    return Err(IrError::UnsupportedOp {
                        op: "negation on non-numeric type",
                    }
                    .at(span));
                }
            };
            let ir_ty =
                lower_ty(op_ty, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;
            Ok(fn_cx.emit_assign(
                ir_ty,
                IrRvalue::UnaryOp {
                    op: ir_op,
                    operand: IrOperand::Local(operand_local),
                },
                span,
            ))
        }
        UnaryOp::Not => {
            let operand_local = lower_expr(fn_cx, operand)?;
            let op_ty = fn_cx
                .cx
                .module_expr_types
                .get(&operand)
                .copied()
                .ok_or_else(|| IrError::MissingExprType.at(span))?;
            let ir_ty =
                lower_ty(op_ty, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;
            Ok(fn_cx.emit_assign(
                ir_ty,
                IrRvalue::UnaryOp {
                    op: IrUnaryOp::Not,
                    operand: IrOperand::Local(operand_local),
                },
                span,
            ))
        }
        UnaryOp::Defer => Err(IrError::UnsupportedOp { op: "defer" }.at(span)),
        UnaryOp::Spawn => Err(IrError::UnsupportedOp { op: "spawn" }.at(span)),
        UnaryOp::Await => Err(IrError::UnsupportedOp { op: "await" }.at(span)),
        UnaryOp::Try => Err(IrError::UnsupportedOp { op: "try" }.at(span)),
    }
}

fn lower_call(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    callee: ExprIdx,
    args: &[Arg],
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    // Only support direct calls to named functions.
    if !matches!(fn_cx.cx.ast.exprs[callee], Expr::Name { .. }) {
        return Err(IrError::UnsupportedExpr {
            kind: "indirect function call",
        }
        .at(span));
    }

    let Some(&callee_def_id) = fn_cx.cx.expr_defs.get(&callee) else {
        return Err(IrError::UnresolvedName.at(span));
    };

    // Check if this is a foreign function call.
    if let Some(&ffi_idx) = fn_cx.cx.foreign_fn_map.get(&callee_def_id) {
        return lower_foreign_call(fn_cx, expr_idx, ffi_idx, args, span);
    }

    let Some(&ir_fn_idx) = fn_cx.cx.fn_map.get(&callee_def_id) else {
        return Err(IrError::UnresolvedName.at(span));
    };

    let mut ir_args = Vec::with_capacity(args.len());
    for &arg in args {
        match arg {
            Arg::Pos { expr: arg_expr, .. } => {
                let arg_local = lower_expr(fn_cx, arg_expr)?;
                ir_args.push(IrOperand::Local(arg_local));
            }
            Arg::Hole { span: hole_span } => {
                return Err(IrError::UnsupportedExpr {
                    kind: "partial application",
                }
                .at(hole_span));
            }
        }
    }

    let ret_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_ret_ty =
        lower_ty(ret_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    Ok(fn_cx.emit_assign(
        ir_ret_ty,
        IrRvalue::Call {
            callee: IrCallee::Direct(ir_fn_idx),
            args: ir_args,
            tail: false,
        },
        span,
    ))
}

fn lower_foreign_call(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    ffi_idx: u32,
    args: &[Arg],
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let mut ir_args = Vec::with_capacity(args.len());
    for &arg in args {
        match arg {
            Arg::Pos { expr: arg_expr, .. } => {
                let arg_local = lower_expr(fn_cx, arg_expr)?;
                ir_args.push(IrOperand::Local(arg_local));
            }
            Arg::Hole { span: hole_span } => {
                return Err(IrError::UnsupportedExpr {
                    kind: "partial application",
                }
                .at(hole_span));
            }
        }
    }

    let ret_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_ret_ty =
        lower_ty(ret_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    Ok(fn_cx.emit_assign(
        ir_ret_ty,
        IrRvalue::ForeignCall {
            fn_idx: ffi_idx,
            args: ir_args,
        },
        span,
    ))
}

fn lower_piecewise(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    arms: &[PwArm],
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    if arms.is_empty() {
        return Err(IrError::UnsupportedExpr {
            kind: "empty piecewise expression",
        }
        .at(span));
    }

    let result_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_result_ty =
        lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;
    let result_local = fn_cx.fresh_local(ir_result_ty, true, span);
    let merge_lbl = fn_cx.fresh_label();

    let n = arms.len();
    for (i, arm) in arms.iter().enumerate() {
        let is_last = i == n - 1;
        if is_last || matches!(arm.guard, PwGuard::Any { .. }) {
            emit_piecewise_arm(fn_cx, arm.result, result_local, arm.span, merge_lbl)?;
        } else if let PwGuard::When {
            expr: guard_expr,
            span: guard_span,
        } = arm.guard
        {
            emit_piecewise_conditional_arm(
                fn_cx,
                guard_expr,
                guard_span,
                arm.result,
                result_local,
                arm.span,
                merge_lbl,
            )?;
        } else {
            emit_piecewise_arm(fn_cx, arm.result, result_local, arm.span, merge_lbl)?;
        }
    }

    fn_cx.emit(IrInst::Label(merge_lbl));
    Ok(result_local)
}

fn emit_piecewise_arm(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    result_expr: ExprIdx,
    result_local: IrLocal,
    arm_span: Span,
    merge_lbl: IrLabel,
) -> Result<(), SpannedIrError> {
    let arm_local = lower_expr(fn_cx, result_expr)?;
    fn_cx.emit(IrInst::Store {
        dst: IrPlace::Local(result_local),
        value: IrOperand::Local(arm_local),
        span: arm_span,
    });
    fn_cx.emit(IrInst::Goto(merge_lbl));
    Ok(())
}

fn emit_piecewise_conditional_arm(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    guard_expr: ExprIdx,
    guard_span: Span,
    result_expr: ExprIdx,
    result_local: IrLocal,
    arm_span: Span,
    merge_lbl: IrLabel,
) -> Result<(), SpannedIrError> {
    let cond_local = lower_expr(fn_cx, guard_expr)?;
    let then_lbl = fn_cx.fresh_label();
    let next_lbl = fn_cx.fresh_label();
    fn_cx.emit(IrInst::Branch {
        cond: IrOperand::Local(cond_local),
        then_label: then_lbl,
        else_label: next_lbl,
        span: guard_span,
    });
    fn_cx.emit(IrInst::Label(then_lbl));
    let arm_local = lower_expr(fn_cx, result_expr)?;
    fn_cx.emit(IrInst::Store {
        dst: IrPlace::Local(result_local),
        value: IrOperand::Local(arm_local),
        span: arm_span,
    });
    fn_cx.emit(IrInst::Goto(merge_lbl));
    fn_cx.emit(IrInst::Label(next_lbl));
    Ok(())
}

fn lower_return(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    value: Option<ExprIdx>,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let ret_local = if let Some(val_expr) = value {
        lower_expr(fn_cx, val_expr)?
    } else {
        let unit_ty = fn_cx.cx.ir.types.alloc(IrType::Unit);
        fn_cx.emit_assign(unit_ty, IrRvalue::Const(IrConstValue::Unit), span)
    };
    fn_cx.emit(IrInst::Return {
        value: Some(IrOperand::Local(ret_local)),
        span,
    });
    // Return an unreachable unit local after the explicit return.
    let unit_ty = fn_cx.cx.ir.types.alloc(IrType::Unit);
    Ok(fn_cx.fresh_local(unit_ty, false, span))
}

fn lower_tuple(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    elems: &[ExprIdx],
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let result_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_result_ty =
        lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;
    let mut fields = Vec::with_capacity(elems.len());
    for &elem in elems {
        let elem_local = lower_expr(fn_cx, elem)?;
        fields.push(IrOperand::Local(elem_local));
    }
    Ok(fn_cx.emit_assign(
        ir_result_ty,
        IrRvalue::MakeProduct {
            ty: ir_result_ty,
            fields,
        },
        span,
    ))
}

fn bind_let_pat(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    pat_idx: PatIdx,
    local: IrLocal,
    _enclosing_span: Span,
) -> Result<(), SpannedIrError> {
    match fn_cx.cx.ast.pats[pat_idx].clone() {
        Pat::Bind {
            span, inner: None, ..
        } => {
            let Some(&def_id) = fn_cx.cx.pat_defs.get(&span) else {
                return Err(IrError::UnresolvedName.at(span));
            };
            let _ = fn_cx.local_map.insert(def_id, local);
            Ok(())
        }
        Pat::Wild { .. } => Ok(()),
        Pat::Tuple { elems, span } => {
            for (i, &elem_pat) in elems.iter().enumerate() {
                let idx = u32::try_from(i).map_err(|_| IrError::IndexOverflow.at(span))?;
                let elem_ty = super::pat::infer_pat_ir_type(fn_cx, elem_pat)?;
                let elem_local = fn_cx.emit_assign(
                    elem_ty,
                    IrRvalue::FieldGet {
                        object: IrOperand::Local(local),
                        index: idx,
                    },
                    span,
                );
                bind_let_pat(fn_cx, elem_pat, elem_local, span)?;
            }
            Ok(())
        }
        Pat::Variant { span, .. } => Err(IrError::UnsupportedPattern { kind: "variant" }.at(span)),
        Pat::Record { span, .. } => Err(IrError::UnsupportedPattern { kind: "record" }.at(span)),
        Pat::Array { span, .. } => Err(IrError::UnsupportedPattern { kind: "array" }.at(span)),
        Pat::Or { span, .. } => Err(IrError::UnsupportedPattern { kind: "or" }.at(span)),
        Pat::Lit { span, .. } => Err(IrError::UnsupportedPattern {
            kind: "literal in let binding",
        }
        .at(span)),
        Pat::Bind { span, .. } => Err(IrError::UnsupportedPattern {
            kind: "nested bind",
        }
        .at(span)),
        Pat::Error { span, .. } => Err(IrError::UnsupportedPattern {
            kind: "error recovery",
        }
        .at(span)),
    }
}

enum TypeFamily {
    Signed,
    Unsigned,
    Float,
    Bool,
}

fn classify_type_family(ty_idx: TypeIdx, sema: &SemaResult) -> Option<TypeFamily> {
    let ty = sema.unify.resolve(ty_idx, &sema.types);
    let Type::Named { def, .. } = &sema.types[ty] else {
        return None;
    };
    let wk = &sema.well_known;
    if *def == wk.ints.int
        || *def == wk.ints.int8
        || *def == wk.ints.int16
        || *def == wk.ints.int32
        || *def == wk.ints.int64
    {
        return Some(TypeFamily::Signed);
    }
    if *def == wk.uints.uint8
        || *def == wk.uints.uint16
        || *def == wk.uints.uint32
        || *def == wk.uints.uint64
    {
        return Some(TypeFamily::Unsigned);
    }
    if *def == wk.floats.float32 || *def == wk.floats.float64 {
        return Some(TypeFamily::Float);
    }
    if *def == wk.bool {
        return Some(TypeFamily::Bool);
    }
    None
}

const fn map_binop(op: BinOp, family: TypeFamily) -> Result<IrBinOp, IrError> {
    match (op, family) {
        (BinOp::Add, TypeFamily::Signed) => Ok(IrBinOp::IAdd),
        (BinOp::Sub, TypeFamily::Signed) => Ok(IrBinOp::ISub),
        (BinOp::Mul, TypeFamily::Signed) => Ok(IrBinOp::IMul),
        (BinOp::Div, TypeFamily::Signed) => Ok(IrBinOp::IDiv),
        (BinOp::Rem, TypeFamily::Signed) => Ok(IrBinOp::IRem),
        (BinOp::Eq, TypeFamily::Signed | TypeFamily::Unsigned | TypeFamily::Bool) => {
            Ok(IrBinOp::IEq)
        }
        (BinOp::Ne, TypeFamily::Signed | TypeFamily::Unsigned | TypeFamily::Bool) => {
            Ok(IrBinOp::INe)
        }
        (BinOp::Lt, TypeFamily::Signed) => Ok(IrBinOp::ILt),
        (BinOp::Le, TypeFamily::Signed) => Ok(IrBinOp::ILe),
        (BinOp::Gt, TypeFamily::Signed) => Ok(IrBinOp::IGt),
        (BinOp::Ge, TypeFamily::Signed) => Ok(IrBinOp::IGe),
        (BinOp::Add, TypeFamily::Unsigned) => Ok(IrBinOp::UAdd),
        (BinOp::Sub, TypeFamily::Unsigned) => Ok(IrBinOp::USub),
        (BinOp::Mul, TypeFamily::Unsigned) => Ok(IrBinOp::UMul),
        (BinOp::Div, TypeFamily::Unsigned) => Ok(IrBinOp::UDiv),
        (BinOp::Rem, TypeFamily::Unsigned) => Ok(IrBinOp::URem),
        (BinOp::Lt, TypeFamily::Unsigned) => Ok(IrBinOp::ULt),
        (BinOp::Le, TypeFamily::Unsigned) => Ok(IrBinOp::ULe),
        (BinOp::Gt, TypeFamily::Unsigned) => Ok(IrBinOp::UGt),
        (BinOp::Ge, TypeFamily::Unsigned) => Ok(IrBinOp::UGe),
        (BinOp::Add, TypeFamily::Float) => Ok(IrBinOp::FAdd),
        (BinOp::Sub, TypeFamily::Float) => Ok(IrBinOp::FSub),
        (BinOp::Mul, TypeFamily::Float) => Ok(IrBinOp::FMul),
        (BinOp::Div, TypeFamily::Float) => Ok(IrBinOp::FDiv),
        (BinOp::Rem, TypeFamily::Float) => Ok(IrBinOp::FRem),
        (BinOp::Eq, TypeFamily::Float) => Ok(IrBinOp::FEq),
        (BinOp::Ne, TypeFamily::Float) => Ok(IrBinOp::FNe),
        (BinOp::Lt, TypeFamily::Float) => Ok(IrBinOp::FLt),
        (BinOp::Le, TypeFamily::Float) => Ok(IrBinOp::FLe),
        (BinOp::Gt, TypeFamily::Float) => Ok(IrBinOp::FGt),
        (BinOp::Ge, TypeFamily::Float) => Ok(IrBinOp::FGe),
        (BinOp::Xor, TypeFamily::Signed | TypeFamily::Unsigned | TypeFamily::Bool) => {
            Ok(IrBinOp::Xor)
        }
        (BinOp::Shl, TypeFamily::Signed | TypeFamily::Unsigned) => Ok(IrBinOp::Shl),
        (BinOp::Shr, TypeFamily::Signed | TypeFamily::Unsigned) => Ok(IrBinOp::Shr),
        (BinOp::ShrUn, TypeFamily::Signed | TypeFamily::Unsigned) => Ok(IrBinOp::ShrUn),
        _ => Err(IrError::UnsupportedOp {
            op: "unknown binary operator for type",
        }),
    }
}

fn lower_field(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    object: ExprIdx,
    field: &FieldKey,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let obj_local = lower_expr(fn_cx, object)?;
    let result_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_result_ty =
        lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    let index = match field {
        FieldKey::Pos { index, .. } => *index,
        FieldKey::Name { name, .. } => resolve_field_index(fn_cx, object, *name, span)?,
    };

    Ok(fn_cx.emit_assign(
        ir_result_ty,
        IrRvalue::FieldGet {
            object: IrOperand::Local(obj_local),
            index,
        },
        span,
    ))
}

fn resolve_field_index(
    fn_cx: &FnLowerCtx<'_, '_>,
    object: ExprIdx,
    name: Symbol,
    span: Span,
) -> Result<u32, SpannedIrError> {
    let obj_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&object)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let resolved = fn_cx
        .cx
        .sema
        .unify
        .resolve(obj_ty_sema, &fn_cx.cx.sema.types);
    let obj_ty = fn_cx.cx.sema.types[resolved].clone();
    if let Type::Record { fields, .. } = obj_ty {
        for (i, f) in fields.iter().enumerate() {
            if f.name == name {
                return u32::try_from(i).map_err(|_| IrError::IndexOverflow.at(span));
            }
        }
    }
    Err(IrError::UnresolvedName.at(span))
}

fn lower_index(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    object: ExprIdx,
    index: ExprIdx,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let obj_local = lower_expr(fn_cx, object)?;
    let idx_local = lower_expr(fn_cx, index)?;
    let result_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_result_ty =
        lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    Ok(fn_cx.emit_assign(
        ir_result_ty,
        IrRvalue::IndexGet {
            array: IrOperand::Local(obj_local),
            index: IrOperand::Local(idx_local),
        },
        span,
    ))
}

fn lower_record(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    fields: &[RecField],
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let result_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_result_ty =
        lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    let mut ir_fields = Vec::with_capacity(fields.len());
    for field in fields {
        match field {
            RecField::Named {
                name,
                value: Some(v),
                ..
            } => {
                let _ = name;
                let field_local = lower_expr(fn_cx, *v)?;
                ir_fields.push(IrOperand::Local(field_local));
            }
            RecField::Named {
                name,
                value: None,
                span: field_span,
            } => {
                // Punning: look up the name as a variable
                let pun_expr_idx = find_pun_expr(fn_cx, *name, *field_span)?;
                let field_local = lower_name(fn_cx, pun_expr_idx, *name, *field_span)?;
                ir_fields.push(IrOperand::Local(field_local));
            }
            RecField::Spread { span: s, .. } => {
                return Err(IrError::UnsupportedExpr {
                    kind: "record spread",
                }
                .at(*s));
            }
        }
    }

    Ok(fn_cx.emit_assign(
        ir_result_ty,
        IrRvalue::MakeProduct {
            ty: ir_result_ty,
            fields: ir_fields,
        },
        span,
    ))
}

fn find_pun_expr(
    fn_cx: &FnLowerCtx<'_, '_>,
    name: Symbol,
    span: Span,
) -> Result<ExprIdx, SpannedIrError> {
    // For record punning, we need to find the ExprIdx for this name.
    // Search expr_defs for any entry whose def has matching name.
    for (&eidx, &did) in fn_cx.cx.expr_defs {
        let def_idx = usize::try_from(did.0).map_err(|_| IrError::IndexOverflow.at(span))?;
        if fn_cx.cx.sema.defs[def_idx].name == name {
            return Ok(eidx);
        }
    }
    Err(IrError::UnresolvedName.at(span))
}

fn lower_array(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    elems: &[ArrayElem],
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let result_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_result_ty =
        lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    let elem_ty = match &fn_cx.cx.ir.types[ir_result_ty] {
        IrType::Array { elem } => *elem,
        _ => fn_cx.cx.ir.types.alloc(IrType::Any),
    };

    let mut ir_elems = Vec::with_capacity(elems.len());
    for elem in elems {
        match elem {
            ArrayElem::Elem { expr, .. } => {
                let elem_local = lower_expr(fn_cx, *expr)?;
                ir_elems.push(IrOperand::Local(elem_local));
            }
            ArrayElem::Spread { span: s, .. } => {
                return Err(IrError::UnsupportedExpr {
                    kind: "array spread",
                }
                .at(*s));
            }
        }
    }

    Ok(fn_cx.emit_assign(
        ir_result_ty,
        IrRvalue::MakeArray {
            elem_ty,
            elems: ir_elems,
        },
        span,
    ))
}

fn lower_variant(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    name: Symbol,
    args: &[ExprIdx],
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let result_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_result_ty =
        lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    let tag = resolve_variant_tag_from_ir(fn_cx, ir_result_ty, name, span)?;

    let mut payload = Vec::with_capacity(args.len());
    for &arg_expr in args {
        let arg_local = lower_expr(fn_cx, arg_expr)?;
        payload.push(IrOperand::Local(arg_local));
    }

    Ok(fn_cx.emit_assign(
        ir_result_ty,
        IrRvalue::MakeVariant {
            ty: ir_result_ty,
            tag,
            payload,
        },
        span,
    ))
}

fn resolve_variant_tag_from_ir(
    fn_cx: &FnLowerCtx<'_, '_>,
    ir_ty: IrTypeIdx,
    name: Symbol,
    span: Span,
) -> Result<u32, SpannedIrError> {
    let ty = fn_cx.cx.ir.types[ir_ty].clone();
    if let IrType::Sum { variants } = ty {
        let target = fn_cx.cx.interner.resolve(name);
        for (i, v) in variants.iter().enumerate() {
            if fn_cx.cx.interner.resolve(v.name) == target {
                return u32::try_from(i).map_err(|_| IrError::IndexOverflow.at(span));
            }
        }
    }
    Err(IrError::UnresolvedName.at(span))
}

fn lower_fn_literal(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: ExprIdx,
    params: &[Param],
    body_expr: ExprIdx,
    span: Span,
) -> Result<IrLocal, SpannedIrError> {
    let fn_ty_sema = fn_cx
        .cx
        .module_expr_types
        .get(&expr_idx)
        .copied()
        .ok_or_else(|| IrError::MissingExprType.at(span))?;
    let ir_fn_ty =
        lower_ty(fn_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;

    let resolved = fn_cx
        .cx
        .sema
        .unify
        .resolve(fn_ty_sema, &fn_cx.cx.sema.types);
    let sema_fn_ty = fn_cx.cx.sema.types[resolved].clone();
    let Type::Fn {
        params: param_sema_tys,
        ret: ret_sema_ty,
        effects: effect_row,
    } = sema_fn_ty
    else {
        return Err(IrError::UnsupportedType.at(span));
    };

    let ir_ret_ty =
        lower_ty(ret_sema_ty, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;
    let ir_effects =
        super::effect::lower_effect_row(&effect_row, &fn_cx.cx.sema.well_known.effects);

    let mut ir_params = Vec::with_capacity(params.len());
    let mut ir_locals = Vec::with_capacity(params.len());
    for (i, (param, &sema_ty)) in params.iter().zip(param_sema_tys.iter()).enumerate() {
        let local = IrLocal(u32::try_from(i).map_err(|_| IrError::IndexOverflow.at(span))?);
        let ir_ty =
            lower_ty(sema_ty, fn_cx.cx.sema, &mut fn_cx.cx.ir.types).map_err(|e| e.at(span))?;
        ir_params.push(IrParam {
            local,
            ty: ir_ty,
            mode: IrParamMode::Value,
            span: param.span,
        });
        ir_locals.push(IrLocalDecl {
            local,
            ty: ir_ty,
            mutable: false,
            span: param.span,
        });
    }

    let fn_id = IrFnId(fn_cx.cx.next_fn_id);
    fn_cx.cx.next_fn_id += 1;

    let lambda_name = fn_cx.cx.interner.intern("<lambda>");
    let stub = IrFunction {
        id: fn_id,
        source_def: None,
        name: lambda_name,
        params: ir_params,
        ret_ty: ir_ret_ty,
        effects: ir_effects,
        body: vec![],
        locals: ir_locals.clone(),
        is_closure: false,
        span,
    };

    let fn_idx = fn_cx.cx.ir.functions.alloc(stub);

    let n_params = u32::try_from(ir_locals.len()).map_err(|_| IrError::IndexOverflow.at(span))?;

    let local_map = super::stmt::build_param_local_map(params, fn_cx.cx.pat_defs);

    let mut inner_fn_cx = FnLowerCtx {
        cx: fn_cx.cx,
        local_map,
        locals: ir_locals,
        body: vec![],
        next_local: n_params,
        next_label: 0,
    };

    let result_local = lower_expr(&mut inner_fn_cx, body_expr)?;
    inner_fn_cx.emit(IrInst::Return {
        value: Some(IrOperand::Local(result_local)),
        span,
    });

    let new_body = inner_fn_cx.body;
    let new_locals = inner_fn_cx.locals;
    fn_cx.cx.ir.functions[fn_idx].body = new_body;
    fn_cx.cx.ir.functions[fn_idx].locals = new_locals;

    let fn_ref = IrConstValue::FnRef(fn_idx.raw());
    Ok(fn_cx.emit_assign(ir_fn_ty, IrRvalue::Const(fn_ref), span))
}
