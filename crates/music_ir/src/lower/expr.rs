//! Expression lowering: AST `Expr` nodes -> IR instructions in ANF.

use std::collections::HashMap;

use music_ast::expr::{Arg, BinOp, Expr, LetFields, PwArm, PwGuard, UnaryOp};
use music_ast::lit::Lit;
use music_ast::pat::Pat;
use music_sema::DefId;
use music_sema::types::Type;
use music_shared::{Idx, Span, Symbol};

use crate::constant::IrConstValue;
use crate::error::IrError;
use crate::func::{IrLocal, IrLocalDecl};
use crate::inst::{IrBinOp, IrCallee, IrInst, IrLabel, IrOperand, IrPlace, IrRvalue, IrUnaryOp};
use crate::types::IrType;

use super::decl::LowerCtx;
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
    pub(super) fn fresh_local(&mut self, ty: Idx<IrType>, mutable: bool, span: Span) -> IrLocal {
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

    pub(super) fn emit_assign(&mut self, ty: Idx<IrType>, rvalue: IrRvalue, span: Span) -> IrLocal {
        let local = self.fresh_local(ty, false, span);
        self.body.push(IrInst::Assign {
            dst: local,
            rvalue,
            span,
        });
        local
    }
}

pub(super) fn lower_expr(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: Idx<Expr>,
) -> Result<IrLocal, IrError> {
    let expr = fn_cx.cx.ast.exprs[expr_idx].clone();
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
        Expr::Return { value, span } => lower_return(fn_cx, value, span),
        Expr::Tuple { elems, span } => lower_tuple(fn_cx, expr_idx, &elems, span),
        _ => Err(IrError::UnsupportedExpr),
    }
}

fn lower_lit(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: Idx<Expr>,
    lit: &Lit,
    span: Span,
) -> Result<IrLocal, IrError> {
    let ty_sema = fn_cx
        .cx
        .sema
        .expr_types
        .get(&expr_idx)
        .copied()
        .ok_or(IrError::UnsupportedExpr)?;
    let ir_ty = lower_ty(ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types)?;
    let const_val = match lit {
        Lit::Int { value, .. } => IrConstValue::Int(*value),
        Lit::Float { value, .. } => IrConstValue::Float(*value),
        Lit::Rune { codepoint, .. } => IrConstValue::Rune(*codepoint),
        Lit::Unit { .. } => IrConstValue::Unit,
        Lit::Str { value, .. } => IrConstValue::Str(*value),
        Lit::FStr { .. } => return Err(IrError::UnsupportedExpr),
    };
    Ok(fn_cx.emit_assign(ir_ty, IrRvalue::Const(const_val), span))
}

fn lower_name(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: Idx<Expr>,
    name: Symbol,
    span: Span,
) -> Result<IrLocal, IrError> {
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

    let Some(&def_id) = fn_cx.cx.sema.resolution.expr_defs.get(&expr_idx) else {
        return Err(IrError::UnsupportedExpr);
    };

    // Check function map first (function references).
    if let Some(&ir_fn_idx) = fn_cx.cx.fn_map.get(&def_id) {
        let ty_sema = fn_cx
            .cx
            .sema
            .expr_types
            .get(&expr_idx)
            .copied()
            .ok_or(IrError::UnsupportedExpr)?;
        let ir_ty = lower_ty(ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types)?;
        let fn_ref = IrConstValue::FnRef(ir_fn_idx.raw());
        return Ok(fn_cx.emit_assign(ir_ty, IrRvalue::Const(fn_ref), span));
    }

    // Local variable reference.
    if let Some(&local) = fn_cx.local_map.get(&def_id) {
        return Ok(local);
    }

    Err(IrError::UnsupportedExpr)
}

fn lower_block(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    stmts: &[Idx<Expr>],
    tail: Option<Idx<Expr>>,
    span: Span,
) -> Result<IrLocal, IrError> {
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
    body: Option<Idx<Expr>>,
    span: Span,
) -> Result<IrLocal, IrError> {
    let value_local = lower_expr(fn_cx, fields.value)?;
    bind_pat(fn_cx, fields.pat, value_local)?;
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
) -> Result<IrLocal, IrError> {
    let value_local = lower_expr(fn_cx, fields.value)?;
    bind_pat(fn_cx, fields.pat, value_local)?;
    let unit_ty = fn_cx.cx.ir.types.alloc(IrType::Unit);
    Ok(fn_cx.emit_assign(unit_ty, IrRvalue::Const(IrConstValue::Unit), span))
}

fn lower_binop(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: Idx<Expr>,
    op: BinOp,
    left: Idx<Expr>,
    right: Idx<Expr>,
    span: Span,
) -> Result<IrLocal, IrError> {
    match op {
        BinOp::And
        | BinOp::Or
        | BinOp::Pipe
        | BinOp::Assign
        | BinOp::In
        | BinOp::Cons
        | BinOp::NilCoal
        | BinOp::RangeInc
        | BinOp::RangeExc => return Err(IrError::UnsupportedExpr),
        _ => {}
    }

    let left_local = lower_expr(fn_cx, left)?;
    let right_local = lower_expr(fn_cx, right)?;

    let left_ty = fn_cx
        .cx
        .sema
        .expr_types
        .get(&left)
        .copied()
        .ok_or(IrError::UnsupportedExpr)?;
    let family = type_family(left_ty, fn_cx.cx.sema).ok_or(IrError::UnsupportedExpr)?;
    let ir_op = map_binop(op, family)?;

    let result_ty_sema = fn_cx
        .cx
        .sema
        .expr_types
        .get(&expr_idx)
        .copied()
        .ok_or(IrError::UnsupportedExpr)?;
    let ir_result_ty = lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types)?;

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
    operand: Idx<Expr>,
    span: Span,
) -> Result<IrLocal, IrError> {
    match op {
        UnaryOp::Neg => {
            let operand_local = lower_expr(fn_cx, operand)?;
            let op_ty = fn_cx
                .cx
                .sema
                .expr_types
                .get(&operand)
                .copied()
                .ok_or(IrError::UnsupportedExpr)?;
            let family = type_family(op_ty, fn_cx.cx.sema).ok_or(IrError::UnsupportedExpr)?;
            let ir_op = match family {
                TypeFamily::Signed => IrUnaryOp::INeg,
                TypeFamily::Float => IrUnaryOp::FNeg,
                _ => return Err(IrError::UnsupportedExpr),
            };
            let ir_ty = lower_ty(op_ty, fn_cx.cx.sema, &mut fn_cx.cx.ir.types)?;
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
                .sema
                .expr_types
                .get(&operand)
                .copied()
                .ok_or(IrError::UnsupportedExpr)?;
            let ir_ty = lower_ty(op_ty, fn_cx.cx.sema, &mut fn_cx.cx.ir.types)?;
            Ok(fn_cx.emit_assign(
                ir_ty,
                IrRvalue::UnaryOp {
                    op: IrUnaryOp::Not,
                    operand: IrOperand::Local(operand_local),
                },
                span,
            ))
        }
        _ => Err(IrError::UnsupportedExpr),
    }
}

fn lower_call(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    expr_idx: Idx<Expr>,
    callee: Idx<Expr>,
    args: &[Arg],
    span: Span,
) -> Result<IrLocal, IrError> {
    // Only support direct calls to named functions.
    if !matches!(fn_cx.cx.ast.exprs[callee], Expr::Name { .. }) {
        return Err(IrError::UnsupportedExpr);
    }

    let Some(&callee_def_id) = fn_cx.cx.sema.resolution.expr_defs.get(&callee) else {
        return Err(IrError::UnsupportedExpr);
    };

    // Check if this is a foreign function call.
    if let Some(&ffi_idx) = fn_cx.cx.foreign_fn_map.get(&callee_def_id) {
        return lower_foreign_call(fn_cx, expr_idx, ffi_idx, args, span);
    }

    let Some(&ir_fn_idx) = fn_cx.cx.fn_map.get(&callee_def_id) else {
        return Err(IrError::UnsupportedExpr);
    };

    let mut ir_args = Vec::with_capacity(args.len());
    for &arg in args {
        match arg {
            Arg::Pos { expr: arg_expr, .. } => {
                let arg_local = lower_expr(fn_cx, arg_expr)?;
                ir_args.push(IrOperand::Local(arg_local));
            }
            Arg::Hole { .. } => return Err(IrError::UnsupportedExpr),
        }
    }

    let ret_ty_sema = fn_cx
        .cx
        .sema
        .expr_types
        .get(&expr_idx)
        .copied()
        .ok_or(IrError::UnsupportedExpr)?;
    let ir_ret_ty = lower_ty(ret_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types)?;

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
    expr_idx: Idx<Expr>,
    ffi_idx: u32,
    args: &[Arg],
    span: Span,
) -> Result<IrLocal, IrError> {
    let mut ir_args = Vec::with_capacity(args.len());
    for &arg in args {
        match arg {
            Arg::Pos { expr: arg_expr, .. } => {
                let arg_local = lower_expr(fn_cx, arg_expr)?;
                ir_args.push(IrOperand::Local(arg_local));
            }
            Arg::Hole { .. } => return Err(IrError::UnsupportedExpr),
        }
    }

    let ret_ty_sema = fn_cx
        .cx
        .sema
        .expr_types
        .get(&expr_idx)
        .copied()
        .ok_or(IrError::UnsupportedExpr)?;
    let ir_ret_ty = lower_ty(ret_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types)?;

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
    expr_idx: Idx<Expr>,
    arms: &[PwArm],
    span: Span,
) -> Result<IrLocal, IrError> {
    if arms.is_empty() {
        return Err(IrError::UnsupportedExpr);
    }

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
        if is_last || matches!(arm.guard, PwGuard::Any { .. }) {
            emit_arm_store(fn_cx, arm.result, result_local, arm.span, merge_lbl)?;
        } else if let PwGuard::When {
            expr: guard_expr,
            span: guard_span,
        } = arm.guard
        {
            emit_conditional_arm(
                fn_cx,
                guard_expr,
                guard_span,
                arm.result,
                result_local,
                arm.span,
                merge_lbl,
            )?;
        } else {
            emit_arm_store(fn_cx, arm.result, result_local, arm.span, merge_lbl)?;
        }
    }

    fn_cx.emit(IrInst::Label(merge_lbl));
    Ok(result_local)
}

fn emit_arm_store(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    result_expr: Idx<Expr>,
    result_local: IrLocal,
    arm_span: Span,
    merge_lbl: IrLabel,
) -> Result<(), IrError> {
    let arm_local = lower_expr(fn_cx, result_expr)?;
    fn_cx.emit(IrInst::Store {
        dst: IrPlace::Local(result_local),
        value: IrOperand::Local(arm_local),
        span: arm_span,
    });
    fn_cx.emit(IrInst::Goto(merge_lbl));
    Ok(())
}

fn emit_conditional_arm(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    guard_expr: Idx<Expr>,
    guard_span: Span,
    result_expr: Idx<Expr>,
    result_local: IrLocal,
    arm_span: Span,
    merge_lbl: IrLabel,
) -> Result<(), IrError> {
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
    value: Option<Idx<Expr>>,
    span: Span,
) -> Result<IrLocal, IrError> {
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
    expr_idx: Idx<Expr>,
    elems: &[Idx<Expr>],
    span: Span,
) -> Result<IrLocal, IrError> {
    let result_ty_sema = fn_cx
        .cx
        .sema
        .expr_types
        .get(&expr_idx)
        .copied()
        .ok_or(IrError::UnsupportedExpr)?;
    let ir_result_ty = lower_ty(result_ty_sema, fn_cx.cx.sema, &mut fn_cx.cx.ir.types)?;
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

fn bind_pat(
    fn_cx: &mut FnLowerCtx<'_, '_>,
    pat_idx: Idx<music_ast::Pat>,
    local: IrLocal,
) -> Result<(), IrError> {
    match fn_cx.cx.ast.pats[pat_idx].clone() {
        Pat::Bind {
            span, inner: None, ..
        } => {
            let Some(&def_id) = fn_cx.cx.sema.resolution.pat_defs.get(&span) else {
                return Err(IrError::UnsupportedExpr);
            };
            let _ = fn_cx.local_map.insert(def_id, local);
            Ok(())
        }
        Pat::Wild { .. } => Ok(()),
        _ => Err(IrError::UnsupportedExpr),
    }
}

enum TypeFamily {
    Signed,
    Unsigned,
    Float,
    Bool,
}

fn type_family(ty_idx: Idx<Type>, sema: &music_sema::SemaResult) -> Option<TypeFamily> {
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
        _ => Err(IrError::UnsupportedExpr),
    }
}
