//! Rvalue emission: translates `IrRvalue` into bytecode sequences.

use std::cmp::Ordering;

use music_ir::{
    IrBinOp, IrCallee, IrConstValue, IrOperand, IrRvalue, IrType, IrTypeIdx, IrUnaryOp,
};
use music_shared::{Arena, Interner};

use crate::const_pool::ConstPool;
use crate::error::EmitError;
use crate::type_pool::TypePool;
use musi_bytecode::{Opcode, encode_no_operand, encode_u8, encode_u32};

use super::fn_emitter::FnEmitter;

/// Emit bytecode for `operand`, leaving its value on the stack.
pub fn emit_operand(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    operand: &IrOperand,
    interner: &Interner,
) -> Result<(), EmitError> {
    match operand {
        IrOperand::Local(local) => {
            fe.emit_ld_loc(*local);
        }
        IrOperand::Const(value) => {
            emit_const_value(fe, cp, value, interner)?;
        }
    }
    Ok(())
}

/// Emit bytecode for a constant value, leaving it on the stack.
pub fn emit_const_value(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    value: &IrConstValue,
    interner: &Interner,
) -> Result<(), EmitError> {
    if matches!(value, IrConstValue::Unit) {
        // Unit inline: nothing to push; callers that need a unit value must handle separately
        return Ok(());
    }
    if let Some(i) = cp.intern(value, interner)? {
        fe.emit_ld_cst(i);
    }
    Ok(())
}

/// Emit bytecode for an `IrRvalue`, leaving the result value on the stack.
pub fn emit_rvalue(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    tp: &mut TypePool,
    type_arena: &Arena<IrType>,
    rvalue: &IrRvalue,
    interner: &Interner,
) -> Result<(), EmitError> {
    match rvalue {
        IrRvalue::Use(operand) => emit_operand(fe, cp, operand, interner),
        IrRvalue::Const(value) => emit_const_value(fe, cp, value, interner),
        IrRvalue::BinOp { op, left, right } => {
            emit_operand(fe, cp, left, interner)?;
            emit_operand(fe, cp, right, interner)?;
            fe.emit_binop(binop_opcode(*op));
            Ok(())
        }
        IrRvalue::UnaryOp { op, operand } => {
            emit_operand(fe, cp, operand, interner)?;
            fe.emit_unop(unaryop_opcode(*op));
            Ok(())
        }
        IrRvalue::Call { callee, args, tail } => {
            emit_rvalue_call(fe, cp, callee, args, *tail, interner)
        }
        IrRvalue::MakeProduct { fields, .. } => emit_rvalue_make_product(fe, cp, fields, interner),
        IrRvalue::MakeVariant { tag, payload, .. } => {
            emit_rvalue_make_variant(fe, cp, *tag, payload, interner)
        }
        IrRvalue::MakeArray { elem_ty, elems } => {
            emit_rvalue_make_array(fe, cp, tp, type_arena, *elem_ty, elems, interner)
        }
        IrRvalue::MakeClosure { fn_id, captures } => {
            emit_rvalue_make_closure(fe, cp, fn_id.raw(), captures, interner)
        }
        IrRvalue::FieldGet { object, index } => {
            emit_operand(fe, cp, object, interner)?;
            fe.emit_ld_fld(*index)?;
            Ok(())
        }
        IrRvalue::IndexGet { array, index } => {
            emit_operand(fe, cp, array, interner)?;
            emit_operand(fe, cp, index, interner)?;
            encode_no_operand(&mut fe.code, Opcode::LD_IDX);
            fe.pop_n(1);
            Ok(())
        }
        IrRvalue::GetTag { value } => {
            emit_operand(fe, cp, value, interner)?;
            fe.emit_ld_tag();
            Ok(())
        }
        IrRvalue::GetPayload { value, field, .. } => {
            emit_operand(fe, cp, value, interner)?;
            let f = u8::try_from(*field).map_err(|_| EmitError::OperandOverflow {
                desc: "payload field index exceeds 255".into(),
            })?;
            encode_u8(&mut fe.code, Opcode::LD_PAY, f);
            Ok(())
        }
        IrRvalue::AllocRef { ty } => {
            let type_id = tp.lower_ir_type(*ty, type_arena)?;
            fe.emit_alc_ref(type_id);
            Ok(())
        }
        IrRvalue::AllocArena { ty } => {
            let type_id = tp.lower_ir_type(*ty, type_arena)?;
            fe.emit_alc_arn(type_id);
            Ok(())
        }
        IrRvalue::Deref { ptr } => emit_rvalue_deref(fe, cp, ptr, interner),
        IrRvalue::Cast { operand, from, to } => {
            emit_operand(fe, cp, operand, interner)?;
            emit_rvalue_cast(fe, type_arena, *from, *to)
        }
        IrRvalue::Spawn { callee, args } => emit_rvalue_spawn(fe, cp, callee, args, interner),
        IrRvalue::Await { task } => {
            emit_operand(fe, cp, task, interner)?;
            encode_no_operand(&mut fe.code, Opcode::TSK_AWT);
            Ok(())
        }
        IrRvalue::ForeignCall { fn_idx, args } => {
            emit_rvalue_foreign_call(fe, cp, *fn_idx, args, interner)
        }
    }
}

fn emit_rvalue_call(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    callee: &IrCallee,
    args: &[IrOperand],
    tail: bool,
    interner: &Interner,
) -> Result<(), EmitError> {
    for arg in args {
        emit_operand(fe, cp, arg, interner)?;
    }
    let arg_count = i32::try_from(args.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many args".into(),
    })?;
    emit_callee(fe, callee, arg_count, tail);
    Ok(())
}

fn emit_rvalue_make_product(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    fields: &[IrOperand],
    interner: &Interner,
) -> Result<(), EmitError> {
    let field_count = u32::try_from(fields.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many product fields".into(),
    })?;
    for field in fields {
        emit_operand(fe, cp, field, interner)?;
    }
    let pop_count = i32::try_from(fields.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many fields".into(),
    })?;
    fe.emit_mk_prd(field_count, pop_count)?;
    Ok(())
}

fn emit_rvalue_make_variant(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    tag: u32,
    payload: &[IrOperand],
    interner: &Interner,
) -> Result<(), EmitError> {
    for p in payload {
        emit_operand(fe, cp, p, interner)?;
    }
    let payload_count = i32::try_from(payload.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many variant payload fields".into(),
    })?;
    fe.emit_mk_var(tag);
    if payload_count > 0 {
        fe.pop_n(payload_count - 1); // N values → 1 variant = -(N-1)
    } else {
        fe.push_n(1); // 0 values → 1 variant = +1
    }
    Ok(())
}

fn emit_rvalue_make_array(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    tp: &mut TypePool,
    type_arena: &Arena<IrType>,
    elem_ty: IrTypeIdx,
    elems: &[IrOperand],
    interner: &Interner,
) -> Result<(), EmitError> {
    let len = u32::try_from(elems.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "array too large".into(),
    })?;
    let len_const = IrConstValue::Int(i64::from(len));
    if let Some(i) = cp.intern(&len_const, interner)? {
        fe.emit_ld_cst(i);
    }
    let type_id = tp.lower_ir_type(elem_ty, type_arena)?;
    encode_u32(&mut fe.code, Opcode::MK_ARR, type_id);
    fe.pop_n(1);
    fe.push_n(1);
    emit_rvalue_array_init(fe, cp, elems, interner)
}

fn emit_rvalue_array_init(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    elems: &[IrOperand],
    interner: &Interner,
) -> Result<(), EmitError> {
    for (i, elem) in elems.iter().enumerate() {
        fe.emit_dup();
        let idx_const =
            IrConstValue::Int(i64::try_from(i).map_err(|_| EmitError::UnresolvableType {
                desc: "array index too large".into(),
            })?);
        if let Some(ci) = cp.intern(&idx_const, interner)? {
            fe.emit_ld_cst(ci);
        }
        emit_operand(fe, cp, elem, interner)?;
        encode_no_operand(&mut fe.code, Opcode::ST_IDX);
        fe.pop_n(3);
    }
    Ok(())
}

fn emit_rvalue_make_closure(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    fn_raw: u32,
    captures: &[IrOperand],
    interner: &Interner,
) -> Result<(), EmitError> {
    let fn_const = IrConstValue::FnRef(fn_raw);
    if let Some(i) = cp.intern(&fn_const, interner)? {
        fe.emit_ld_cst(i);
    }
    let capture_count = u32::try_from(captures.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many captures".into(),
    })?;
    let pop_count = i32::try_from(captures.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many captures".into(),
    })?;
    for cap in captures {
        emit_operand(fe, cp, cap, interner)?;
    }
    fe.emit_mk_prd(capture_count, pop_count)?;
    // Stack: [fn_id, env_product] → make 2-field product = closure
    fe.emit_mk_prd(2, 2)?;
    Ok(())
}

fn emit_rvalue_deref(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    ptr: &IrOperand,
    interner: &Interner,
) -> Result<(), EmitError> {
    emit_operand(fe, cp, ptr, interner)?;
    let zero = IrConstValue::Int(0);
    if let Some(i) = cp.intern(&zero, interner)? {
        fe.emit_ld_cst(i);
    }
    encode_no_operand(&mut fe.code, Opcode::LD_IDX);
    fe.pop_n(1);
    Ok(())
}

fn emit_rvalue_spawn(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    callee: &IrCallee,
    args: &[IrOperand],
    interner: &Interner,
) -> Result<(), EmitError> {
    for arg in args {
        emit_operand(fe, cp, arg, interner)?;
    }
    let fn_id = match callee {
        IrCallee::Direct(fn_idx) => fn_idx.raw(),
        IrCallee::Instance { instance_fn, .. } => instance_fn.raw(),
        IrCallee::Indirect(_) => {
            return Err(EmitError::UnsupportedFeature {
                desc: "indirect spawn not supported".into(),
            });
        }
    };
    encode_u32(&mut fe.code, Opcode::TSK_SPN, fn_id);
    let arg_count = i32::try_from(args.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many spawn args".into(),
    })?;
    fe.pop_n(arg_count);
    fe.push_n(1);
    Ok(())
}

fn emit_rvalue_foreign_call(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    fn_idx: u32,
    args: &[IrOperand],
    interner: &Interner,
) -> Result<(), EmitError> {
    for arg in args {
        emit_operand(fe, cp, arg, interner)?;
    }
    let arg_count = i32::try_from(args.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many FFI args".into(),
    })?;
    encode_u32(&mut fe.code, Opcode::INV_FFI, fn_idx);
    fe.pop_n(arg_count);
    fe.push_n(1);
    Ok(())
}

fn emit_callee(fe: &mut FnEmitter, callee: &IrCallee, arg_count: i32, tail: bool) {
    match callee {
        IrCallee::Direct(fn_idx) => {
            if tail {
                fe.emit_inv_tail(fn_idx.raw(), false);
            } else {
                fe.emit_inv(fn_idx.raw(), false, arg_count);
            }
        }
        IrCallee::Indirect(local) => {
            fe.emit_ld_loc(*local);
            fe.emit_inv_dyn(arg_count);
        }
        IrCallee::Instance { instance_fn, .. } => {
            if tail {
                fe.emit_inv_tail(instance_fn.raw(), false);
            } else {
                fe.emit_inv(instance_fn.raw(), false, arg_count);
            }
        }
    }
}

const fn is_signed_int(ty: &IrType) -> bool {
    matches!(
        ty,
        IrType::Int8 | IrType::Int16 | IrType::Int32 | IrType::Int64
    )
}

const fn is_unsigned_int(ty: &IrType) -> bool {
    matches!(
        ty,
        IrType::UInt8 | IrType::UInt16 | IrType::UInt32 | IrType::UInt64
    )
}

const fn is_float(ty: &IrType) -> bool {
    matches!(ty, IrType::Float32 | IrType::Float64)
}

const fn bit_width(ty: &IrType) -> u8 {
    match ty {
        IrType::Int8 | IrType::UInt8 | IrType::Bool => 8,
        IrType::Int16 | IrType::UInt16 => 16,
        IrType::Int32 | IrType::UInt32 | IrType::Rune | IrType::Float32 => 32,
        IrType::Int64 | IrType::UInt64 | IrType::Float64 => 64,
        _ => 0,
    }
}

fn emit_rvalue_cast(
    fe: &mut FnEmitter,
    type_arena: &Arena<IrType>,
    from: IrTypeIdx,
    to: IrTypeIdx,
) -> Result<(), EmitError> {
    let from_ty = type_arena[from].clone();
    let to_ty = type_arena[to].clone();
    let from_w = bit_width(&from_ty);
    let to_w = bit_width(&to_ty);

    // Same type → no-op
    if from.raw() == to.raw() {
        return Ok(());
    }

    // int → float
    if (is_signed_int(&from_ty) || is_unsigned_int(&from_ty)) && is_float(&to_ty) {
        encode_no_operand(&mut fe.code, Opcode::CNV_ITF);
        return Ok(());
    }

    // float → int
    if is_float(&from_ty) && (is_signed_int(&to_ty) || is_unsigned_int(&to_ty)) {
        encode_no_operand(&mut fe.code, Opcode::CNV_FTI);
        return Ok(());
    }

    // float → float
    if is_float(&from_ty) && is_float(&to_ty) {
        if to_w > from_w {
            encode_u8(&mut fe.code, Opcode::CNV_WDN, to_w);
        } else {
            encode_u8(&mut fe.code, Opcode::CNV_NRW, to_w);
        }
        return Ok(());
    }

    // int → int
    let from_int = is_signed_int(&from_ty) || is_unsigned_int(&from_ty);
    let to_int = is_signed_int(&to_ty) || is_unsigned_int(&to_ty);
    if from_int && to_int {
        match to_w.cmp(&from_w) {
            Ordering::Greater => {
                // Widen
                if is_unsigned_int(&to_ty) {
                    encode_u8(&mut fe.code, Opcode::CNV_WDN_UN, to_w);
                } else {
                    encode_u8(&mut fe.code, Opcode::CNV_WDN, to_w);
                }
            }
            Ordering::Less => {
                // Narrow
                encode_u8(&mut fe.code, Opcode::CNV_NRW, to_w);
            }
            Ordering::Equal => {
                // Same width, different signedness → reinterpret
                encode_no_operand(&mut fe.code, Opcode::CNV_TRM);
            }
        }
        return Ok(());
    }

    Err(EmitError::UnresolvableType {
        desc: "unsupported cast between non-numeric types".into(),
    })
}

const fn binop_opcode(op: IrBinOp) -> Opcode {
    match op {
        IrBinOp::IAdd => Opcode::I_ADD,
        IrBinOp::ISub => Opcode::I_SUB,
        IrBinOp::IMul => Opcode::I_MUL,
        IrBinOp::IDiv => Opcode::I_DIV,
        IrBinOp::IRem => Opcode::I_REM,
        IrBinOp::UAdd => Opcode::I_ADD_UN,
        IrBinOp::USub => Opcode::I_SUB_UN,
        IrBinOp::UMul => Opcode::I_MUL_UN,
        IrBinOp::UDiv => Opcode::I_DIV_UN,
        IrBinOp::URem => Opcode::I_REM_UN,
        IrBinOp::FAdd => Opcode::F_ADD,
        IrBinOp::FSub => Opcode::F_SUB,
        IrBinOp::FMul => Opcode::F_MUL,
        IrBinOp::FDiv => Opcode::F_DIV,
        IrBinOp::FRem => Opcode::F_REM,
        IrBinOp::IEq => Opcode::CMP_EQ,
        IrBinOp::INe => Opcode::CMP_NE,
        IrBinOp::ILt => Opcode::CMP_LT,
        IrBinOp::ILe => Opcode::CMP_LE,
        IrBinOp::IGt => Opcode::CMP_GT,
        IrBinOp::IGe => Opcode::CMP_GE,
        IrBinOp::ULt => Opcode::CMP_LT_UN,
        IrBinOp::ULe => Opcode::CMP_LE_UN,
        IrBinOp::UGt => Opcode::CMP_GT_UN,
        IrBinOp::UGe => Opcode::CMP_GE_UN,
        IrBinOp::FEq => Opcode::CMP_F_EQ,
        IrBinOp::FNe => Opcode::CMP_F_NE,
        IrBinOp::FLt => Opcode::CMP_F_LT,
        IrBinOp::FLe => Opcode::CMP_F_LE,
        IrBinOp::FGt => Opcode::CMP_F_GT,
        IrBinOp::FGe => Opcode::CMP_F_GE,
        IrBinOp::And => Opcode::B_AND,
        IrBinOp::Or => Opcode::B_OR,
        IrBinOp::Xor => Opcode::B_XOR,
        IrBinOp::Shl => Opcode::B_SHL,
        IrBinOp::Shr => Opcode::B_SHR,
        IrBinOp::ShrUn => Opcode::B_SHR_UN,
    }
}

const fn unaryop_opcode(op: IrUnaryOp) -> Opcode {
    match op {
        IrUnaryOp::INeg => Opcode::I_NEG,
        IrUnaryOp::FNeg => Opcode::F_NEG,
        IrUnaryOp::Not => Opcode::B_NOT,
    }
}
