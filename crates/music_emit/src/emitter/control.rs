//! Control-flow instruction emission.

use music_ir::{
    IrConstValue, IrEffectOpId, IrInst, IrLabel, IrLocal, IrOperand, IrPlace, IrRvalue,
    IrSwitchArm,
};
use music_shared::{Arena, Interner};

use music_ir::IrType;

use crate::const_pool::ConstPool;
use crate::error::EmitError;
use crate::opcode::{Opcode, encode_no_operand, encode_u8, encode_u16};
use crate::type_pool::TypePool;

use super::fn_emitter::FnEmitter;
use super::rvalue::{emit_operand, emit_rvalue};

/// Emit a single `IrInst` into `fe`.
///
/// Returns `true` if this instruction terminates the basic block (ret / tail call).
pub fn emit_inst(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    tp: &mut TypePool,
    type_arena: &Arena<IrType>,
    inst: &IrInst,
    interner: &Interner,
) -> Result<bool, EmitError> {
    match inst {
        IrInst::Assign { dst, rvalue, .. } => {
            emit_rvalue(fe, cp, tp, type_arena, rvalue, interner)?;
            let is_tail = matches!(rvalue, IrRvalue::Call { tail: true, .. });
            if !is_tail {
                fe.emit_st_loc(*dst);
            }
        }
        IrInst::Store { dst, value, .. } => {
            emit_inst_store(fe, cp, dst, value, interner)?;
        }
        IrInst::Goto(label) => {
            fe.emit_jmp(*label);
        }
        IrInst::Branch {
            cond,
            then_label,
            else_label,
            ..
        } => {
            emit_operand(fe, cp, cond, interner)?;
            fe.emit_jmp_t(*then_label);
            fe.emit_jmp(*else_label);
        }
        IrInst::Switch {
            scrutinee,
            arms,
            default,
            ..
        } => {
            emit_inst_switch(fe, cp, scrutinee, arms, *default, interner)?;
        }
        IrInst::Label(label) => {
            fe.emit_label(*label);
        }
        IrInst::Return { value, .. } => {
            emit_inst_return(fe, cp, value.as_ref(), interner)?;
            return Ok(true);
        }
        IrInst::EffectPush {
            effect,
            handler_fn,
            ..
        } => {
            fe.emit_eff_psh(effect.0, handler_fn.raw())?;
        }
        IrInst::EffectPop { effect, .. } => {
            fe.emit_eff_pop(effect.0)?;
        }
        IrInst::EffectDo { op, args, dst, .. } => {
            emit_inst_effect_do(fe, cp, *op, args, *dst, interner)?;
        }
        IrInst::EffectResume { value, .. } => {
            emit_operand(fe, cp, value, interner)?;
            fe.emit_eff_res();
        }
        IrInst::Nop => {}
    }
    Ok(false)
}

fn emit_inst_store(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    dst: &IrPlace,
    value: &IrOperand,
    interner: &Interner,
) -> Result<(), EmitError> {
    emit_operand(fe, cp, value, interner)?;
    match dst {
        IrPlace::Local(local) => {
            fe.emit_st_loc(*local);
        }
        IrPlace::Field { base, index } => {
            fe.emit_ld_loc(*base);
            if let Ok(i) = u8::try_from(*index) {
                encode_u8(&mut fe.code, Opcode::ST_FLD, i);
            } else {
                let i = u16::try_from(*index).map_err(|_| EmitError::OperandOverflow {
                    desc: "store field index exceeds 65535".into(),
                })?;
                encode_u16(&mut fe.code, Opcode::ST_FLD_W, i);
            }
            fe.pop_n(2);
        }
        IrPlace::Index { base, index } => {
            fe.emit_ld_loc(*base);
            emit_operand(fe, cp, index, interner)?;
            encode_no_operand(&mut fe.code, Opcode::ST_IDX);
            fe.pop_n(3);
        }
        IrPlace::Deref { ptr } => {
            fe.emit_ld_loc(*ptr);
            let zero = IrConstValue::Int(0);
            if let Some(i) = cp.intern(&zero, interner)? {
                fe.emit_ld_cst(i);
            }
            encode_no_operand(&mut fe.code, Opcode::ST_IDX);
            fe.pop_n(3);
        }
    }
    Ok(())
}

fn emit_inst_switch(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    scrutinee: &IrOperand,
    arms: &[IrSwitchArm],
    default: IrLabel,
    interner: &Interner,
) -> Result<(), EmitError> {
    for arm in arms {
        emit_operand(fe, cp, scrutinee, interner)?;
        fe.emit_get_tag();
        let arm_const = arm.value.clone();
        if let Some(i) = cp.intern(&arm_const, interner)? {
            fe.emit_ld_cst(i);
        }
        encode_no_operand(&mut fe.code, Opcode::CMP_EQ);
        fe.pop_n(1);
        fe.emit_jmp_t(arm.label);
    }
    fe.emit_jmp(default);
    Ok(())
}

fn emit_inst_return(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    value: Option<&IrOperand>,
    interner: &Interner,
) -> Result<(), EmitError> {
    match value {
        Some(IrOperand::Local(local)) => {
            fe.emit_ld_loc(*local);
            fe.emit_ret();
        }
        Some(IrOperand::Const(cv)) => {
            if matches!(cv, IrConstValue::Unit) {
                fe.emit_ret_u();
            } else if let Some(i) = cp.intern(cv, interner)? {
                fe.emit_ld_cst(i);
                fe.emit_ret();
            } else {
                fe.emit_ret_u();
            }
        }
        None => {
            fe.emit_ret_u();
        }
    }
    Ok(())
}

fn emit_inst_effect_do(
    fe: &mut FnEmitter,
    cp: &mut ConstPool,
    op: IrEffectOpId,
    args: &[IrOperand],
    dst: IrLocal,
    interner: &Interner,
) -> Result<(), EmitError> {
    for arg in args {
        emit_operand(fe, cp, arg, interner)?;
    }
    let arg_count = i32::try_from(args.len()).map_err(|_| EmitError::UnresolvableType {
        desc: "too many effect args".into(),
    })?;
    fe.emit_eff_do(op.0, arg_count);
    fe.emit_st_loc(dst);
    Ok(())
}
