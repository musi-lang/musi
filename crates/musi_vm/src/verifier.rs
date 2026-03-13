//! Single-pass pre-execution bytecode verifier.
//!
//! Validates each function in the module before the interpreter begins.
//! Checks operand bounds, jump targets, and stack depth.

use std::collections::HashSet;

use musi_bc::{Opcode, instr_len};

use crate::error::VmError;
use crate::loader::{LoadedFn, LoadedModule};

/// Verify all functions in the module before execution.
///
/// # Errors
///
/// Returns `VmError::Verify` if any function has invalid operands, jump
/// targets that do not land on instruction boundaries, or a stack depth
/// that exceeds the declared `max_stack`.
pub fn verify(module: &LoadedModule) -> Result<(), VmError> {
    for func in &module.functions {
        verify_fn(func, module)?;
    }
    Ok(())
}

/// Collect instruction boundary offsets for the given bytecode.
fn collect_boundaries(code: &[u8]) -> Result<HashSet<usize>, VmError> {
    let mut boundaries = HashSet::new();
    let mut ip = 0usize;
    while ip < code.len() {
        let _ = boundaries.insert(ip);
        let op = code[ip];
        ip += instr_len(op);
    }
    if ip != code.len() {
        return Err(VmError::Verify {
            desc: "instruction stream does not end on a boundary".into(),
        });
    }
    Ok(boundaries)
}

/// Check that a local slot index is within bounds.
fn check_local(slot: usize, local_count: usize, name: &str) -> Result<(), VmError> {
    if slot >= local_count {
        return Err(VmError::Verify {
            desc: format!("{name} slot {slot} >= local_count {local_count}").into_boxed_str(),
        });
    }
    Ok(())
}

/// Check that a constant pool index is within bounds.
fn check_const(idx: usize, const_len: usize, name: &str) -> Result<(), VmError> {
    if idx >= const_len {
        return Err(VmError::Verify {
            desc: format!("{name} index {idx} >= const pool size {const_len}").into_boxed_str(),
        });
    }
    Ok(())
}

/// Check that a function id exists in the module (when the module has functions).
fn check_fn_id(fn_id: u32, module: &LoadedModule, name: &str) -> Result<(), VmError> {
    let found = module.functions.iter().any(|f| f.fn_id == fn_id);
    if !found && !module.functions.is_empty() {
        return Err(VmError::Verify {
            desc: format!("{name} target fn_id {fn_id} not in module").into_boxed_str(),
        });
    }
    Ok(())
}

/// Check that a jump offset lands on an instruction boundary.
fn check_jump_target(
    ip: usize,
    len: usize,
    offset: isize,
    boundaries: &HashSet<usize>,
) -> Result<(), VmError> {
    let after = ip + len;
    let target = after.wrapping_add_signed(offset);
    if !boundaries.contains(&target) {
        return Err(VmError::Verify {
            desc: format!("jump to non-instruction boundary at {target}").into_boxed_str(),
        });
    }
    Ok(())
}

/// Operand decoders.
struct Operands<'a> {
    code: &'a [u8],
    ip: usize,
}

impl Operands<'_> {
    fn u8_op(&self) -> usize {
        usize::from(self.code[self.ip + 1])
    }

    fn u16_op(&self) -> usize {
        let lo = self.code[self.ip + 1];
        let hi = self.code[self.ip + 2];
        usize::from(u16::from_le_bytes([lo, hi]))
    }

    fn u32_op(&self) -> u32 {
        let b = [
            self.code[self.ip + 1],
            self.code[self.ip + 2],
            self.code[self.ip + 3],
            self.code[self.ip + 4],
        ];
        u32::from_le_bytes(b)
    }

    fn i32_jump(&self) -> Result<isize, VmError> {
        let b = [
            self.code[self.ip + 1],
            self.code[self.ip + 2],
            self.code[self.ip + 3],
            self.code[self.ip + 4],
        ];
        let raw = i32::from_le_bytes(b);
        isize::try_from(raw).map_err(|_| VmError::Verify {
            desc: "jump offset overflows isize".into(),
        })
    }
}

/// Stack depth delta for fixed-pattern opcodes (no operand validation needed).
///
/// Returns `Some(delta)` if the opcode is in a fixed group, `None` otherwise.
const fn fixed_stack_delta(op: Opcode) -> Option<i32> {
    match op {
        // Net 0: no-ops, terminators, unary, struct ops, effects, concurrency
        Opcode::NOP
        | Opcode::BRK
        | Opcode::HLT
        | Opcode::RET
        | Opcode::RET_U
        | Opcode::UNR
        | Opcode::SWP
        | Opcode::I_NEG
        | Opcode::F_NEG
        | Opcode::B_NOT
        | Opcode::CNV_ITF
        | Opcode::CNV_FTI
        | Opcode::CNV_TRM
        | Opcode::LD_FLD
        | Opcode::MK_ARR
        | Opcode::MK_VAR
        | Opcode::MK_VAR_W
        | Opcode::LD_PAY
        | Opcode::LD_TAG
        | Opcode::CMP_TAG
        | Opcode::CMP_TAG_W
        | Opcode::LD_LEN
        | Opcode::EFF_PSH
        | Opcode::EFF_POP
        | Opcode::EFF_RES_C
        | Opcode::EFF_ABT
        | Opcode::EFF_RES
        | Opcode::TSK_CHR
        | Opcode::TSK_AWT
        | Opcode::ALC_REF
        | Opcode::TYP_CHK => Some(0),

        // Push 1: dup, load global, alloc, channel make, load upvalue
        Opcode::DUP | Opcode::LD_GLB | Opcode::ALC_ARN | Opcode::TSK_CMK | Opcode::LD_UPV => Some(1),

        // Net -1: pop, store global, free, index load, channel send, binary ops
        Opcode::POP
        | Opcode::ST_GLB
        | Opcode::FRE
        | Opcode::LD_IDX
        | Opcode::TSK_CHS
        | Opcode::I_ADD
        | Opcode::I_ADD_UN
        | Opcode::I_SUB
        | Opcode::I_SUB_UN
        | Opcode::I_MUL
        | Opcode::I_MUL_UN
        | Opcode::I_DIV
        | Opcode::I_DIV_UN
        | Opcode::I_REM
        | Opcode::I_REM_UN
        | Opcode::F_ADD
        | Opcode::F_SUB
        | Opcode::F_MUL
        | Opcode::F_DIV
        | Opcode::F_REM
        | Opcode::B_AND
        | Opcode::B_OR
        | Opcode::B_XOR
        | Opcode::B_SHL
        | Opcode::B_SHR
        | Opcode::B_SHR_UN
        | Opcode::CMP_EQ
        | Opcode::CMP_NE
        | Opcode::CMP_LT
        | Opcode::CMP_LT_UN
        | Opcode::CMP_LE
        | Opcode::CMP_LE_UN
        | Opcode::CMP_GT
        | Opcode::CMP_GT_UN
        | Opcode::CMP_GE
        | Opcode::CMP_GE_UN
        | Opcode::CMP_F_EQ
        | Opcode::CMP_F_NE
        | Opcode::CMP_F_LT
        | Opcode::CMP_F_LE
        | Opcode::CMP_F_GT
        | Opcode::CMP_F_GE => Some(-1),

        // Net -2: store field (pops obj+value)
        Opcode::ST_FLD => Some(-2),

        // Net -3: store index (pops value+base+index)
        Opcode::ST_IDX => Some(-3),

        _ => None,
    }
}

/// Verify opcodes that need operand validation and return their stack delta.
fn verify_operand_op(
    op: Opcode,
    ops: &Operands<'_>,
    len: usize,
    module: &LoadedModule,
    local_count: usize,
    const_len: usize,
    boundaries: &HashSet<usize>,
) -> Result<i32, VmError> {
    match op {
        Opcode::LD_LOC => {
            check_local(ops.u8_op(), local_count, "ld.loc")?;
            Ok(1)
        }
        Opcode::ST_LOC => {
            check_local(ops.u8_op(), local_count, "st.loc")?;
            Ok(-1)
        }
        Opcode::LD_LOC_W => {
            check_local(ops.u16_op(), local_count, "ld.loc.w")?;
            Ok(1)
        }
        Opcode::ST_LOC_W => {
            check_local(ops.u16_op(), local_count, "st.loc.w")?;
            Ok(-1)
        }
        Opcode::LD_CST => {
            check_const(ops.u8_op(), const_len, "ld.cst")?;
            Ok(1)
        }
        Opcode::LD_CST_W => {
            check_const(ops.u16_op(), const_len, "ld.cst.w")?;
            Ok(1)
        }
        Opcode::JMP_W => {
            check_jump_target(ops.ip, len, ops.i32_jump()?, boundaries)?;
            Ok(0)
        }
        Opcode::JMP_T_W | Opcode::JMP_F_W => {
            check_jump_target(ops.ip, len, ops.i32_jump()?, boundaries)?;
            Ok(-1)
        }
        Opcode::INV | Opcode::INV_EFF => {
            let fn_id = ops.u32_op();
            check_fn_id(fn_id, module, "inv")?;
            let param_count = module
                .fn_by_id(fn_id)
                .map_or(0, |(_, f)| i32::from(f.param_count));
            Ok(1 - param_count)
        }
        Opcode::INV_TAL | Opcode::INV_TAL_EFF => {
            let fn_id = ops.u32_op();
            check_fn_id(fn_id, module, "inv.tal")?;
            let param_count = module
                .fn_by_id(fn_id)
                .map_or(0, |(_, f)| i32::from(f.param_count));
            Ok(-param_count)
        }
        Opcode::MK_PRD => {
            let field_count = i32::try_from(ops.u8_op()).unwrap_or(i32::MAX);
            Ok(1 - field_count)
        }
        Opcode::INV_DYN => {
            let arg_count = i32::try_from(ops.u8_op()).unwrap_or(i32::MAX);
            Ok(-arg_count)
        }
        Opcode::EFF_DO => {
            let op_id = ops.u32_op();
            let param_count = lookup_effect_op_param_count(module, op_id);
            Ok(1 - param_count)
        }
        Opcode::TSK_SPN => {
            let fn_id = ops.u32_op();
            check_fn_id(fn_id, module, "tsk.spn")?;
            let param_count = module
                .fn_by_id(fn_id)
                .map_or(0, |(_, f)| i32::from(f.param_count));
            Ok(1 - param_count)
        }
        Opcode::INV_FFI => {
            let idx = usize::try_from(ops.u32_op()).unwrap_or(usize::MAX);
            if idx >= module.foreign_fns.len() {
                return Err(VmError::Verify {
                    desc: format!("inv.ffi index {idx} out of bounds").into_boxed_str(),
                });
            }
            let param_count = i32::from(module.foreign_fns[idx].param_count);
            Ok(1 - param_count)
        }
        Opcode::MK_CLO => {
            let fn_id = ops.u32_op();
            check_fn_id(fn_id, module, "mk.clo")?;
            let upvalue_count = module
                .fn_by_id(fn_id)
                .map_or(0, |(_, f)| i32::from(f.upvalue_count));
            Ok(1 - upvalue_count)
        }
        _ => Err(VmError::Verify {
            desc: format!("unknown opcode {:#04x}", op.0).into_boxed_str(),
        }),
    }
}

/// Look up the parameter count for an effect operation by its flat index.
fn lookup_effect_op_param_count(module: &LoadedModule, op_id: u32) -> i32 {
    let mut idx = 0u32;
    for effect in &module.effects {
        for op in &effect.ops {
            if idx == op_id {
                return i32::try_from(op.param_type_ids.len()).unwrap_or(0);
            }
            idx += 1;
        }
    }
    0
}

/// Compute the stack depth delta for an opcode and verify its operands.
///
/// Returns the net stack change.
fn verify_op(
    op: Opcode,
    ops: &Operands<'_>,
    len: usize,
    module: &LoadedModule,
    local_count: usize,
    const_len: usize,
    boundaries: &HashSet<usize>,
) -> Result<i32, VmError> {
    if let Some(delta) = fixed_stack_delta(op) {
        return Ok(delta);
    }
    verify_operand_op(op, ops, len, module, local_count, const_len, boundaries)
}

fn verify_fn(func: &LoadedFn, module: &LoadedModule) -> Result<(), VmError> {
    let code = &func.code;
    let const_len = module.consts.len();
    let local_count = usize::from(func.local_count);
    let max_stack = usize::from(func.max_stack);

    let boundaries = collect_boundaries(code)?;

    let mut ip = 0usize;
    let mut depth: i32 = 0;

    while ip < code.len() {
        let op = Opcode(code[ip]);
        let len = instr_len(op.0);
        let ops = Operands { code, ip };

        depth += verify_op(op, &ops, len, module, local_count, const_len, &boundaries)?;

        if depth > 0 {
            let d = usize::try_from(depth).unwrap_or(usize::MAX);
            if d > max_stack && max_stack > 0 {
                return Err(VmError::Verify {
                    desc: format!("stack depth {d} exceeds max_stack {max_stack} at offset {ip}")
                        .into_boxed_str(),
                });
            }
        }

        let is_terminator = matches!(op, Opcode::RET | Opcode::RET_U | Opcode::UNR | Opcode::HLT);
        let is_unconditional_jump = matches!(op, Opcode::JMP_W);
        if is_terminator || is_unconditional_jump {
            depth = 0;
        }

        ip += len;
    }

    Ok(())
}
