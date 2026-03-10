//! Single-pass pre-execution bytecode verifier.
//!
//! Validates each function in the module before the interpreter begins.
//! Checks operand bounds, jump targets, and stack depth.

use std::collections::HashSet;

use musi_bytecode::{Opcode, instr_len};

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

    fn i16_jump(&self) -> isize {
        let lo = self.code[self.ip + 1];
        let hi = self.code[self.ip + 2];
        let raw = i16::from_le_bytes([lo, hi]);
        isize::from(raw)
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
        | Opcode::CNV_WDN
        | Opcode::CNV_WDN_UN
        | Opcode::CNV_NRW
        | Opcode::CNV_ITF
        | Opcode::CNV_FTI
        | Opcode::CNV_TRM
        | Opcode::ST_FLD
        | Opcode::ST_FLD_W
        | Opcode::LD_FLD
        | Opcode::MK_PRD
        | Opcode::MK_VAR
        | Opcode::MK_VAR_W
        | Opcode::LD_PAY
        | Opcode::LD_TAG
        | Opcode::CMP_TAG
        | Opcode::CMP_TAG_W
        | Opcode::LD_LEN
        | Opcode::LD_IDX
        | Opcode::ST_IDX
        | Opcode::EFF_PSH
        | Opcode::EFF_POP
        | Opcode::EFF_RES_C
        | Opcode::EFF_ABT
        | Opcode::EFF_DO
        | Opcode::EFF_RES
        | Opcode::INV_DYN
        | Opcode::TSK_SPN
        | Opcode::TSK_CHS
        | Opcode::TSK_CHR
        | Opcode::TSK_CMK
        | Opcode::TSK_AWT => Some(0),

        // Push 1: dup, load global, alloc
        Opcode::DUP
        | Opcode::LD_GLB
        | Opcode::MK_ARR
        | Opcode::ALC_REF
        | Opcode::ALC_MAN
        | Opcode::ALC_ARN => Some(1),

        // Net -1: pop, store global, free, binary ops
        Opcode::POP
        | Opcode::ST_GLB
        | Opcode::FRE
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
        Opcode::JMP => {
            check_jump_target(ops.ip, len, ops.i16_jump(), boundaries)?;
            Ok(0)
        }
        Opcode::JMP_T | Opcode::JMP_F => {
            check_jump_target(ops.ip, len, ops.i16_jump(), boundaries)?;
            Ok(-1)
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
            check_fn_id(ops.u32_op(), module, "inv")?;
            Ok(1)
        }
        Opcode::INV_TAL | Opcode::INV_TAL_EFF => {
            check_fn_id(ops.u32_op(), module, "inv.tal")?;
            Ok(0)
        }
        Opcode::INV_FFI => {
            let idx = usize::try_from(ops.u32_op()).unwrap_or(usize::MAX);
            if idx >= module.foreign_fns.len() {
                return Err(VmError::Verify {
                    desc: format!("inv.ffi index {idx} out of bounds").into_boxed_str(),
                });
            }
            Ok(0)
        }
        _ => Err(VmError::Verify {
            desc: format!("unknown opcode {:#04x}", op.0).into_boxed_str(),
        }),
    }
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

        ip += len;
    }

    Ok(())
}
