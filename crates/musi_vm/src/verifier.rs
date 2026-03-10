//! Single-pass pre-execution bytecode verifier.
//!
//! Validates each function in the module before the interpreter begins.
//! Checks operand bounds, jump targets, and stack depth.

use std::collections::HashSet;

use crate::error::VmError;
use crate::loader::{LoadedFn, LoadedModule};
#[allow(clippy::wildcard_imports)]
use crate::opcode::*;

/// Instruction length from top-2-bits encoding.
///
/// `0x00..=0x3F` → 1 byte, `0x40..=0x7F` → 2 bytes,
/// `0x80..=0xBF` → 3 bytes, `0xC0..=0xFF` → 5 bytes.
pub const fn instr_len(op: u8) -> usize {
    match op >> 6 {
        0 => 1,
        1 => 2,
        2 => 3,
        _ => 5, // 3 → 5
    }
}

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

/// Compute the stack depth delta for an opcode and verify its operands.
///
/// Returns the net stack change.
fn verify_op(
    op: u8,
    ops: &Operands<'_>,
    len: usize,
    module: &LoadedModule,
    local_count: usize,
    const_len: usize,
    boundaries: &HashSet<usize>,
) -> Result<i32, VmError> {
    match op {
        // Net 0: no-ops, terminators, unary, struct ops, effects, concurrency
        NOP | BRK | HLT | RET | RET_U | UNR | SWP | I_NEG | F_NEG | B_NOT | CNV_WDN
        | CNV_WDN_UN | CNV_NRW | CNV_ITF | CNV_FTI | CNV_TRM | ST_FLD | ST_FLD_W | LD_FLD
        | MK_PRD | MK_VAR | MK_VAR_W | LD_PAY | LD_TAG | CMP_TAG | CMP_TAG_W | LD_LEN | LD_IDX
        | ST_IDX | EFF_PSH | EFF_POP | EFF_RES_C | EFF_ABT | EFF_DO | EFF_RES | INV_DYN
        | TSK_SPN | TSK_CHS | TSK_CHR | TSK_CMK | TSK_AWT => Ok(0),

        // Push 1: dup, load global, alloc
        DUP | LD_GLB | MK_ARR | ALC_REF | ALC_MAN | ALC_ARN => Ok(1),

        // Net -1: pop, store global, free, binary ops
        POP | ST_GLB | FRE | I_ADD | I_ADD_UN | I_SUB | I_SUB_UN | I_MUL | I_MUL_UN | I_DIV
        | I_DIV_UN | I_REM | I_REM_UN | F_ADD | F_SUB | F_MUL | F_DIV | F_REM | B_AND | B_OR
        | B_XOR | B_SHL | B_SHR | B_SHR_UN | CMP_EQ | CMP_NE | CMP_LT | CMP_LT_UN | CMP_LE
        | CMP_LE_UN | CMP_GT | CMP_GT_UN | CMP_GE | CMP_GE_UN | CMP_F_EQ | CMP_F_NE | CMP_F_LT
        | CMP_F_LE | CMP_F_GT | CMP_F_GE => Ok(-1),

        // Load local (narrow): push 1
        LD_LOC => {
            check_local(ops.u8_op(), local_count, "ld.loc")?;
            Ok(1)
        }
        // Store local (narrow): pop 1
        ST_LOC => {
            check_local(ops.u8_op(), local_count, "st.loc")?;
            Ok(-1)
        }
        // Load local (wide): push 1
        LD_LOC_W => {
            check_local(ops.u16_op(), local_count, "ld.loc.w")?;
            Ok(1)
        }
        // Store local (wide): pop 1
        ST_LOC_W => {
            check_local(ops.u16_op(), local_count, "st.loc.w")?;
            Ok(-1)
        }

        // Load constant: push 1
        LD_CST => {
            check_const(ops.u8_op(), const_len, "ld.cst")?;
            Ok(1)
        }
        LD_CST_W => {
            check_const(ops.u16_op(), const_len, "ld.cst.w")?;
            Ok(1)
        }

        // Jumps (i16 operand)
        JMP => {
            check_jump_target(ops.ip, len, ops.i16_jump(), boundaries)?;
            Ok(0)
        }
        JMP_T | JMP_F => {
            check_jump_target(ops.ip, len, ops.i16_jump(), boundaries)?;
            Ok(-1)
        }
        // Wide jumps (i32 operand)
        JMP_W => {
            check_jump_target(ops.ip, len, ops.i32_jump()?, boundaries)?;
            Ok(0)
        }
        JMP_T_W | JMP_F_W => {
            check_jump_target(ops.ip, len, ops.i32_jump()?, boundaries)?;
            Ok(-1)
        }

        // Invocations
        INV | INV_EFF => {
            check_fn_id(ops.u32_op(), module, "inv")?;
            Ok(1)
        }
        INV_TAL | INV_TAL_EFF => {
            check_fn_id(ops.u32_op(), module, "inv.tal")?;
            Ok(0)
        }
        _ => Err(VmError::Verify {
            desc: format!("unknown opcode {op:#04x}").into_boxed_str(),
        }),
    }
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
        let op = code[ip];
        let len = instr_len(op);
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
