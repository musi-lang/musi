//! Single-pass pre-execution bytecode verifier.
//!
//! Validates each function in the module before the interpreter begins.
//! Checks operand bounds, jump targets, and stack depth.

use std::collections::HashSet;

use crate::error::VmError;
use crate::loader::{LoadedFn, LoadedModule};

// Opcode constants (mirrors music_emit::opcode).
const NOP: u8 = 0x00;
const HLT: u8 = 0x01;
const RET: u8 = 0x02;
const RET_U: u8 = 0x03;
const UNR: u8 = 0x04;
const BRK: u8 = 0x05;
const DUP: u8 = 0x06;
const POP: u8 = 0x07;
const SWP: u8 = 0x08;

const I_ADD: u8 = 0x10;
const I_ADD_UN: u8 = 0x11;
const I_SUB: u8 = 0x12;
const I_SUB_UN: u8 = 0x13;
const I_MUL: u8 = 0x14;
const I_MUL_UN: u8 = 0x15;
const I_DIV: u8 = 0x16;
const I_DIV_UN: u8 = 0x17;
const I_REM: u8 = 0x18;
const I_REM_UN: u8 = 0x19;
const I_NEG: u8 = 0x1A;

const F_ADD: u8 = 0x20;
const F_SUB: u8 = 0x21;
const F_MUL: u8 = 0x22;
const F_DIV: u8 = 0x23;
const F_REM: u8 = 0x24;
const F_NEG: u8 = 0x25;

const B_AND: u8 = 0x30;
const B_OR: u8 = 0x31;
const B_XOR: u8 = 0x32;
const B_NOT: u8 = 0x33;
const B_SHL: u8 = 0x34;
const B_SHR: u8 = 0x36;
const B_SHR_UN: u8 = 0x37;

const CMP_EQ: u8 = 0x3B;
const CMP_NE: u8 = 0x3C;

const LD_LOC: u8 = 0x40;
const ST_LOC: u8 = 0x41;
const LD_CST: u8 = 0x42;
const ST_FLD: u8 = 0x43;
const MK_PRD: u8 = 0x44;
const LD_FLD: u8 = 0x45;
const MK_VAR: u8 = 0x46;
const LD_PAY: u8 = 0x47;
const CMP_TAG: u8 = 0x48;
const CNV_WDN: u8 = 0x49;
const CNV_WDN_UN: u8 = 0x4A;
const CNV_NRW: u8 = 0x4B;
const EFF_PSH: u8 = 0x4C;
const EFF_POP: u8 = 0x4D;

const CMP_LT: u8 = 0x50;
const CMP_LT_UN: u8 = 0x51;
const CMP_LE: u8 = 0x52;
const CMP_LE_UN: u8 = 0x53;
const CMP_GT: u8 = 0x54;
const CMP_GT_UN: u8 = 0x55;
const CMP_GE: u8 = 0x56;
const CMP_GE_UN: u8 = 0x57;

const CMP_F_EQ: u8 = 0x58;
const CMP_F_NE: u8 = 0x59;
const CMP_F_LT: u8 = 0x5A;
const CMP_F_LE: u8 = 0x5B;
const CMP_F_GT: u8 = 0x5C;
const CMP_F_GE: u8 = 0x5D;

const CNV_ITF: u8 = 0x5E;
const CNV_FTI: u8 = 0x5F;
const CNV_TRM: u8 = 0x60;

const LD_TAG: u8 = 0x61;
const LD_LEN: u8 = 0x62;
const LD_IDX: u8 = 0x63;
const ST_IDX: u8 = 0x64;
const FRE: u8 = 0x65;
const EFF_RES_C: u8 = 0x66;
const EFF_ABT: u8 = 0x67;
const TSK_AWT: u8 = 0x68;
const INV_DYN: u8 = 0x69;

const LD_LOC_W: u8 = 0x80;
const ST_LOC_W: u8 = 0x81;
const LD_CST_W: u8 = 0x82;
const ST_FLD_W: u8 = 0x83;
const MK_VAR_W: u8 = 0x84;
const CMP_TAG_W: u8 = 0x85;
const JMP: u8 = 0x86;
const JMP_T: u8 = 0x87;
const JMP_F: u8 = 0x88;

const INV: u8 = 0xC0;
const INV_EFF: u8 = 0xC1;
const INV_TAL: u8 = 0xC2;
const INV_TAL_EFF: u8 = 0xC3;
const LD_GLB: u8 = 0xC4;
const ST_GLB: u8 = 0xC5;
const MK_ARR: u8 = 0xC6;
const ALC_REF: u8 = 0xC7;
const ALC_MAN: u8 = 0xC8;
const ALC_ARN: u8 = 0xC9;
const EFF_DO: u8 = 0xCA;
const EFF_RES: u8 = 0xCB;
const TSK_SPN: u8 = 0xCC;
const TSK_CHS: u8 = 0xCD;
const TSK_CHR: u8 = 0xCE;
const TSK_CMK: u8 = 0xCF;
const JMP_W: u8 = 0xD0;
const JMP_T_W: u8 = 0xD1;
const JMP_F_W: u8 = 0xD2;

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
