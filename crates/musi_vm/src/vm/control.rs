//! §11/§15 invocation, return, and jump dispatch.

use crate::error::VmError;
use crate::value::Value;
use crate::verifier::instr_len;
use crate::vm::Frame;

// Opcode constants.
const HLT: u8 = 0x01;
const RET: u8 = 0x02;
const RET_U: u8 = 0x03;
const UNR: u8 = 0x04;
const BRK: u8 = 0x05;
const INV_DYN: u8 = 0x69;
const JMP: u8 = 0x86;
const JMP_T: u8 = 0x87;
const JMP_F: u8 = 0x88;
const INV: u8 = 0xC0;
const INV_EFF: u8 = 0xC1;
const INV_TAL: u8 = 0xC2;
const INV_TAL_EFF: u8 = 0xC3;
const JMP_W: u8 = 0xD0;
const JMP_T_W: u8 = 0xD1;
const JMP_F_W: u8 = 0xD2;
const TSK_SPN: u8 = 0xCC;
const TSK_CHS: u8 = 0xCD;
const TSK_CHR: u8 = 0xCE;
const TSK_CMK: u8 = 0xCF;
const TSK_AWT: u8 = 0x68;

/// What the control dispatcher wants the main loop to do next.
#[derive(Clone, Copy)]
pub enum ControlFlow {
    /// Execute the next instruction normally.
    Continue,
    /// Jump to `ip` (absolute offset in current function).
    Jump { ip: usize },
    /// Push a new call frame for the given function.
    Call { fn_id: u32 },
    /// Tail-call: reuse current frame, reset `ip = 0`.
    TailCall { fn_id: u32 },
    /// Pop the current frame, return `value` to the caller.
    Return { value: Value },
    /// Halt execution gracefully.
    Halt,
}

/// Dispatch §11/§15 control-flow opcodes.
pub fn exec(op: u8, operand: u32, frame: &mut Frame) -> Result<ControlFlow, VmError> {
    let instr_end = frame.ip; // ip already points past the instruction
    match op {
        HLT => Ok(ControlFlow::Halt),
        RET => {
            let value = pop(frame)?;
            Ok(ControlFlow::Return { value })
        }
        RET_U => Ok(ControlFlow::Return { value: Value::UNIT }),
        UNR => Err(VmError::Malformed {
            desc: "unr (unreachable) reached at runtime".into(),
        }),
        BRK => Ok(ControlFlow::Continue), // breakpoint — no-op in MVP
        JMP => exec_jmp(instr_end, operand),
        JMP_T => exec_jmp_cond(frame, instr_end, operand, true),
        JMP_F => exec_jmp_cond(frame, instr_end, operand, false),
        JMP_W => exec_jmp_wide(instr_end, operand),
        JMP_T_W => exec_jmp_cond_wide(frame, instr_end, operand, true),
        JMP_F_W => exec_jmp_cond_wide(frame, instr_end, operand, false),
        INV | INV_EFF => Ok(ControlFlow::Call { fn_id: operand }),
        INV_TAL | INV_TAL_EFF => Ok(ControlFlow::TailCall { fn_id: operand }),
        INV_DYN => exec_inv_dyn(operand, frame),
        TSK_SPN | TSK_CHS | TSK_CHR | TSK_CMK | TSK_AWT => Err(VmError::Unimplemented {
            desc: "concurrency opcodes not yet implemented",
        }),
        _ => Err(VmError::Malformed {
            desc: "unrecognized opcode in control dispatcher".into(),
        }),
    }
}

// ── Jump helpers ─────────────────────────────────────────────────────────────

fn exec_jmp(instr_end: usize, operand: u32) -> Result<ControlFlow, VmError> {
    let offset = read_i16_operand(operand);
    let target = jump_target(instr_end, offset)?;
    Ok(ControlFlow::Jump { ip: target })
}

fn exec_jmp_cond(
    frame: &mut Frame,
    instr_end: usize,
    operand: u32,
    jump_when: bool,
) -> Result<ControlFlow, VmError> {
    let cond = pop(frame)?;
    let offset = read_i16_operand(operand);
    if cond.as_bool()? == jump_when {
        let target = jump_target(instr_end, offset)?;
        Ok(ControlFlow::Jump { ip: target })
    } else {
        Ok(ControlFlow::Continue)
    }
}

fn exec_jmp_wide(instr_end: usize, operand: u32) -> Result<ControlFlow, VmError> {
    let offset = read_i32_operand(operand);
    let target = jump_target(instr_end, offset)?;
    Ok(ControlFlow::Jump { ip: target })
}

fn exec_jmp_cond_wide(
    frame: &mut Frame,
    instr_end: usize,
    operand: u32,
    jump_when: bool,
) -> Result<ControlFlow, VmError> {
    let cond = pop(frame)?;
    let offset = read_i32_operand(operand);
    if cond.as_bool()? == jump_when {
        let target = jump_target(instr_end, offset)?;
        Ok(ControlFlow::Jump { ip: target })
    } else {
        Ok(ControlFlow::Continue)
    }
}

// ── Invocation helpers ───────────────────────────────────────────────────────

fn exec_inv_dyn(operand: u32, frame: &mut Frame) -> Result<ControlFlow, VmError> {
    let arg_count = usize::from(u8::try_from(operand).unwrap_or(u8::MAX));
    let mut args: Vec<Value> = (0..arg_count)
        .map(|_| {
            frame.stack.pop().ok_or_else(|| VmError::Malformed {
                desc: "inv.dyn: stack underflow reading args".into(),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    args.reverse();
    let callee = pop(frame)?;
    let fn_id = callee.as_fn_id()?;
    for a in args {
        frame.stack.push(a);
    }
    Ok(ControlFlow::Call { fn_id })
}

// ── Helpers ─────────────────────────────────────────────────────────────────

fn pop(frame: &mut Frame) -> Result<Value, VmError> {
    frame.stack.pop().ok_or_else(|| VmError::Malformed {
        desc: "operand stack underflow".into(),
    })
}

/// The `operand` field for u8/u16 instructions is widened to u32.
/// For `jmp` (u16 i16 operand): reinterpret low 16 bits as i16.
fn read_i16_operand(operand: u32) -> isize {
    let raw = u16::try_from(operand & 0xFFFF).unwrap_or(0);
    let signed = i16::from_le_bytes(raw.to_le_bytes());
    isize::from(signed)
}

/// For wide jumps (u32 i32 operand): reinterpret as i32.
const fn read_i32_operand(operand: u32) -> isize {
    let signed = i32::from_le_bytes(operand.to_le_bytes());
    // i32 → isize is lossless on 32+ bit platforms.
    #[allow(clippy::as_conversions)]
    let result = signed as isize;
    result
}

fn jump_target(after_instr: usize, offset: isize) -> Result<usize, VmError> {
    after_instr
        .checked_add_signed(offset)
        .ok_or_else(|| VmError::Malformed {
            desc: "jump target out of range".into(),
        })
}

/// Decode the operand from raw bytecode starting at `base_ip` (the opcode byte).
///
/// Returns `(operand_value, bytes_consumed_including_opcode)`.
#[must_use]
pub fn decode_operand(code: &[u8], base_ip: usize) -> (u32, usize) {
    let op = code[base_ip];
    let len = instr_len(op);
    let operand = match len {
        1 => 0u32,
        2 => u32::from(code[base_ip + 1]),
        3 => {
            let lo = code[base_ip + 1];
            let hi = code[base_ip + 2];
            u32::from(u16::from_le_bytes([lo, hi]))
        }
        _ => {
            let b = [
                code[base_ip + 1],
                code[base_ip + 2],
                code[base_ip + 3],
                code[base_ip + 4],
            ];
            u32::from_le_bytes(b)
        }
    };
    (operand, len)
}
