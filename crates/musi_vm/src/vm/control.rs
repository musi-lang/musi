//! §11/§15 invocation, return, and jump dispatch.

use musi_bc::{Opcode, instr_len};

use crate::error::VmError;
use crate::heap::Heap;
use crate::value::Value;
use crate::vm::Frame;

/// What the control dispatcher wants the main loop to do next.
#[derive(Clone, Copy)]
pub enum ControlFlow {
    /// Execute the next instruction normally.
    Continue,
    /// Jump to `ip` (absolute offset in current function).
    Jump { ip: usize },
    /// Push a new call frame for the given function.
    Call { fn_id: u32 },
    /// Push a new call frame for a closure (heap-allocated fn + captured upvalues).
    CallClosure { fn_id: u32, closure_ref: Value },
    /// Tail-call: reuse current frame, reset `ip = 0`.
    TailCall { fn_id: u32 },
    /// Pop the current frame, return `value` to the caller.
    Return { value: Value },
    /// Halt execution gracefully.
    Halt,
}

/// Dispatch §11/§15 control-flow opcodes.
pub fn exec(op: Opcode, operand: u32, frame: &mut Frame, heap: &Heap) -> Result<ControlFlow, VmError> {
    let base = frame.ip;
    match op {
        Opcode::HLT => Ok(ControlFlow::Halt),
        Opcode::RET => {
            let value = pop(frame)?;
            Ok(ControlFlow::Return { value })
        }
        Opcode::RET_U => Ok(ControlFlow::Return { value: Value::UNIT }),
        Opcode::UNR => Err(VmError::Malformed {
            desc: "unr (unreachable) reached at runtime".into(),
        }),
        Opcode::BRK => Ok(ControlFlow::Continue), // breakpoint — no-op in MVP
        Opcode::JMP_W => exec_jmp_wide(base, operand),
        Opcode::JMP_T_W => exec_jmp_cond_wide(frame, base, operand, true),
        Opcode::JMP_F_W => exec_jmp_cond_wide(frame, base, operand, false),
        Opcode::INV | Opcode::INV_EFF => Ok(ControlFlow::Call { fn_id: operand }),
        Opcode::INV_TAL | Opcode::INV_TAL_EFF => Ok(ControlFlow::TailCall { fn_id: operand }),
        Opcode::INV_DYN => exec_inv_dyn(operand, frame, heap),
        // Concurrency opcodes (TSK_SPN, TSK_AWT, TSK_CMK, TSK_CHS, TSK_CHR)
        // are dispatched in Vm::step_inner() before reaching this function.
        _ => Err(VmError::Malformed {
            desc: "unrecognized opcode in control dispatcher".into(),
        }),
    }
}

fn exec_jmp_wide(base: usize, operand: u32) -> Result<ControlFlow, VmError> {
    let offset = read_i32_operand(operand);
    let target = jump_target(base, offset)?;
    Ok(ControlFlow::Jump { ip: target })
}

fn exec_jmp_cond_wide(
    frame: &mut Frame,
    base: usize,
    operand: u32,
    jump_when: bool,
) -> Result<ControlFlow, VmError> {
    let cond = pop(frame)?;
    let offset = read_i32_operand(operand);
    if cond.as_bool()? == jump_when {
        let target = jump_target(base, offset)?;
        Ok(ControlFlow::Jump { ip: target })
    } else {
        Ok(ControlFlow::Continue)
    }
}

fn exec_inv_dyn(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<ControlFlow, VmError> {
    let arg_count = usize::from(u8::try_from(operand).map_err(|_| VmError::Malformed {
        desc: "inv.dyn operand overflow".into(),
    })?);
    let mut args: Vec<Value> = (0..arg_count)
        .map(|_| {
            frame.stack.pop().ok_or_else(|| VmError::Malformed {
                desc: "inv.dyn: stack underflow reading args".into(),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;
    args.reverse();
    let callee = pop(frame)?;

    // Try plain fn value first.
    if let Ok(fn_id) = callee.as_fn_id() {
        for a in args {
            frame.stack.push(a);
        }
        return Ok(ControlFlow::Call { fn_id });
    }

    // Try closure (heap ref with sentinel type_id).
    if let Ok(ptr) = callee.as_ref() {
        let obj = heap.get(ptr)?;
        if obj.type_id == CLOSURE_TYPE_ID {
            let fn_id = obj.tag.ok_or_else(|| VmError::Malformed {
                desc: "closure object missing fn_id tag".into(),
            })?;
            for a in args {
                frame.stack.push(a);
            }
            return Ok(ControlFlow::CallClosure { fn_id, closure_ref: callee });
        }
    }

    Err(VmError::Malformed {
        desc: "inv.dyn: callee is neither fn nor closure".into(),
    })
}

/// Sentinel `type_id` for closure heap objects.
pub const CLOSURE_TYPE_ID: u32 = 0xFFFF_FFFE;

fn pop(frame: &mut Frame) -> Result<Value, VmError> {
    frame.stack.pop().ok_or_else(|| VmError::Malformed {
        desc: "operand stack underflow".into(),
    })
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
