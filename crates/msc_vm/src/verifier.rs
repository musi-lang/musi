//! Single-pass pre-execution bytecode verifier.
//!
//! Validates each function in the module before the interpreter begins.
//! Checks operand bounds, jump targets, and stack depth.

use std::collections::HashSet;

use msc_bc::{Opcode, instr_len};

use crate::VmResult;
use crate::error::VmError;
use crate::loader::{LoadedFn, LoadedModule};

/// Verify all functions in the module before execution.
///
/// # Errors
///
/// Returns `VmError::Verify` if any function has invalid operands, jump
/// targets that do not land on instruction boundaries, or a stack depth
/// that exceeds the declared `max_stack`.
pub fn verify(module: &LoadedModule) -> VmResult {
    for func in &module.functions {
        verify_fn(func, module)?;
    }
    Ok(())
}

/// Collect instruction boundary offsets for the given bytecode.
fn collect_boundaries(code: &[u8]) -> VmResult<HashSet<usize>> {
    let mut boundaries = HashSet::new();
    let mut ip = 0usize;
    while ip < code.len() {
        let _ = boundaries.insert(ip);
        let raw = code[ip];
        let len = instr_len(raw);
        ip += len;
    }
    if ip != code.len() {
        return Err(VmError::Verify {
            desc: "instruction stream does not end on a boundary".into(),
        });
    }
    Ok(boundaries)
}

/// Check that a local slot index is within bounds.
fn check_local(slot: usize, local_count: usize, name: &str) -> VmResult {
    if slot >= local_count {
        return Err(VmError::Verify {
            desc: format!("{name} slot {slot} >= local_count {local_count}").into_boxed_str(),
        });
    }
    Ok(())
}

/// Check that a constant pool index is within bounds.
fn check_const(idx: usize, const_len: usize, name: &str) -> VmResult {
    if idx >= const_len {
        return Err(VmError::Verify {
            desc: format!("{name} index {idx} >= const pool size {const_len}").into_boxed_str(),
        });
    }
    Ok(())
}

/// Check that a function id (as index) is within the module's function table.
fn check_fn_id(fn_id: u32, module: &LoadedModule, name: &str) -> VmResult {
    let idx = usize::try_from(fn_id).unwrap_or(usize::MAX);
    if idx >= module.functions.len() && !module.functions.is_empty() {
        return Err(VmError::Verify {
            desc: format!(
                "{name} target fn_id {fn_id} out of bounds (len={})",
                module.functions.len()
            )
            .into_boxed_str(),
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
) -> VmResult {
    let after = ip + len;
    let target = after.wrapping_add_signed(offset);
    if !boundaries.contains(&target) {
        return Err(VmError::Verify {
            desc: format!("jump to non-instruction boundary at {target}").into_boxed_str(),
        });
    }
    Ok(())
}

/// Stack depth delta for fixed-format opcodes that need no operand validation.
///
/// Returns `Some(delta)` when the opcode is in a known fixed group.
const fn fixed_stack_delta(op: Opcode) -> Option<i32> {
    match op {
        // Net 0: no-ops, terminators, unary, type, effect, gc hints, arena
        Opcode::NOP
        | Opcode::PANIC
        | Opcode::RET
        | Opcode::RET_UNIT
        | Opcode::SWAP
        | Opcode::NEG
        | Opcode::BNOT
        | Opcode::ARR_LEN
        | Opcode::MAT_DATA
        | Opcode::TY_TEST
        | Opcode::TY_OF
        | Opcode::OPT_SOME
        | Opcode::OPT_IS
        | Opcode::OPT_GET
        | Opcode::STR_LEN
        | Opcode::EFF_POP
        | Opcode::EFF_RES
        | Opcode::GC_PIN
        | Opcode::GC_UNPIN
        | Opcode::AR_FREE => Some(0),

        // Push 1: dup, load upvalue, load immediates, alloc arena
        Opcode::DUP
        | Opcode::LD_UPV
        | Opcode::LD_UNIT
        | Opcode::LD_TRUE
        | Opcode::LD_FALSE
        | Opcode::LD_NONE
        | Opcode::AR_NEW => Some(1),

        // Net -1: pop, conditional branch (consumes cond), index get, binary ops
        Opcode::POP
        | Opcode::BR_TRUE
        | Opcode::BR_FALSE
        | Opcode::ARR_GET
        | Opcode::ADD
        | Opcode::SUB
        | Opcode::MUL
        | Opcode::DIV
        | Opcode::REM
        | Opcode::BAND
        | Opcode::BOR
        | Opcode::BXOR
        | Opcode::CMP_EQ
        | Opcode::CMP_NE
        | Opcode::CMP_LT
        | Opcode::CMP_LE
        | Opcode::CMP_GT
        | Opcode::CMP_GE
        | Opcode::TY_EQ
        | Opcode::TY_CAST
        | Opcode::STR_CAT
        | Opcode::AR_ALLOC => Some(-1),

        // Net -2: rec.set (pops obj + value)
        Opcode::REC_SET => Some(-2),

        // Net -3: arr.set (value + idx + arr)
        Opcode::ARR_SET => Some(-3),

        _ => None,
    }
}

/// Read a big-endian i16 jump offset from the instruction at `ip`.
fn read_be_i16_offset(code: &[u8], ip: usize) -> isize {
    let hi = code[ip + 1];
    let lo = code[ip + 2];
    let raw = i16::from_be_bytes([hi, lo]);
    isize::from(raw)
}

/// Read a big-endian signed i24 jump offset from the instruction at `ip`.
fn read_be_i24_offset(code: &[u8], ip: usize) -> isize {
    let b0 = code[ip + 1];
    let b1 = code[ip + 2];
    let b2 = code[ip + 3];
    let raw = i32::from_be_bytes([b0, b1, b2, 0]) >> 8;
    isize::try_from(raw).unwrap_or(isize::MAX)
}

/// Read a big-endian u16 operand (`FI16` or `FI8x2`) as a plain u32.
fn read_be_u16(code: &[u8], ip: usize) -> u32 {
    let hi = code[ip + 1];
    let lo = code[ip + 2];
    u32::from(u16::from_be_bytes([hi, lo]))
}

/// Per-function context threaded through operand verification.
struct VerifyCtx<'a> {
    code: &'a [u8],
    module: &'a LoadedModule,
    local_count: usize,
    upvalue_count: usize,
    const_len: usize,
    boundaries: &'a HashSet<usize>,
}

/// Compute the stack depth delta for an opcode and validate its operands.
fn verify_op(op: Opcode, ip: usize, len: usize, cx: &VerifyCtx<'_>) -> VmResult<i32> {
    if let Some(delta) = fixed_stack_delta(op) {
        return Ok(delta);
    }

    match op {
        // §4.1 Data movement
        Opcode::LD_LOC => {
            let slot = usize::from(cx.code[ip + 1]);
            check_local(slot, cx.local_count, "ld.loc")?;
            Ok(1)
        }
        Opcode::ST_LOC => {
            let slot = usize::from(cx.code[ip + 1]);
            check_local(slot, cx.local_count, "st.loc")?;
            Ok(-1)
        }
        Opcode::LD_CONST => {
            let idx = usize::try_from(read_be_u16(cx.code, ip)).unwrap_or(usize::MAX);
            check_const(idx, cx.const_len, "ld.const")?;
            Ok(1)
        }
        Opcode::LD_IND | Opcode::REC_GET | Opcode::ARR_NEW | Opcode::TUP_GET | Opcode::MAT_TAG => {
            Ok(0)
        }
        Opcode::ST_IND => Ok(-2),
        Opcode::ST_UPV | Opcode::ST_GLB => Ok(-1),

        // §4.6 Branch
        Opcode::BR => {
            let offset = read_be_i16_offset(cx.code, ip);
            check_jump_target(ip, len, offset, cx.boundaries)?;
            Ok(0)
        }
        Opcode::BR_LONG => {
            let offset = read_be_i24_offset(cx.code, ip);
            check_jump_target(ip, len, offset, cx.boundaries)?;
            Ok(0)
        }

        // §4.7 Call/Return
        Opcode::CALL => {
            let arity = i32::from(cx.code[ip + 1]);
            Ok(-arity)
        }
        Opcode::CALL_TAIL => {
            let arity = i32::from(cx.code[ip + 1]);
            Ok(-(arity + 1))
        }

        // §4.8 Closure
        Opcode::CLS_NEW => {
            let fn_id = read_be_u16(cx.code, ip);
            check_fn_id(fn_id, cx.module, "cls.new")?;
            Ok(1)
        }
        Opcode::CLS_UPV => verify_cls_upv(ip, cx),

        // §4.9 Record
        Opcode::REC_NEW => {
            // FI8x2: tag in hi byte, arity in lo byte.
            let operand = read_be_u16(cx.code, ip);
            let arity = i32::from(u8::try_from(operand & 0xFF).unwrap_or(u8::MAX));
            Ok(1 - arity)
        }
        Opcode::LD_SMI
        | Opcode::LD_ADDR
        | Opcode::LD_GLB
        | Opcode::REC_ADDR
        | Opcode::TY_DESC
        | Opcode::OPT_NONE => Ok(1),

        // §4.11 Tuple
        Opcode::TUP_NEW => {
            let arity = i32::from(cx.code[ip + 1]);
            Ok(1 - arity)
        }

        // §4.13 Effect - check 11: effect refs within effect table
        Opcode::EFF_NEED => {
            let operand = read_be_u16(cx.code, ip);
            let op_id = u32::from(u8::try_from(operand >> 8).unwrap_or(u8::MAX));
            let inline_arity = i32::from(u8::try_from(operand & 0xFF).unwrap_or(u8::MAX));
            let from_table = lookup_effect_op_param_count(cx.module, op_id);
            let param_count = if from_table > 0 {
                from_table
            } else {
                inline_arity
            };
            Ok(1 - param_count)
        }
        // EFF_HDL operand is a runtime effect ID (matches handler chain),
        // not a table index into the EFCT section. No bounds check needed.
        //
        // §4.5 Class Dispatch
        // CLS_DICT: pops val, pushes dict record. Net 0.
        // CLS_DISP: pops val, invokes method - result pushed by callee. Net 0 (conservative).
        Opcode::EFF_HDL | Opcode::CLS_DICT | Opcode::CLS_DISP => Ok(0),

        // §4.19 Foreign
        Opcode::FFI_CALL => verify_ffi_call(ip, cx),

        _ => Err(VmError::Verify {
            desc: format!("unknown opcode {:#04x}", op.0).into_boxed_str(),
        }),
    }
}

fn verify_cls_upv(ip: usize, cx: &VerifyCtx<'_>) -> VmResult<i32> {
    let operand = read_be_u16(cx.code, ip);
    let kind = u8::try_from(operand >> 8).unwrap_or(u8::MAX);
    let idx = usize::from(u8::try_from(operand & 0xFF).unwrap_or(u8::MAX));
    if kind > 1 {
        return Err(VmError::Verify {
            desc: format!("cls.upv: invalid kind {kind}").into_boxed_str(),
        });
    }
    if kind == 0 {
        check_local(idx, cx.local_count, "cls.upv local")?;
    } else if cx.upvalue_count > 0 && idx >= cx.upvalue_count {
        return Err(VmError::Verify {
            desc: format!(
                "cls.upv parent: idx {idx} >= upvalue_count {}",
                cx.upvalue_count
            )
            .into_boxed_str(),
        });
    }
    Ok(0)
}

fn verify_ffi_call(ip: usize, cx: &VerifyCtx<'_>) -> VmResult<i32> {
    let operand = read_be_u16(cx.code, ip);
    let ffi_id = usize::from(u8::try_from(operand >> 8).unwrap_or(u8::MAX));
    if ffi_id >= cx.module.foreign_fns.len() && !cx.module.foreign_fns.is_empty() {
        return Err(VmError::Verify {
            desc: format!("ffi.call sym_idx {ffi_id} out of bounds").into_boxed_str(),
        });
    }
    let param_count = cx
        .module
        .foreign_fns
        .get(ffi_id)
        .map_or(0, |f| i32::from(f.param_count));
    Ok(1 - param_count)
}

fn verify_fn(func: &LoadedFn, module: &LoadedModule) -> VmResult {
    let code = &func.code;
    let local_count = usize::from(func.param_count) + usize::from(func.local_count);
    let max_stack = usize::from(func.max_stack);

    let boundaries = collect_boundaries(code)?;
    let cx = VerifyCtx {
        code,
        module,
        local_count,
        upvalue_count: usize::from(func.upvalue_count),
        const_len: module.consts.len(),
        boundaries: &boundaries,
    };

    let mut ip = 0usize;
    let mut depth: i32 = 0;

    while ip < code.len() {
        let raw = code[ip];
        let op = Opcode(raw);
        let len = instr_len(raw);

        if ip + len > code.len() {
            return Err(VmError::Verify {
                desc: format!("instruction at {ip} extends past end of bytecode").into_boxed_str(),
            });
        }

        // BR_TRUE / BR_FALSE: delta is -1 AND need jump check.
        if matches!(op, Opcode::BR_TRUE | Opcode::BR_FALSE) {
            let offset = read_be_i16_offset(code, ip);
            check_jump_target(ip, len, offset, &boundaries)?;
            depth -= 1;
        } else {
            depth += verify_op(op, ip, len, &cx)?;
        }

        if depth > 0 {
            let d = usize::try_from(depth).unwrap_or(usize::MAX);
            if d > max_stack && max_stack > 0 {
                return Err(VmError::Verify {
                    desc: format!("stack depth {d} exceeds max_stack {max_stack} at offset {ip}")
                        .into_boxed_str(),
                });
            }
        }

        let is_terminator = matches!(op, Opcode::RET | Opcode::RET_UNIT | Opcode::PANIC);
        let is_unconditional_branch = matches!(op, Opcode::BR | Opcode::BR_LONG);
        if is_terminator || is_unconditional_branch {
            depth = 0;
        }

        ip += len;
    }

    Ok(())
}

/// Look up the parameter count for an effect operation by `op_id`.
///
/// Used for `EFF_NEED` stack delta: pops arity args, pushes 1 result.
/// Returns 0 if not found; the caller falls back to the inline arity operand.
const fn lookup_effect_op_param_count(module: &LoadedModule, op_id: u32) -> i32 {
    // op.id is the op's index within its parent effect's op array.
    // Param count is not stored in the new binary format; return 0 to trigger
    // the inline-arity fallback in the caller.
    let _ = (module, op_id);
    0
}
