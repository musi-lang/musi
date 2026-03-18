//! Instruction decode and structural opcode handlers.
//!
//! The SEAM ISA uses big-endian operands with no WID/EXT prefix opcodes.
//! Five fixed-width formats: `F0` (1B), `FI8` (2B), `FI16` (3B), `FI8x2` (3B), `FI24` (4B).

pub mod array;
pub mod closure;
pub mod record;
pub mod string;
pub mod ty;

pub use array::{exec_arr_get, exec_arr_len, exec_arr_new, exec_arr_set};
pub use closure::{exec_cls_new, exec_cls_upv, exec_ld_upv, exec_st_upv};
pub use record::{
    exec_ld_ind, exec_mat_data, exec_mat_tag, exec_opt_get, exec_opt_is, exec_opt_none,
    exec_opt_some, exec_rec_get, exec_rec_new, exec_rec_set, exec_st_ind, exec_tup_get,
    exec_tup_new,
};
pub use string::{exec_str_cat, exec_str_len};
pub use ty::{exec_ty_desc, exec_ty_of};

use msc_bc::{Opcode, instr_len};

use crate::error::{VmError, malformed};
use crate::heap::{Heap, HeapPayload};
use crate::loader::{LoadedConst, LoadedType};
use crate::value::Value;
use crate::vm::Frame;

// Type pool tags (§11.3 of spec) - used for runtime type matching.
const TYPE_TAG_UNIT: u8 = 0x01;
const TYPE_TAG_BOOL: u8 = 0x02;
const TYPE_TAG_I8: u8 = 0x03;
const TYPE_TAG_I16: u8 = 0x04;
const TYPE_TAG_I32: u8 = 0x05;
const TYPE_TAG_I64: u8 = 0x06;
const TYPE_TAG_U8: u8 = 0x07;
const TYPE_TAG_U16: u8 = 0x08;
const TYPE_TAG_U32: u8 = 0x09;
const TYPE_TAG_U64: u8 = 0x0A;
const TYPE_TAG_F32: u8 = 0x0B;
const TYPE_TAG_F64: u8 = 0x0C;
const TYPE_TAG_RUNE: u8 = 0x0D;
const TYPE_TAG_FN: u8 = 0x12;
const TYPE_TAG_ANY: u8 = 0x14;

/// Decoded instruction: opcode + packed u32 carrying all operand bytes.
///
/// For `FI8`:   operand = u8 in bits 7:0.
/// For `FI16`:  operand = big-endian u16 in bits 15:0.
/// For `FI8x2`: operand = (a << 8) | b in bits 15:0, where byte a is bits 15:8.
/// For `FI24`:  operand = sign-extended i24 as i32 reinterpreted in u32.
/// For `F0`:    operand = 0.
pub struct DecodedInstr {
    pub op: Opcode,
    pub operand: u32,
    pub total_len: usize,
}

/// Decode one instruction at `base_ip` using the SEAM big-endian format.
///
/// No WID/EXT prefix handling - the emitter never emits those.
#[must_use]
pub fn decode_instruction(code: &[u8], base_ip: usize) -> DecodedInstr {
    let raw = code[base_ip];
    let len = instr_len(raw);

    let operand = match len {
        1 => 0u32,
        2 => u32::from(code[base_ip + 1]),
        3 => {
            // FI16 or FI8x2: both are 2-byte big-endian.
            let hi = code[base_ip + 1];
            let lo = code[base_ip + 2];
            u32::from(u16::from_be_bytes([hi, lo]))
        }
        _ => {
            // FI24: signed 24-bit big-endian - sign-extend to i32 then reinterpret.
            let b0 = code[base_ip + 1];
            let b1 = code[base_ip + 2];
            let b2 = code[base_ip + 3];
            // Sign-extend 24-bit BE value: pack into top 3 bytes of i32, arithmetic-shift down.
            let raw24 = i32::from_be_bytes([b0, b1, b2, 0]) >> 8;
            raw24.cast_unsigned()
        }
    };

    DecodedInstr {
        op: Opcode(raw),
        operand,
        total_len: len,
    }
}

/// Extract the `FI16` operand as a signed i16 jump offset.
pub fn read_i16_operand(operand: u32) -> isize {
    // The u32 carries the u16 bits - reinterpret as i16 then widen.
    // try_from cannot fail: value is masked to 16 bits before conversion.
    let raw = u16::try_from(operand & 0xFFFF).unwrap_or(0);
    let signed = i16::from_ne_bytes(raw.to_ne_bytes());
    isize::from(signed)
}

/// Extract the `FI24` operand as a signed isize jump offset.
pub fn read_i24_operand(operand: u32) -> Result<isize, VmError> {
    // decode_instruction stored it as i32 cast to u32.
    let signed = operand.cast_signed();
    isize::try_from(signed).map_err(|_| malformed!("br.long offset overflows isize"))
}

/// Compute absolute jump target: address after current instruction + signed offset.
pub fn jump_target(after_instr: usize, offset: isize) -> Result<usize, VmError> {
    after_instr
        .checked_add_signed(offset)
        .ok_or_else(|| malformed!("jump target out of range"))
}

/// Extract the first byte (high byte) of an `FI8x2` operand.
#[must_use]
#[expect(clippy::as_conversions, reason = "value is masked to 0xFF before cast")]
pub const fn fi8x2_a(operand: u32) -> u8 {
    ((operand >> 8) & 0xFF) as u8
}

/// Extract the second byte (low byte) of an `FI8x2` operand.
#[must_use]
#[expect(clippy::as_conversions, reason = "value is masked to 0xFF before cast")]
pub const fn fi8x2_b(operand: u32) -> u8 {
    (operand & 0xFF) as u8
}

/// Convert a `usize`-range value from either int or nat.
pub fn as_usize(v: Value) -> Result<usize, VmError> {
    if let Ok(u) = v.as_nat() {
        usize::try_from(u).map_err(|_| VmError::OutOfBounds {
            index: usize::MAX,
            len: 0,
        })
    } else {
        let n = v.as_int()?;
        usize::try_from(n).map_err(|_| VmError::OutOfBounds {
            index: usize::MAX,
            len: 0,
        })
    }
}

pub fn const_to_value(c: &LoadedConst, heap: &mut Heap) -> Value {
    match c {
        LoadedConst::Int(n) => Value::from_int_wide(*n, heap),
        LoadedConst::Float(f) => Value::from_float(*f),
        LoadedConst::Str(s) => {
            let ptr = heap.alloc_string(0, s.clone());
            Value::from_ref(ptr)
        }
    }
}

// --------------------------------------------------------------------------
// §4.1 Data movement - LD_CONST
// --------------------------------------------------------------------------

pub fn exec_ld_const(
    operand: u32,
    frame: &mut Frame,
    consts: &[LoadedConst],
    heap: &mut Heap,
) -> Result<(), VmError> {
    let idx = usize::try_from(operand).map_err(|_| malformed!("ld.const index overflow"))?;
    let c = consts.get(idx).ok_or(VmError::OutOfBounds {
        index: idx,
        len: consts.len(),
    })?;
    frame.stack.push(const_to_value(c, heap));
    Ok(())
}

// --------------------------------------------------------------------------
// §4.12 Type operations
// --------------------------------------------------------------------------

pub fn exec_ty_test(
    type_id: u32,
    frame: &mut Frame,
    types: &[LoadedType],
    heap: &Heap,
) -> Result<(), VmError> {
    let val = frame.pop()?;

    let type_tag = types
        .get(usize::try_from(type_id).unwrap_or(usize::MAX))
        .map_or(TYPE_TAG_ANY, |t| t.tag);

    let matches = match () {
        () if type_tag == TYPE_TAG_ANY => true,
        () if val.is_float() => type_tag == TYPE_TAG_F32 || type_tag == TYPE_TAG_F64,
        () if val.is_unit() => type_tag == TYPE_TAG_UNIT,
        () if val.is_int() => matches!(
            type_tag,
            TYPE_TAG_I8 | TYPE_TAG_I16 | TYPE_TAG_I32 | TYPE_TAG_I64
        ),
        () if val.is_nat() => matches!(
            type_tag,
            TYPE_TAG_U8 | TYPE_TAG_U16 | TYPE_TAG_U32 | TYPE_TAG_U64
        ),
        () if val.is_bool() => type_tag == TYPE_TAG_BOOL,
        () if val.is_rune() => type_tag == TYPE_TAG_RUNE,
        () if val.is_fn() => type_tag == TYPE_TAG_FN,
        () => val
            .as_ref()
            .is_ok_and(|ptr| heap.get(ptr).is_ok_and(|obj| obj.type_id() == type_id)),
    };

    frame.stack.push(Value::from_bool(matches));
    Ok(())
}

// --------------------------------------------------------------------------
// §4.7 Call - resolve callee from stack
// --------------------------------------------------------------------------

/// Resolve a `CALL arity:u8` callee: pop the callee value, classify it.
pub enum CallTarget {
    Fn(u32),
    Closure { fn_id: u32, closure_ref: Value },
}

/// Pop the callee from the operand stack and resolve to a `CallTarget`.
///
/// The operand stack at the point of CALL holds (bottom→top):
///   arg0 … arg_{arity-1}  callee
/// We need to save the args, pop the callee, then push args back.
pub fn resolve_callee(arity: u8, frame: &mut Frame, heap: &Heap) -> Result<CallTarget, VmError> {
    let n = usize::from(arity);
    let stack_len = frame.stack.len();
    if stack_len < n + 1 {
        return Err(malformed!(
            "call: stack has {stack_len} values, need {need}",
            need = n + 1
        ));
    }
    // The callee sits immediately below the args.
    let callee_pos = stack_len - n - 1;
    let callee = frame.stack.remove(callee_pos);

    if let Ok(fn_id) = callee.as_fn_id() {
        return Ok(CallTarget::Fn(fn_id));
    }

    if let Ok(ptr) = callee.as_ref() {
        if let Ok(obj) = heap.get(ptr) {
            if let HeapPayload::Closure { fn_id, .. } = obj.payload {
                return Ok(CallTarget::Closure {
                    fn_id,
                    closure_ref: callee,
                });
            }
        }
    }

    Err(malformed!("call: callee is neither fn nor closure"))
}
