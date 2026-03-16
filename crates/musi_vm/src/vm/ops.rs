//! Structural, type, closure, array, and globals opcode handlers.
//!
//! Each handler is called directly from the flat dispatch in `Vm::step_inner`.

use musi_bc::instr_len;

use crate::error::{VmError, malformed};
use crate::heap::Heap;
use crate::loader::{LoadedConst, LoadedFn, LoadedType};
use crate::value::Value;
use crate::vm::Frame;

/// Sentinel `type_id` for closure heap objects.
pub const CLOSURE_TYPE_ID: u32 = 0xFFFF_FFFE;

// Type pool tags (§11.3 of spec) — used for runtime type matching.
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

/// Operand decoding and jump-target helpers.
///
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

/// Reinterpret a u32 operand as a signed i32 jump offset.
pub fn read_i32_operand(operand: u32) -> Result<isize, VmError> {
    let signed = i32::from_le_bytes(operand.to_le_bytes());
    isize::try_from(signed).map_err(|_| malformed!("jump offset overflows isize"))
}

/// Compute absolute jump target from the address after the instruction + signed offset.
pub fn jump_target(after_instr: usize, offset: isize) -> Result<usize, VmError> {
    after_instr
        .checked_add_signed(offset)
        .ok_or_else(|| malformed!("jump target out of range"))
}

/// Extract a `usize` from either an int or nat value.
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
        LoadedConst::I32(n) => Value::from_int(i64::from(*n)),
        LoadedConst::I64(n) => Value::from_int(*n),
        LoadedConst::F64(f) => Value::from_float(*f),
        LoadedConst::Str(s) => {
            let ptr = heap.alloc_string(0, s.clone());
            Value::from_ref(ptr)
        }
        LoadedConst::Rune(c) => Value::from_rune(*c),
        LoadedConst::Fn(id) => Value::from_fn_id(*id),
    }
}

/// §5 — Load/store locals and constants.
pub fn exec_ld_cst(
    operand: u32,
    frame: &mut Frame,
    consts: &[LoadedConst],
    heap: &mut Heap,
) -> Result<(), VmError> {
    let idx = usize::try_from(operand).map_err(|_| malformed!("ld.cst index overflow"))?;
    let c = consts.get(idx).ok_or(VmError::OutOfBounds {
        index: idx,
        len: consts.len(),
    })?;
    frame.stack.push(const_to_value(c, heap));
    Ok(())
}

/// §5 — Struct and variant construction/access.
pub fn exec_mk_prd(operand: u32, frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let n = usize::try_from(operand).map_err(|_| malformed!("mk.prd field count overflow"))?;
    let mut fields: Vec<Value> = Vec::with_capacity(n);
    for _ in 0..n {
        fields.push(frame.pop()?);
    }
    fields.reverse();
    let ptr = heap.alloc(0, fields);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

pub fn exec_ld_fld(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let idx = usize::try_from(operand).map_err(|_| malformed!("ld.fld index overflow"))?;
    let product_val = frame.pop()?;
    let ptr = product_val.as_ref()?;
    let product = heap.get(ptr)?;
    let v = product
        .fields
        .get(idx)
        .copied()
        .ok_or(VmError::OutOfBounds {
            index: idx,
            len: product.fields.len(),
        })?;
    frame.stack.push(v);
    Ok(())
}

pub fn exec_mk_var(operand: u32, frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let tag = operand;
    let payload = frame.pop()?;
    let ptr = heap.alloc(0, vec![payload]);
    let ptr_usize =
        usize::try_from(ptr).map_err(|_| malformed!("variant heap pointer overflows usize"))?;
    let variant = heap.get_mut(ptr_usize)?;
    variant.tag = Some(tag);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

pub fn exec_ld_pay(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let field_idx =
        usize::try_from(operand).map_err(|_| malformed!("ld.pay field index overflow"))?;
    let variant_val = frame.pop()?;
    let ptr = variant_val.as_ref()?;
    let variant = heap.get(ptr)?;
    let payload = variant
        .fields
        .get(field_idx)
        .copied()
        .unwrap_or(Value::UNIT);
    frame.stack.push(payload);
    Ok(())
}

pub fn exec_cmp_tag(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let variant_val = frame.pop()?;
    let ptr = variant_val.as_ref()?;
    let variant = heap.get(ptr)?;
    frame
        .stack
        .push(Value::from_bool(variant.tag == Some(operand)));
    Ok(())
}

/// §9 — Array and heap operations.
pub fn exec_ld_tag(frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let variant_val = frame.pop()?;
    let ptr = variant_val.as_ref()?;
    let variant = heap.get(ptr)?;
    frame
        .stack
        .push(Value::from_int(i64::from(variant.tag.unwrap_or(0))));
    Ok(())
}

pub fn exec_ld_len(frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let arr_val = frame.pop()?;
    let ptr = arr_val.as_ref()?;
    let arr = heap.get(ptr)?;
    let len = u64::try_from(arr.elems.len()).unwrap_or(u64::MAX);
    frame.stack.push(Value::from_nat(len));
    Ok(())
}

pub fn exec_ld_idx(frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let idx_val = frame.pop()?;
    let arr_val = frame.pop()?;
    let idx = as_usize(idx_val)?;
    let ptr = arr_val.as_ref()?;
    let arr = heap.get(ptr)?;
    let v = arr.elems.get(idx).copied().ok_or(VmError::OutOfBounds {
        index: idx,
        len: arr.elems.len(),
    })?;
    frame.stack.push(v);
    Ok(())
}

pub fn exec_st_idx(frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let val = frame.pop()?;
    let idx_val = frame.pop()?;
    let arr_val = frame.pop()?;
    let idx = as_usize(idx_val)?;
    let ptr = arr_val.as_ref()?;
    let arr = heap.get_mut(ptr)?;
    let len = arr.elems.len();
    let elem = arr
        .elems
        .get_mut(idx)
        .ok_or(VmError::OutOfBounds { index: idx, len })?;
    *elem = val;
    Ok(())
}

pub fn exec_mk_arr(operand: u32, frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let len_val = frame.pop()?;
    let len = as_usize(len_val)?;
    let elems = vec![Value::UNIT; len];
    let ptr = heap.alloc_array(operand, elems);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

pub fn exec_st_fld(operand: u32, frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let field_idx = usize::try_from(operand).map_err(|_| malformed!("st.fld index overflow"))?;
    let val = frame.pop()?;
    let product_val = frame.pop()?;
    let ptr = product_val.as_ref()?;
    let product = heap.get_mut(ptr)?;
    let len = product.fields.len();
    let field = product
        .fields
        .get_mut(field_idx)
        .ok_or(VmError::OutOfBounds {
            index: field_idx,
            len,
        })?;
    *field = val;
    Ok(())
}

/// §12 — Global variable load/store.
pub fn exec_ld_glb(
    operand: u32,
    frame: &mut Frame,
    globals: &mut Vec<Value>,
) -> Result<(), VmError> {
    let idx = usize::try_from(operand).map_err(|_| malformed!("ld.glb index overflow"))?;
    if idx >= globals.len() {
        globals.resize(idx + 1, Value::UNIT);
    }
    frame.stack.push(globals[idx]);
    Ok(())
}

pub fn exec_st_glb(
    operand: u32,
    frame: &mut Frame,
    globals: &mut Vec<Value>,
) -> Result<(), VmError> {
    let idx = usize::try_from(operand).map_err(|_| malformed!("st.glb index overflow"))?;
    let val = frame.pop()?;
    if idx >= globals.len() {
        globals.resize(idx + 1, Value::UNIT);
    }
    globals[idx] = val;
    Ok(())
}

/// §16 — Runtime type checking.
pub fn exec_type_chk(
    type_id: u32,
    frame: &mut Frame,
    types: &[LoadedType],
    heap: &Heap,
) -> Result<(), VmError> {
    let val = frame.pop()?;

    let type_tag = types
        .get(usize::try_from(type_id).unwrap_or(usize::MAX))
        .map_or(TYPE_TAG_ANY, |t| t.tag);

    let matches = if type_tag == TYPE_TAG_ANY {
        true
    } else if val.is_unit() {
        type_tag == TYPE_TAG_UNIT
    } else if val.is_float() {
        type_tag == TYPE_TAG_F32 || type_tag == TYPE_TAG_F64
    } else {
        let vtag = val.tag();
        match vtag {
            0x7FF1 => matches!(
                type_tag,
                TYPE_TAG_I8 | TYPE_TAG_I16 | TYPE_TAG_I32 | TYPE_TAG_I64
            ),
            0x7FF2 => matches!(
                type_tag,
                TYPE_TAG_U8 | TYPE_TAG_U16 | TYPE_TAG_U32 | TYPE_TAG_U64
            ),
            0x7FF3 => type_tag == TYPE_TAG_BOOL,
            0x7FF4 => type_tag == TYPE_TAG_RUNE,
            0x7FF7 => type_tag == TYPE_TAG_FN,
            0x7FF5 => {
                if let Ok(ptr) = val.as_ref()
                    && let Ok(obj) = heap.get(ptr)
                {
                    obj.type_id == type_id
                } else {
                    false
                }
            }
            _ => false,
        }
    };

    frame.stack.push(Value::from_bool(matches));
    Ok(())
}

/// §17 — Closure operations.
pub fn exec_mk_clo(
    fn_id: u32,
    frame: &mut Frame,
    functions: &[LoadedFn],
    heap: &mut Heap,
) -> Result<(), VmError> {
    let func = functions
        .iter()
        .find(|f| f.fn_id == fn_id)
        .ok_or_else(|| malformed!("mk.clo: unknown fn_id {fn_id}", fn_id = fn_id))?;
    let n = usize::from(func.upvalue_count);
    if frame.stack.len() < n {
        return Err(malformed!(
            "mk.clo: need {} upvalues, stack has {}",
            n,
            frame.stack.len()
        ));
    }
    let start = frame.stack.len() - n;
    let upvalues: Vec<Value> = frame.stack.drain(start..).collect();

    let ptr = heap.alloc(CLOSURE_TYPE_ID, upvalues);
    let ptr_usize =
        usize::try_from(ptr).map_err(|_| malformed!("closure heap pointer overflows usize"))?;
    heap.get_mut(ptr_usize)?.tag = Some(fn_id);

    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

pub fn exec_ld_upv(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let idx = usize::try_from(operand).map_err(|_| malformed!("ld.upv index overflow"))?;
    let closure_val = frame
        .closure_ref
        .ok_or_else(|| malformed!("ld.upv: frame has no closure_ref"))?;
    let ptr = closure_val.as_ref()?;
    let closure = heap.get(ptr)?;
    let val = closure
        .fields
        .get(idx)
        .copied()
        .ok_or(VmError::OutOfBounds {
            index: idx,
            len: closure.fields.len(),
        })?;
    frame.stack.push(val);
    Ok(())
}

/// §11 — Dynamic dispatch resolution.
///
/// Result of resolving an `INV_DYN` callee.
pub enum DynCall {
    Fn(u32),
    Closure { fn_id: u32, closure_ref: Value },
}

pub fn resolve_inv_dyn(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<DynCall, VmError> {
    let arg_count =
        usize::from(u8::try_from(operand).map_err(|_| malformed!("inv.dyn operand overflow"))?);
    let mut args: Vec<Value> = (0..arg_count)
        .map(|_| frame.pop())
        .collect::<Result<Vec<_>, _>>()?;
    args.reverse();
    let callee = frame.pop()?;

    // Push args back for the caller to consume.
    for a in args {
        frame.stack.push(a);
    }

    if let Ok(fn_id) = callee.as_fn_id() {
        return Ok(DynCall::Fn(fn_id));
    }

    if let Ok(ptr) = callee.as_ref() {
        let callee_obj = heap.get(ptr)?;
        if callee_obj.type_id == CLOSURE_TYPE_ID {
            let fn_id = callee_obj
                .tag
                .ok_or_else(|| malformed!("closure object missing fn_id tag"))?;
            return Ok(DynCall::Closure {
                fn_id,
                closure_ref: callee,
            });
        }
    }

    Err(malformed!("inv.dyn: callee is neither fn nor closure"))
}
