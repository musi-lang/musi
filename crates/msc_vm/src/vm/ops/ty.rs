//! Type operation opcode handlers (§4.12).

use msc_bc::type_tag;

use crate::VmResult;
use crate::error::{VmError, malformed};
use crate::heap::{Heap, HeapObject, HeapPayload};
use crate::loader::LoadedType;
use crate::value::Value;
use crate::vm::Frame;

// Type ID constants
const TAG_TY_UNIT: u16 = 0x01;
const TAG_TY_BOOL: u16 = 0x02;
const TAG_TY_I64: u16 = 0x06;
const TAG_TY_U64: u16 = 0x0A;
const TAG_TY_F64: u16 = 0x0C;
const TAG_TY_RUNE: u16 = 0x0D;
const TAG_TY_FN: u16 = 0x12;
const TAG_TY_UNKNOWN: u16 = 0xFFFF;
const TAG_TY_UNKNOWN_32: u32 = 0xFFFF_FFFF;

/// Return the well-known type id for a value without consuming it.
///
/// Mirrors the tag constants used by `exec_ty_of`. For heap references the
/// `type_id` field stored in the object header is returned; for non-heap
/// values the corresponding primitive type id constant is used.
/// Returns `0xFFFF` for unrecognised tags (unknown / Any).
#[must_use]
pub fn value_type_id(val: Value, heap: &Heap) -> u16 {
    match () {
        () if val.is_float() => TAG_TY_F64,
        () if val.is_int() => TAG_TY_I64,
        () if val.is_nat() => TAG_TY_U64,
        () if val.is_bool() => TAG_TY_BOOL,
        () if val.is_unit() => TAG_TY_UNIT,
        () if val.is_rune() => TAG_TY_RUNE,
        () if val.is_fn() => TAG_TY_FN,
        () => val
            .as_ref()
            .ok()
            .and_then(|ptr| heap.get(ptr).ok())
            .map_or(TAG_TY_UNKNOWN, |o| {
                u16::try_from(o.type_id()).unwrap_or(TAG_TY_UNKNOWN)
            }),
    }
}

pub fn exec_ty_of(frame: &mut Frame, heap: &Heap) -> VmResult {
    let val = frame.pop()?;
    let type_id: u32 = match () {
        () if val.is_float() => u32::from(TAG_TY_F64),
        () if val.is_int() => u32::from(TAG_TY_I64),
        () if val.is_nat() => u32::from(TAG_TY_U64),
        () if val.is_bool() => u32::from(TAG_TY_BOOL),
        () if val.is_unit() => u32::from(TAG_TY_UNIT),
        () if val.is_rune() => u32::from(TAG_TY_RUNE),
        () if val.is_fn() => u32::from(TAG_TY_FN),
        () => val
            .as_ref()
            .ok()
            .and_then(|ptr| heap.get(ptr).ok())
            .map_or(TAG_TY_UNKNOWN_32, HeapObject::type_id),
    };
    frame.stack.push(Value::from_nat(u64::from(type_id)));
    Ok(())
}

pub fn exec_ty_desc(operand: u32, frame: &mut Frame, heap: &mut Heap, types: &[LoadedType]) {
    let type_tag = types
        .get(usize::try_from(operand).unwrap_or(usize::MAX))
        .map_or(0u8, |t| t.tag);
    let desc: Box<str> = format!("type_id:{operand} tag:{type_tag}").into_boxed_str();
    let ptr = heap.alloc_string(operand, desc);
    frame.stack.push(Value::from_ref(ptr));
}

/// Stack effect: `type_desc val → val` (net -1). Halts on type mismatch.
pub fn exec_ty_cast(frame: &mut Frame, types: &[LoadedType], heap: &Heap) -> VmResult {
    let desc_val = frame.pop()?;
    let ptr = desc_val
        .as_ref()
        .map_err(|_| malformed!("ty.cast: type descriptor is not a heap reference"))?;
    let type_id = match &heap.get(ptr)?.payload {
        HeapPayload::Str { type_id, .. } => *type_id,
        _ => return Err(malformed!("ty.cast: type descriptor is not a Str object")),
    };

    let val = frame.pop()?;

    let type_tag_val = types
        .get(usize::try_from(type_id).unwrap_or(usize::MAX))
        .map_or(type_tag::TAG_ANY, |t| t.tag);

    let matches = match () {
        () if type_tag_val == type_tag::TAG_ANY => true,
        () if val.is_float() => {
            type_tag_val == type_tag::TAG_F32 || type_tag_val == type_tag::TAG_F64
        }
        () if val.is_unit() => type_tag_val == type_tag::TAG_UNIT,
        () if val.is_int() => matches!(
            type_tag_val,
            type_tag::TAG_I8 | type_tag::TAG_I16 | type_tag::TAG_I32 | type_tag::TAG_I64
        ),
        () if val.is_nat() => matches!(
            type_tag_val,
            type_tag::TAG_U8 | type_tag::TAG_U16 | type_tag::TAG_U32 | type_tag::TAG_U64
        ),
        () if val.is_bool() => type_tag_val == type_tag::TAG_BOOL,
        () if val.is_rune() => type_tag_val == type_tag::TAG_RUNE,
        () if val.is_fn() => type_tag_val == type_tag::TAG_FN,
        () => val
            .as_ref()
            .is_ok_and(|p| heap.get(p).is_ok_and(|obj| obj.type_id() == type_id)),
    };

    if matches {
        frame.stack.push(val);
        Ok(())
    } else {
        Err(VmError::Halted)
    }
}
