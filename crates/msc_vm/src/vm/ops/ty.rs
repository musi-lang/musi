//! Type operation opcode handlers (§4.12).

use crate::error::VmError;
use crate::heap::Heap;
use crate::loader::LoadedType;
use crate::value::Value;
use crate::vm::Frame;

pub fn exec_ty_of(frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let val = frame.pop()?;
    let type_id: u32 = if val.is_float() {
        0x0C // TAG_TY_F64
    } else if val.is_int() {
        0x06 // TAG_TY_I64
    } else if val.is_nat() {
        0x0A // TAG_TY_U64
    } else if val.is_bool() {
        0x02 // TAG_TY_BOOL
    } else if val.is_unit() {
        0x01 // TAG_TY_UNIT
    } else if val.is_rune() {
        0x0D // TAG_TY_RUNE
    } else if val.is_fn() {
        0x12 // TAG_TY_FN
    } else if let Ok(ptr) = val.as_ref() {
        heap.get(ptr).map_or(0xFFFF_FFFF, |obj| obj.type_id())
    } else {
        0xFFFF_FFFF
    };
    frame.stack.push(Value::from_nat(u64::from(type_id)));
    Ok(())
}

pub fn exec_ty_desc(
    operand: u32,
    frame: &mut Frame,
    heap: &mut Heap,
    types: &[LoadedType],
) -> Result<(), VmError> {
    let type_tag = types
        .get(usize::try_from(operand).unwrap_or(usize::MAX))
        .map_or(0u8, |t| t.tag);
    let desc: Box<str> = format!("type_id:{operand} tag:{type_tag}").into_boxed_str();
    let ptr = heap.alloc_string(0, desc);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}
