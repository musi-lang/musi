//! String opcode handlers.

use crate::error::{malformed, VmError};
use crate::heap::Heap;
use crate::value::Value;
use crate::vm::Frame;

pub fn exec_str_cat(frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let (b, a) = frame.pop2()?;
    let ptr_a = a.as_ref()?;
    let ptr_b = b.as_ref()?;
    let s_a = heap
        .get_string(ptr_a)
        .map_err(|_| malformed!("str.cat: left operand is not a string"))?;
    let s_b = heap
        .get_string(ptr_b)
        .map_err(|_| malformed!("str.cat: right operand is not a string"))?;
    let concatenated: Box<str> = format!("{s_a}{s_b}").into_boxed_str();
    let ptr = heap.alloc_string(0, concatenated);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

pub fn exec_str_len(frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let val = frame.pop()?;
    let ptr = val.as_ref()?;
    let s = heap
        .get_string(ptr)
        .map_err(|_| malformed!("str.len: value is not a string"))?;
    let len = u64::try_from(s.chars().count()).unwrap_or(u64::MAX);
    frame.stack.push(Value::from_nat(len));
    Ok(())
}
