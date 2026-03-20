//! Array opcode handlers.

use crate::VmResult;
use crate::error::VmError;
use crate::heap::Heap;
use crate::value::Value;
use crate::vm::Frame;

pub fn exec_arr_len(frame: &mut Frame, heap: &Heap) -> VmResult {
    let arr_val = frame.pop()?;
    let ptr = arr_val.as_ref()?;
    let elems = heap.get_array(ptr)?;
    let n = i64::try_from(elems.len()).unwrap_or(i64::MAX);
    frame.stack.push(Value::from_int(n));
    Ok(())
}

pub fn exec_arr_get(frame: &mut Frame, heap: &Heap) -> VmResult {
    let idx_val = frame.pop()?;
    let arr_val = frame.pop()?;
    let idx = super::as_usize(idx_val)?;
    let ptr = arr_val.as_ref()?;
    // String indexing: return the character at position `idx` as a rune.
    if let Ok(s) = heap.get_string(ptr) {
        let ch = s.chars().nth(idx).ok_or_else(|| VmError::OutOfBounds {
            index: idx,
            len: s.chars().count(),
        })?;
        frame.stack.push(Value::from_int(i64::from(u32::from(ch))));
        return Ok(());
    }
    let elems = heap.get_array(ptr)?;
    let v = elems.get(idx).copied().ok_or(VmError::OutOfBounds {
        index: idx,
        len: elems.len(),
    })?;
    frame.stack.push(v);
    Ok(())
}

pub fn exec_arr_set(frame: &mut Frame, heap: &mut Heap) -> VmResult {
    let val = frame.pop()?;
    let idx_val = frame.pop()?;
    let arr_val = frame.pop()?;
    let idx = super::as_usize(idx_val)?;
    let ptr = arr_val.as_ref()?;
    let elems = heap.get_array_mut(ptr)?;
    let len = elems.len();
    let elem = elems
        .get_mut(idx)
        .ok_or(VmError::OutOfBounds { index: idx, len })?;
    *elem = val;
    heap.write_barrier(ptr, val);
    Ok(())
}

pub fn exec_arr_new(operand: u32, frame: &mut Frame, heap: &mut Heap) -> VmResult {
    let len_val = frame.pop()?;
    let len = super::as_usize(len_val)?;
    let elems = vec![Value::UNIT; len];
    let ptr = heap.alloc_array(operand, elems);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}
