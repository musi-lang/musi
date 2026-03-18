//! Closure and upvalue opcode handlers.

use std::collections::HashMap;

use crate::error::{malformed, VmError};
use crate::heap::{Heap, UpvalueCell};
use crate::loader::LoadedFn;
use crate::value::Value;
use crate::vm::Frame;

/// `CLS_NEW fn_id:u16` — allocate a closure shell (upvalues follow as CLS_UPV).
pub fn exec_cls_new(
    operand: u32,
    frame: &mut Frame,
    functions: &[LoadedFn],
    heap: &mut Heap,
) -> Result<(), VmError> {
    let fn_id = operand & 0xFFFF;
    let fn_idx = fn_id as usize;
    if fn_idx >= functions.len() {
        return Err(malformed!("cls.new: unknown fn_id {fn_id}", fn_id = fn_id));
    }

    let ptr = heap.alloc_closure(fn_id);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

/// `CLS_UPV kind:u8, idx:u8` — attach an upvalue cell ref to the closure on TOS.
///
/// `kind=0`: capture local slot `idx` from the current frame (open upvalue).
/// `kind=1`: re-capture upvalue `idx` from the current frame's parent closure.
///
/// The closure being built must be on TOS (placed there by `CLS_NEW`).
pub fn exec_cls_upv(
    operand: u32,
    call_stack: &mut [Frame],
    heap: &mut Heap,
    open_upvalue_map: &mut HashMap<(usize, usize), usize>,
) -> Result<(), VmError> {
    let kind = super::fi8x2_a(operand);
    let idx = super::fi8x2_b(operand);

    let frame_idx = call_stack.len() - 1;

    let upv_ptr: usize = match kind {
        0 => {
            let slot = usize::from(idx);
            let key = (frame_idx, slot);
            if let Some(&existing) = open_upvalue_map.get(&key) {
                existing
            } else {
                let ptr = heap.alloc_upvalue(UpvalueCell::Open {
                    frame_depth: frame_idx,
                    slot,
                });
                let ptr_usize = ptr as usize;
                let _ = open_upvalue_map.insert(key, ptr_usize);
                call_stack[frame_idx].open_upvalues.push(ptr_usize);
                ptr_usize
            }
        }
        1 => {
            let closure_val = call_stack[frame_idx]
                .closure_ref
                .ok_or_else(|| malformed!("cls.upv parent: no closure_ref"))?;
            let closure_ptr = closure_val.as_ref()?;
            let (_, upvalues) = heap.get_closure(closure_ptr)?;
            let parent_upv =
                upvalues
                    .get(usize::from(idx))
                    .copied()
                    .ok_or(VmError::OutOfBounds {
                        index: usize::from(idx),
                        len: upvalues.len(),
                    })?;
            parent_upv.as_ref()? as usize
        }
        _ => return Err(malformed!("cls.upv: invalid kind {kind}")),
    };

    let closure_val = call_stack[frame_idx]
        .stack
        .last()
        .copied()
        .ok_or_else(|| malformed!("cls.upv: no closure on stack"))?;
    let closure_heap_ptr = closure_val.as_ref()?;
    heap.get_closure_upvalues_mut(closure_heap_ptr)?
        .push(Value::from_ref(upv_ptr as u64));

    Ok(())
}

/// `LD_UPV idx:u8` — load the value of upvalue `idx` from the current closure.
///
/// Dereferences the upvalue cell: open cells read from the live frame locals,
/// closed cells return the captured value directly.
pub fn exec_ld_upv(operand: u32, call_stack: &[Frame], heap: &Heap) -> Result<Value, VmError> {
    let frame = call_stack
        .last()
        .ok_or_else(|| malformed!("ld.upv: empty call stack"))?;
    let idx = usize::try_from(operand).map_err(|_| malformed!("ld.upv index overflow"))?;
    let closure_val = frame
        .closure_ref
        .ok_or_else(|| malformed!("ld.upv: frame has no closure_ref"))?;
    let ptr = closure_val.as_ref()?;
    let (_, upvalues) = heap.get_closure(ptr)?;
    let upv_ref = upvalues.get(idx).copied().ok_or(VmError::OutOfBounds {
        index: idx,
        len: upvalues.len(),
    })?;
    let upv_ptr = upv_ref.as_ref()?;
    match heap.get_upvalue(upv_ptr)? {
        UpvalueCell::Open { frame_depth, slot } => Ok(call_stack[*frame_depth].locals[*slot]),
        UpvalueCell::Closed(v) => Ok(*v),
    }
}

/// `ST_UPV idx:u8` — write to upvalue `idx` of the current closure.
///
/// Open cells write directly to the live frame's locals. Closed cells update
/// the heap cell in place.
pub fn exec_st_upv(
    operand: u32,
    new_val: Value,
    call_stack: &mut [Frame],
    heap: &mut Heap,
) -> Result<(), VmError> {
    let frame_idx = call_stack.len() - 1;
    let idx = usize::try_from(operand).map_err(|_| malformed!("st.upv index overflow"))?;
    let closure_val = call_stack[frame_idx]
        .closure_ref
        .ok_or_else(|| malformed!("st.upv: frame has no closure_ref"))?;
    let closure_ptr = closure_val.as_ref()?;
    let (_, upvalues) = heap.get_closure(closure_ptr)?;
    let upv_ref = upvalues.get(idx).copied().ok_or(VmError::OutOfBounds {
        index: idx,
        len: upvalues.len(),
    })?;
    let upv_ptr = upv_ref.as_ref()?;
    match heap.get_upvalue(upv_ptr)? {
        UpvalueCell::Open { frame_depth, slot } => {
            let (fd, sl) = (*frame_depth, *slot);
            call_stack[fd].locals[sl] = new_val;
            Ok(())
        }
        UpvalueCell::Closed(_) => {
            *heap.get_upvalue_mut(upv_ptr)? = UpvalueCell::Closed(new_val);
            Ok(())
        }
    }
}
