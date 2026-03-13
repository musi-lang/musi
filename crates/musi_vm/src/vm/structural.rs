//! §5/§9 locals, constants, struct/array, stack manipulation, and globals dispatch.

use musi_bc::Opcode;

use crate::error::VmError;
use crate::heap::Heap;
use crate::loader::LoadedConst;
use crate::value::Value;
use crate::vm::Frame;

/// Dispatch §0/§5/§9/§12 structural opcodes.
///
/// Returns `true` if the opcode was handled, `false` if not in this group.
pub fn exec(
    op: Opcode,
    operand: u32,
    frame: &mut Frame,
    consts: &[LoadedConst],
    heap: &mut Heap,
    globals: &mut Vec<Value>,
) -> Result<bool, VmError> {
    if let Some(result) = exec_stack(op, frame)? {
        return Ok(result);
    }
    if let Some(result) = exec_locals_consts(op, operand, frame, consts, heap)? {
        return Ok(result);
    }
    if let Some(result) = exec_struct(op, operand, frame, heap)? {
        return Ok(result);
    }
    exec_array_alloc_globals(op, operand, frame, heap, globals)
}

/// §0 Control/Stack: NOP, HLT, DUP, POP, SWP.
///
/// Returns `Some(true)` if handled, `None` if the opcode is not in this group.
fn exec_stack(op: Opcode, frame: &mut Frame) -> Result<Option<bool>, VmError> {
    match op {
        Opcode::NOP => {}
        Opcode::DUP => {
            let top = frame
                .stack
                .last()
                .copied()
                .ok_or_else(|| VmError::Malformed {
                    desc: "dup on empty stack".into(),
                })?;
            frame.stack.push(top);
        }
        Opcode::POP => {
            let _ = frame.stack.pop();
        }
        Opcode::SWP => {
            let len = frame.stack.len();
            if len < 2 {
                return Err(VmError::Malformed {
                    desc: "swp requires at least 2 stack values".into(),
                });
            }
            frame.stack.swap(len - 1, len - 2);
        }
        _ => return Ok(None),
    }
    Ok(Some(true))
}

/// §5 Locals and constants.
///
/// Returns `Some(true)` if handled, `None` if the opcode is not in this group.
fn exec_locals_consts(
    op: Opcode,
    operand: u32,
    frame: &mut Frame,
    consts: &[LoadedConst],
    heap: &mut Heap,
) -> Result<Option<bool>, VmError> {
    match op {
        Opcode::LD_LOC => {
            let slot = usize::from(u8::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "ld.loc operand overflow".into(),
            })?);
            let v = get_local(frame, slot)?;
            frame.stack.push(v);
        }
        Opcode::ST_LOC => {
            let slot = usize::from(u8::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "st.loc operand overflow".into(),
            })?);
            let v = pop(frame)?;
            set_local(frame, slot, v)?;
        }
        Opcode::LD_LOC_W => {
            let slot = usize::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "ld.loc.w operand overflow".into(),
            })?;
            let v = get_local(frame, slot)?;
            frame.stack.push(v);
        }
        Opcode::ST_LOC_W => {
            let slot = usize::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "st.loc.w operand overflow".into(),
            })?;
            let v = pop(frame)?;
            set_local(frame, slot, v)?;
        }
        Opcode::LD_CST | Opcode::LD_CST_W => {
            let idx = usize::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "ld.cst index overflow".into(),
            })?;
            let c = consts.get(idx).ok_or(VmError::OutOfBounds {
                index: idx,
                len: consts.len(),
            })?;
            frame.stack.push(const_to_value(c, heap));
        }
        _ => return Ok(None),
    }
    Ok(Some(true))
}

/// §5 Struct/variant: `MK_PRD`, `LD_FLD`, `MK_VAR`/W, `LD_PAY`, `CMP_TAG`/W.
///
/// Returns `Some(true)` if handled, `None` if the opcode is not in this group.
fn exec_struct(
    op: Opcode,
    operand: u32,
    frame: &mut Frame,
    heap: &mut Heap,
) -> Result<Option<bool>, VmError> {
    match op {
        Opcode::MK_PRD => {
            let n = usize::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "mk.prd field count overflow".into(),
            })?;
            let mut fields: Vec<Value> = Vec::with_capacity(n);
            for _ in 0..n {
                fields.push(pop(frame)?);
            }
            fields.reverse();
            let ptr = heap.alloc(0, fields);
            frame.stack.push(Value::from_ref(ptr));
        }
        Opcode::LD_FLD => {
            let idx = usize::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "ld.fld index overflow".into(),
            })?;
            let obj_val = pop(frame)?;
            let ptr = obj_val.as_ref()?;
            let obj = heap.get(ptr)?;
            let v = obj.fields.get(idx).copied().ok_or(VmError::OutOfBounds {
                index: idx,
                len: obj.fields.len(),
            })?;
            frame.stack.push(v);
        }
        Opcode::MK_VAR | Opcode::MK_VAR_W => {
            let tag = operand;
            let payload = pop(frame)?;
            let ptr = heap.alloc(0, vec![payload]);
            let ptr_usize = usize::try_from(ptr).map_err(|_| VmError::Malformed {
                desc: "variant heap pointer overflows usize".into(),
            })?;
            let obj = heap.get_mut(ptr_usize)?;
            obj.tag = Some(tag);
            frame.stack.push(Value::from_ref(ptr));
        }
        Opcode::LD_PAY => {
            let field_idx = usize::try_from(operand).map_err(|_| VmError::Malformed {
                desc: "ld.pay field index overflow".into(),
            })?;
            let obj_val = pop(frame)?;
            let ptr = obj_val.as_ref()?;
            let obj = heap.get(ptr)?;
            let raw_payload = obj.fields.first().copied().unwrap_or(Value::UNIT);
            // Multi-field payloads are stored as a nested product.
            // If the payload is a heap ref with multiple fields, index into it.
            let payload = if let Ok(inner_ptr) = raw_payload.as_ref()
                && let Ok(inner) = heap.get(inner_ptr)
                && inner.fields.len() > 1
            {
                inner.fields.get(field_idx).copied().unwrap_or(Value::UNIT)
            } else if field_idx == 0 {
                raw_payload
            } else {
                Value::UNIT
            };
            frame.stack.push(payload);
        }
        Opcode::CMP_TAG | Opcode::CMP_TAG_W => {
            let expected_tag = operand;
            let obj_val = pop(frame)?;
            let ptr = obj_val.as_ref()?;
            let obj = heap.get(ptr)?;
            let matches = obj.tag == Some(expected_tag);
            frame.stack.push(Value::from_bool(matches));
        }
        _ => return Ok(None),
    }
    Ok(Some(true))
}

/// §9/§12 Array, allocation, and globals.
///
/// Returns `true` if handled, `false` if the opcode is not in this group.
fn exec_array_alloc_globals(
    op: Opcode,
    operand: u32,
    frame: &mut Frame,
    heap: &mut Heap,
    globals: &mut Vec<Value>,
) -> Result<bool, VmError> {
    match op {
        Opcode::LD_TAG => exec_ld_tag(frame, heap),
        Opcode::LD_LEN => exec_ld_len(frame, heap),
        Opcode::LD_IDX => exec_ld_idx(frame, heap),
        Opcode::ST_IDX => exec_st_idx(frame, heap),
        Opcode::FRE => {
            let obj_val = pop(frame)?;
            let ptr = obj_val.as_ref()?;
            heap.free(ptr)?;
            Ok(true)
        }
        Opcode::MK_ARR => exec_mk_arr(operand, frame, heap),
        Opcode::ALC_REF => {
            let initial = pop(frame)?;
            let ptr = heap.alloc(operand, vec![initial]);
            frame.stack.push(Value::from_ref(ptr));
            Ok(true)
        }
        Opcode::ALC_ARN => {
            let ptr = heap.alloc(operand, vec![]);
            frame.stack.push(Value::from_ref(ptr));
            Ok(true)
        }
        Opcode::ST_FLD => exec_st_fld(operand, frame, heap),
        Opcode::LD_GLB => exec_ld_glb(operand, frame, globals),
        Opcode::ST_GLB => exec_st_glb(operand, frame, globals),
        _ => Ok(false),
    }
}

fn exec_ld_tag(frame: &mut Frame, heap: &Heap) -> Result<bool, VmError> {
    let obj_val = pop(frame)?;
    let ptr = obj_val.as_ref()?;
    let obj = heap.get(ptr)?;
    let tag = obj.tag.unwrap_or(0);
    frame.stack.push(Value::from_int(i64::from(tag)));
    Ok(true)
}

fn exec_ld_len(frame: &mut Frame, heap: &Heap) -> Result<bool, VmError> {
    let obj_val = pop(frame)?;
    let ptr = obj_val.as_ref()?;
    let obj = heap.get(ptr)?;
    let len = u64::try_from(obj.elems.len()).unwrap_or(u64::MAX);
    frame.stack.push(Value::from_uint(len));
    Ok(true)
}

fn exec_ld_idx(frame: &mut Frame, heap: &Heap) -> Result<bool, VmError> {
    let idx_val = pop(frame)?;
    let arr_val = pop(frame)?;
    let idx = as_usize(idx_val)?;
    let ptr = arr_val.as_ref()?;
    let obj = heap.get(ptr)?;
    let v = obj.elems.get(idx).copied().ok_or(VmError::OutOfBounds {
        index: idx,
        len: obj.elems.len(),
    })?;
    frame.stack.push(v);
    Ok(true)
}

fn exec_st_idx(frame: &mut Frame, heap: &mut Heap) -> Result<bool, VmError> {
    let val = pop(frame)?;
    let idx_val = pop(frame)?;
    let arr_val = pop(frame)?;
    let idx = as_usize(idx_val)?;
    let ptr = arr_val.as_ref()?;
    let obj = heap.get_mut(ptr)?;
    let len = obj.elems.len();
    let elem = obj
        .elems
        .get_mut(idx)
        .ok_or(VmError::OutOfBounds { index: idx, len })?;
    *elem = val;
    Ok(true)
}

fn exec_st_fld(operand: u32, frame: &mut Frame, heap: &mut Heap) -> Result<bool, VmError> {
    let field_idx = usize::try_from(operand).map_err(|_| VmError::Malformed {
        desc: "st.fld index overflow".into(),
    })?;
    let val = pop(frame)?;
    let obj_val = pop(frame)?;
    let ptr = obj_val.as_ref()?;
    let obj = heap.get_mut(ptr)?;
    let len = obj.fields.len();
    let field = obj.fields.get_mut(field_idx).ok_or(VmError::OutOfBounds {
        index: field_idx,
        len,
    })?;
    *field = val;
    Ok(true)
}

fn exec_mk_arr(operand: u32, frame: &mut Frame, heap: &mut Heap) -> Result<bool, VmError> {
    let len_val = pop(frame)?;
    let len = as_usize(len_val)?;
    let elems = vec![Value::UNIT; len];
    let ptr = heap.alloc_array(operand, elems);
    frame.stack.push(Value::from_ref(ptr));
    Ok(true)
}

fn exec_ld_glb(operand: u32, frame: &mut Frame, globals: &mut Vec<Value>) -> Result<bool, VmError> {
    let idx = usize::try_from(operand).map_err(|_| VmError::Malformed {
        desc: "ld.glb index overflow".into(),
    })?;
    if idx >= globals.len() {
        globals.resize(idx + 1, Value::UNIT);
    }
    frame.stack.push(globals[idx]);
    Ok(true)
}

fn exec_st_glb(operand: u32, frame: &mut Frame, globals: &mut Vec<Value>) -> Result<bool, VmError> {
    let idx = usize::try_from(operand).map_err(|_| VmError::Malformed {
        desc: "st.glb index overflow".into(),
    })?;
    let val = pop(frame)?;
    if idx >= globals.len() {
        globals.resize(idx + 1, Value::UNIT);
    }
    globals[idx] = val;
    Ok(true)
}

/// Extract a `usize` from either an int or uint value.
fn as_usize(v: Value) -> Result<usize, VmError> {
    if let Ok(u) = v.as_uint() {
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

fn pop(frame: &mut Frame) -> Result<Value, VmError> {
    frame.stack.pop().ok_or_else(|| VmError::Malformed {
        desc: "operand stack underflow".into(),
    })
}

fn get_local(frame: &Frame, slot: usize) -> Result<Value, VmError> {
    frame.locals.get(slot).copied().ok_or(VmError::OutOfBounds {
        index: slot,
        len: frame.locals.len(),
    })
}

fn set_local(frame: &mut Frame, slot: usize, v: Value) -> Result<(), VmError> {
    let len = frame.locals.len();
    let dest = frame
        .locals
        .get_mut(slot)
        .ok_or(VmError::OutOfBounds { index: slot, len })?;
    *dest = v;
    Ok(())
}

fn const_to_value(c: &LoadedConst, heap: &mut Heap) -> Value {
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
