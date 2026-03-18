//! Record, tuple, optional, and match opcode handlers.

use crate::error::{malformed, VmError};
use crate::heap::Heap;
use crate::value::Value;
use crate::vm::Frame;

// --------------------------------------------------------------------------
// §4.9 Record
// --------------------------------------------------------------------------

/// `REC_NEW tag:u8, arity:u8` — allocate a record/variant and push a ref.
pub fn exec_rec_new(operand: u32, frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let tag = super::fi8x2_a(operand);
    let arity = super::fi8x2_b(operand);
    let n = usize::from(arity);
    let mut fields: Vec<Value> = Vec::with_capacity(n);
    for _ in 0..n {
        fields.push(frame.pop()?);
    }
    fields.reverse();
    let variant_tag = if tag != 0 { Some(u32::from(tag)) } else { None };
    let ptr = heap.alloc_record(0, variant_tag, fields);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

pub fn exec_rec_get(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let idx = usize::try_from(operand).map_err(|_| malformed!("rec.get index overflow"))?;
    let record_val = frame.pop()?;
    let ptr = record_val.as_ref()?;
    let fields = heap.get_record_fields(ptr)?;
    let v = fields.get(idx).copied().ok_or(VmError::OutOfBounds {
        index: idx,
        len: fields.len(),
    })?;
    frame.stack.push(v);
    Ok(())
}

pub fn exec_rec_set(operand: u32, frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let field_idx = usize::try_from(operand).map_err(|_| malformed!("rec.set index overflow"))?;
    let val = frame.pop()?;
    let record_val = frame.pop()?;
    let ptr = record_val.as_ref()?;
    let fields = heap.get_record_fields_mut(ptr)?;
    let len = fields.len();
    let field = fields.get_mut(field_idx).ok_or(VmError::OutOfBounds {
        index: field_idx,
        len,
    })?;
    *field = val;
    Ok(())
}

// --------------------------------------------------------------------------
// §4.14 Match
// --------------------------------------------------------------------------

pub fn exec_mat_data(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let field_idx =
        usize::try_from(operand).map_err(|_| malformed!("mat.data field index overflow"))?;
    let variant_val = frame.pop()?;
    let ptr = variant_val.as_ref()?;
    let fields = heap.get_record_fields(ptr)?;
    let payload = fields.get(field_idx).copied().unwrap_or(Value::UNIT);
    frame.stack.push(payload);
    Ok(())
}

/// `MAT_TAG tag:u16` — pop a ref, push bool(obj.tag == tag).
pub fn exec_mat_tag(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let expected_tag = operand & 0xFFFF;
    let variant_val = frame.pop()?;
    let ptr = variant_val.as_ref()?;
    let tag = heap.get_record_tag(ptr)?;
    frame
        .stack
        .push(Value::from_bool(tag == Some(expected_tag)));
    Ok(())
}

// --------------------------------------------------------------------------
// §4.11 Tuple
// --------------------------------------------------------------------------

pub fn exec_tup_new(operand: u32, frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let n = usize::try_from(operand).map_err(|_| malformed!("tup.new arity overflow"))?;
    let mut fields: Vec<Value> = Vec::with_capacity(n);
    for _ in 0..n {
        fields.push(frame.pop()?);
    }
    fields.reverse();
    let ptr = heap.alloc_record(0, None, fields);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

pub fn exec_tup_get(operand: u32, frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let idx = usize::try_from(operand).map_err(|_| malformed!("tup.get index overflow"))?;
    let tup_val = frame.pop()?;
    let ptr = tup_val.as_ref()?;
    let fields = heap.get_record_fields(ptr)?;
    let v = fields.get(idx).copied().ok_or(VmError::OutOfBounds {
        index: idx,
        len: fields.len(),
    })?;
    frame.stack.push(v);
    Ok(())
}

// --------------------------------------------------------------------------
// §4.15 Optional
// --------------------------------------------------------------------------

pub fn exec_opt_some(frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let val = frame.pop()?;
    // Represent Some(v) as a single-field record with tag=1.
    let ptr = heap.alloc_record(0, Some(1), vec![val]);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

pub fn exec_opt_none(frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    // Represent None as an empty record with tag=0.
    let ptr = heap.alloc_record(0, Some(0), vec![]);
    frame.stack.push(Value::from_ref(ptr));
    Ok(())
}

pub fn exec_opt_is(frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let val = frame.pop()?;
    let is_some = if let Ok(ptr) = val.as_ref() {
        heap.get_record_tag(ptr).is_ok_and(|tag| tag == Some(1))
    } else {
        false
    };
    frame.stack.push(Value::from_bool(is_some));
    Ok(())
}

pub fn exec_opt_get(frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let val = frame.pop()?;
    let ptr = val.as_ref()?;
    if heap.get_record_tag(ptr)? != Some(1) {
        return Err(malformed!("opt.get: value is None"));
    }
    let fields = heap.get_record_fields(ptr)?;
    let inner = fields.first().copied().unwrap_or(Value::UNIT);
    frame.stack.push(inner);
    Ok(())
}

// --------------------------------------------------------------------------
// §4.1 Data movement — indirect memory
// --------------------------------------------------------------------------

pub fn exec_ld_ind(frame: &mut Frame, heap: &Heap) -> Result<(), VmError> {
    let addr_val = frame.pop()?;
    let ptr = addr_val.as_ref()?;
    let fields = heap.get_record_fields(ptr)?;
    let v = fields.first().copied().unwrap_or(Value::UNIT);
    frame.stack.push(v);
    Ok(())
}

pub fn exec_st_ind(frame: &mut Frame, heap: &mut Heap) -> Result<(), VmError> {
    let val = frame.pop()?;
    let addr_val = frame.pop()?;
    let ptr = addr_val.as_ref()?;
    let fields = heap.get_record_fields_mut(ptr)?;
    if fields.is_empty() {
        fields.push(val);
    } else {
        fields[0] = val;
    }
    Ok(())
}
