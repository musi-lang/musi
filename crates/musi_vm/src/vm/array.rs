use music_il::opcode::Opcode;

use super::Vm;
use crate::errors::{VmError, VmResult};
use crate::heap::{Heap, HeapObject};
use crate::value::Value;

enum CopyData {
    Array {
        tag: Value,
        elements: Vec<Value>,
    },
    Str(String),
    Slice {
        source: usize,
        start: usize,
        end: usize,
    },
}

enum ConcatData {
    Str(String),
    Array { tag: Value, elems: Vec<Value> },
}

impl Vm {
    pub(super) fn dispatch_array(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> VmResult {
        match op {
            Opcode::ArrGetI | Opcode::ArrSetI | Opcode::ArrGet | Opcode::ArrSet => {
                return self.dispatch_arr_rw(op, method_idx, pc);
            }
            Opcode::ArrNew => {
                let type_id = self.read_u16(method_idx, pc);
                let len = usize::from(self.read_u16(method_idx, pc));
                let heap_idx =
                    self.heap
                        .alloc_array_t(type_id, Value::UNIT, vec![Value::UNIT; len]);
                self.push_stack(Value::from_ptr(heap_idx))?;
                self.maybe_collect();
            }
            Opcode::ArrNewT => {
                let type_id = self.read_u16(method_idx, pc);
                let tag_pool_idx = usize::from(self.read_u8(method_idx, pc));
                let len = usize::from(self.read_u16(method_idx, pc));
                let tag = self
                    .resolved_constants
                    .get(tag_pool_idx)
                    .copied()
                    .ok_or(VmError::InvalidConstant(tag_pool_idx))?;
                let heap_idx = self
                    .heap
                    .alloc_array_t(type_id, tag, vec![Value::UNIT; len]);
                self.push_stack(Value::from_ptr(heap_idx))?;
                self.maybe_collect();
            }
            Opcode::ArrLen => {
                let ptr = self.pop_stack()?;
                if !ptr.is_ptr() {
                    return Err(VmError::NotArray);
                }
                let obj = self.heap.get(ptr.as_ptr_idx()).ok_or(VmError::NotArray)?;
                let len = match obj {
                    HeapObject::Array(arr) => arr.elements.len(),
                    HeapObject::String(s) => s.len(),
                    HeapObject::Slice(sl) => sl.end - sl.start,
                    _ => return Err(VmError::NotArray),
                };
                self.frames
                    .last_mut()
                    .unwrap()
                    .push(Value::from_int(i64::try_from(len).unwrap_or(i64::MAX)));
            }
            Opcode::ArrTag => {
                let ptr = self.pop_stack()?;
                if !ptr.is_ptr() {
                    return Err(VmError::NotArray);
                }
                let ptr_idx = ptr.as_ptr_idx();
                let tag = match self.heap.get(ptr_idx).ok_or(VmError::NotArray)? {
                    HeapObject::Array(arr) => arr.tag,
                    HeapObject::String(_) => {
                        let str_idx = self.heap.alloc_string("Str".into());
                        Value::from_ptr(str_idx)
                    }
                    HeapObject::Slice(_) => Value::UNIT,
                    _ => return Err(VmError::NotArray),
                };
                self.frames.last_mut().unwrap().push(tag);
            }
            Opcode::ArrCopy => {
                self.exec_arr_copy()?;
                self.maybe_collect();
            }
            Opcode::ArrCaten => {
                let b = self.pop_stack()?;
                let a = self.pop_stack()?;
                let result = exec_arr_concat(&mut self.heap, a, b)?;
                self.frames.last_mut().unwrap().push(result);
                self.maybe_collect();
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    pub(super) fn dispatch_arr_rw(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> VmResult {
        match op {
            Opcode::ArrGetI => {
                let elem_idx = usize::from(self.read_u8(method_idx, pc));
                let arr_ptr = self.pop_stack()?;
                let val = exec_arr_geti(&self.heap, arr_ptr, elem_idx)?;
                self.frames.last_mut().unwrap().push(val);
            }
            Opcode::ArrSetI => {
                let elem_idx = usize::from(self.read_u8(method_idx, pc));
                let val = self.pop_stack()?;
                let arr_ptr = self.peek_stack()?;
                exec_arr_seti(&mut self.heap, arr_ptr, elem_idx, val)?;
                if arr_ptr.is_ptr() {
                    self.write_barrier(arr_ptr.as_ptr_idx(), val);
                }
            }
            Opcode::ArrGet => {
                let idx_val = self.pop_stack()?;
                let arr_ptr = self.pop_stack()?;
                let val = exec_arr_get(&self.heap, arr_ptr, idx_val)?;
                self.frames.last_mut().unwrap().push(val);
            }
            Opcode::ArrSet => {
                let val = self.pop_stack()?;
                let idx_val = self.pop_stack()?;
                let arr_ptr = self.pop_stack()?;
                exec_arr_set(&mut self.heap, arr_ptr, idx_val, val)?;
                if arr_ptr.is_ptr() {
                    self.write_barrier(arr_ptr.as_ptr_idx(), val);
                }
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    pub(super) fn exec_arr_slice(&mut self) -> VmResult {
        let end_val = self.pop_stack()?;
        let start_val = self.pop_stack()?;
        let arr_ptr = self.pop_stack()?;
        if !arr_ptr.is_ptr() {
            return Err(VmError::NotArray);
        }
        if !start_val.is_int() || !end_val.is_int() {
            return Err(VmError::TypeError {
                expected: "`Int`",
                found: "non-`Int`",
            });
        }
        let start = usize::try_from(start_val.as_int()).map_err(|_| VmError::IndexOutOfBounds {
            index: 0,
            length: 0,
        })?;
        let end = usize::try_from(end_val.as_int()).map_err(|_| VmError::IndexOutOfBounds {
            index: 0,
            length: 0,
        })?;
        let source = arr_ptr.as_ptr_idx();
        let arr_len = match self.heap.get(source) {
            Some(HeapObject::Array(arr)) => arr.elements.len(),
            _ => return Err(VmError::NotArray),
        };
        if start > end || end > arr_len {
            return Err(VmError::IndexOutOfBounds {
                index: end,
                length: arr_len,
            });
        }
        let slice_idx = self.heap.alloc_slice(source, start, end);
        self.push_stack(Value::from_ptr(slice_idx))?;
        Ok(())
    }

    pub(super) fn exec_arr_copy(&mut self) -> VmResult {
        let ptr = self.pop_stack()?;
        if !ptr.is_ptr() {
            return Err(VmError::NotArray);
        }
        let ptr_idx = ptr.as_ptr_idx();
        let copy_data = match self.heap.get(ptr_idx).ok_or(VmError::NotArray)? {
            HeapObject::Array(arr) => CopyData::Array {
                tag: arr.tag,
                elements: arr.elements.clone(),
            },
            HeapObject::String(s) => CopyData::Str(s.data.clone()),
            HeapObject::Slice(sl) => CopyData::Slice {
                source: sl.source,
                start: sl.start,
                end: sl.end,
            },
            _ => return Err(VmError::NotArray),
        };
        let new_idx = match copy_data {
            CopyData::Array { tag, elements } => self.heap.alloc_array(tag, elements),
            CopyData::Str(s) => self.heap.alloc_string(s),
            CopyData::Slice { source, start, end } => {
                let elements = match self.heap.get(source) {
                    Some(HeapObject::Array(arr)) => arr.elements[start..end].to_vec(),
                    _ => return Err(VmError::NotArray),
                };
                self.heap.alloc_array(Value::UNIT, elements)
            }
        };
        self.push_stack(Value::from_ptr(new_idx))?;
        Ok(())
    }

    pub(super) fn exec_arr_fill(&mut self) -> VmResult {
        let len_val = self.pop_stack()?;
        let value = self.pop_stack()?;
        if !len_val.is_int() {
            return Err(VmError::TypeError {
                expected: "`Int`",
                found: "non-`Int`",
            });
        }
        let len = usize::try_from(len_val.as_int()).map_err(|_| VmError::TypeError {
            expected: "non-negative `Int`",
            found: "negative `Int`",
        })?;
        let elements = vec![value; len];
        let heap_idx = self.heap.alloc_array(Value::UNIT, elements);
        self.push_stack(Value::from_ptr(heap_idx))?;
        self.maybe_collect();
        Ok(())
    }
}

pub(super) fn exec_arr_geti(heap: &Heap, arr_ptr: Value, elem_idx: usize) -> VmResult<Value> {
    if !arr_ptr.is_ptr() {
        return Err(VmError::NotArray);
    }
    let ptr_idx = arr_ptr.as_ptr_idx();
    let obj = heap
        .get(ptr_idx)
        .ok_or(VmError::InvalidHeapIndex(ptr_idx))?;
    match obj {
        HeapObject::Array(arr) => {
            let len = arr.elements.len();
            arr.elements
                .get(elem_idx)
                .copied()
                .ok_or(VmError::IndexOutOfBounds {
                    index: elem_idx,
                    length: len,
                })
        }
        HeapObject::String(s) => {
            let bytes = s.as_bytes();
            bytes
                .get(elem_idx)
                .map(|&b| Value::from_int(i64::from(b)))
                .ok_or(VmError::IndexOutOfBounds {
                    index: elem_idx,
                    length: bytes.len(),
                })
        }
        HeapObject::Slice(sl) => {
            let real_idx = sl.start + elem_idx;
            let len = sl.end - sl.start;
            if elem_idx >= len {
                return Err(VmError::IndexOutOfBounds {
                    index: elem_idx,
                    length: len,
                });
            }
            let source = sl.source;
            match heap.get(source) {
                Some(HeapObject::Array(arr)) => {
                    arr.elements
                        .get(real_idx)
                        .copied()
                        .ok_or(VmError::IndexOutOfBounds {
                            index: real_idx,
                            length: arr.elements.len(),
                        })
                }
                _ => Err(VmError::NotArray),
            }
        }
        _ => Err(VmError::NotArray),
    }
}

pub(super) fn exec_arr_seti(
    heap: &mut Heap,
    arr_ptr: Value,
    elem_idx: usize,
    val: Value,
) -> VmResult {
    if !arr_ptr.is_ptr() {
        return Err(VmError::NotArray);
    }
    let ptr_idx = arr_ptr.as_ptr_idx();
    match heap
        .get_mut(ptr_idx)
        .ok_or(VmError::InvalidHeapIndex(ptr_idx))?
    {
        HeapObject::Array(arr) => {
            let len = arr.elements.len();
            *arr.elements
                .get_mut(elem_idx)
                .ok_or(VmError::IndexOutOfBounds {
                    index: elem_idx,
                    length: len,
                })? = val;
            Ok(())
        }
        HeapObject::String(_) => Err(VmError::TypeError {
            expected: "Array",
            found: "String",
        }),
        _ => Err(VmError::NotArray),
    }
}

pub(super) fn exec_arr_get(heap: &Heap, arr_ptr: Value, idx_val: Value) -> VmResult<Value> {
    if !idx_val.is_int() {
        return Err(VmError::TypeError {
            expected: "`Int`",
            found: "non-`Int`",
        });
    }
    let raw_idx = idx_val.as_int();
    let elem_idx = usize::try_from(raw_idx).map_err(|_| VmError::IndexOutOfBounds {
        index: 0,
        length: 0,
    })?;
    exec_arr_geti(heap, arr_ptr, elem_idx)
}

pub(super) fn exec_arr_set(
    heap: &mut Heap,
    arr_ptr: Value,
    idx_val: Value,
    val: Value,
) -> VmResult {
    if !idx_val.is_int() {
        return Err(VmError::TypeError {
            expected: "`Int`",
            found: "non-`Int`",
        });
    }
    let raw_idx = idx_val.as_int();
    let elem_idx = usize::try_from(raw_idx).map_err(|_| VmError::IndexOutOfBounds {
        index: 0,
        length: 0,
    })?;
    exec_arr_seti(heap, arr_ptr, elem_idx, val)
}

pub(super) fn exec_arr_concat(heap: &mut Heap, a: Value, b: Value) -> VmResult<Value> {
    match (a.is_ptr(), b.is_ptr()) {
        (true, true) => {
            let a_idx = a.as_ptr_idx();
            let b_idx = b.as_ptr_idx();
            let concat_data = match (
                heap.get(a_idx).ok_or(VmError::NotArray)?,
                heap.get(b_idx).ok_or(VmError::NotArray)?,
            ) {
                (HeapObject::String(sa), HeapObject::String(sb)) => {
                    let mut result = sa.data.clone();
                    result.push_str(&sb.data);
                    ConcatData::Str(result)
                }
                (HeapObject::Array(arr_a), HeapObject::Array(arr_b)) => {
                    let tag = arr_a.tag;
                    let mut elems = arr_a.elements.clone();
                    elems.extend_from_slice(&arr_b.elements);
                    ConcatData::Array { tag, elems }
                }
                _ => {
                    return Err(VmError::TypeError {
                        expected: "matching types",
                        found: "mismatched",
                    });
                }
            };
            match concat_data {
                ConcatData::Str(s) => Ok(Value::from_ptr(heap.alloc_string(s))),
                ConcatData::Array { tag, elems } => {
                    Ok(Value::from_ptr(heap.alloc_array(tag, elems)))
                }
            }
        }
        (true, false) => {
            let a_idx = a.as_ptr_idx();
            let (tag, mut elems) = match heap.get(a_idx).ok_or(VmError::NotArray)? {
                HeapObject::Array(arr) => (arr.tag, arr.elements.clone()),
                _ => return Err(VmError::NotArray),
            };
            elems.push(b);
            Ok(Value::from_ptr(heap.alloc_array(tag, elems)))
        }
        (false, true) => {
            let b_idx = b.as_ptr_idx();
            let (tag, tail) = match heap.get(b_idx).ok_or(VmError::NotArray)? {
                HeapObject::Array(arr) => (arr.tag, arr.elements.clone()),
                _ => return Err(VmError::NotArray),
            };
            let mut elems = vec![a];
            elems.extend_from_slice(&tail);
            Ok(Value::from_ptr(heap.alloc_array(tag, elems)))
        }
        (false, false) => Ok(Value::UNIT),
    }
}
