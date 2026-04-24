use music_seam::TypeId;

use crate::VmValueKind::Seq;
use crate::error::{VmError, VmErrorKind};
use crate::types::VmResult;
use crate::value::{GcRef, I64ArrayValue, SequenceValue, Value, ValueList};

use super::super::error::invalid_heap_ref;
use super::super::object::HeapObject;
use super::RuntimeHeap;
use super::util::fast_int;

impl RuntimeHeap {
    pub(crate) fn sequence(&self, reference: GcRef) -> VmResult<&SequenceValue> {
        match self.object(reference)? {
            HeapObject::Seq(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    #[cfg(test)]
    pub(crate) fn sequence_mut(&mut self, reference: GcRef) -> VmResult<&mut SequenceValue> {
        self.mark_write_barrier(reference)?;
        match self.object_mut(reference)? {
            HeapObject::Seq(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    pub(crate) fn sequence_len(&self, reference: GcRef) -> VmResult<usize> {
        match self.object(reference)? {
            HeapObject::Seq(value) => Ok(value.len()),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    pub(crate) fn sequence_ty(&self, reference: GcRef) -> VmResult<TypeId> {
        match self.object(reference)? {
            HeapObject::Seq(value) => Ok(value.ty),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    pub(crate) fn sequence_get_cloned(&self, reference: GcRef, index: usize) -> VmResult<Value> {
        match sequence_read(self, reference, index)? {
            SequenceRead::Inline { value, len } => {
                value.ok_or_else(|| invalid_sequence_index(index, len))
            }
            SequenceRead::I64Array { buffer, len } => self
                .i64_array(buffer)?
                .get(index)
                .map(Value::Int)
                .ok_or_else(|| invalid_sequence_index(index, len)),
        }
    }

    pub(crate) fn i64_array(&self, reference: GcRef) -> VmResult<&I64ArrayValue> {
        match self.object(reference)? {
            HeapObject::I64Array(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "i64 array")),
        }
    }

    pub(crate) fn sequence_set(
        &mut self,
        reference: GcRef,
        index: usize,
        value: Value,
    ) -> VmResult {
        self.mark_write_barrier(reference)?;
        if let Some((buffer, len)) = self.sequence(reference)?.i64_array() {
            if index >= len {
                return Err(invalid_sequence_index(index, len));
            }
            return match value {
                Value::Int(value) => {
                    if self.i64_array(buffer)?.is_shared() {
                        let mut items = self.i64_array_values(buffer)?;
                        *items.get_mut(index).expect("index checked before detach") =
                            Value::Int(value);
                        let HeapObject::Seq(sequence) = self.object_mut(reference)? else {
                            return Err(invalid_heap_ref(reference, "sequence"));
                        };
                        sequence.replace_with_values(items);
                        Ok(())
                    } else {
                        self.i64_array_set(buffer, index, value)
                    }
                }
                value => {
                    let mut items = self.i64_array_values(buffer)?;
                    *items.get_mut(index).expect("index checked before demotion") = value;
                    let HeapObject::Seq(sequence) = self.object_mut(reference)? else {
                        return Err(invalid_heap_ref(reference, "sequence"));
                    };
                    sequence.replace_with_values(items);
                    Ok(())
                }
            };
        }
        let HeapObject::Seq(sequence) = self.object_mut(reference)? else {
            return Err(invalid_heap_ref(reference, "sequence"));
        };
        let len = sequence.len();
        if sequence.set(index, value).is_none() {
            return Err(VmError::new(VmErrorKind::InvalidSequenceIndex {
                index: i64::try_from(index).unwrap_or(i64::MAX),
                len,
            }));
        }
        Ok(())
    }

    pub(crate) fn sequence_items_cloned(&self, reference: GcRef) -> VmResult<Vec<Value>> {
        if let Some((buffer, _)) = self.sequence(reference)?.i64_array() {
            return Ok(self.i64_array_values(buffer)?.into_iter().collect());
        }
        let len = self.sequence_len(reference)?;
        (0..len)
            .map(|index| self.sequence_get_cloned(reference, index))
            .collect()
    }

    pub(crate) fn sequence_int_at(&self, reference: GcRef, index: usize) -> VmResult<i64> {
        fast_int(&self.sequence_get_cloned(reference, index)?)
    }

    pub(crate) fn sequence_seq_at(&self, reference: GcRef, index: usize) -> VmResult<GcRef> {
        match self.sequence_get_cloned(reference, index)? {
            Value::Seq(reference) => Ok(reference),
            other => Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: Seq,
                found: other.kind(),
            })),
        }
    }

    pub(crate) fn sequence_int_pair(&self, reference: GcRef) -> VmResult<[i64; 2]> {
        match self.object(reference)? {
            HeapObject::Seq(value) => value
                .int_pair()
                .ok_or_else(|| invalid_heap_ref(reference, "int pair sequence")),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    pub(crate) fn sequence_set_int_pair_cell(
        &mut self,
        reference: GcRef,
        index: usize,
        value: i64,
    ) -> VmResult {
        self.mark_write_barrier(reference)?;
        match self.object_mut(reference)? {
            HeapObject::Seq(sequence) => sequence
                .set_int_pair_cell(index, value)
                .ok_or_else(|| invalid_heap_ref(reference, "int pair sequence")),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    fn i64_array_set(&mut self, reference: GcRef, index: usize, value: i64) -> VmResult {
        match self.object_mut(reference)? {
            HeapObject::I64Array(array) => array
                .set(index, value)
                .ok_or_else(|| invalid_sequence_index(index, array.len())),
            _ => Err(invalid_heap_ref(reference, "i64 array")),
        }
    }

    fn i64_array_values(&self, reference: GcRef) -> VmResult<ValueList> {
        match self.object(reference)? {
            HeapObject::I64Array(array) => {
                Ok(array.values().iter().copied().map(Value::Int).collect())
            }
            _ => Err(invalid_heap_ref(reference, "i64 array")),
        }
    }

    #[cfg(test)]
    pub(crate) fn sequence_i64_array_ref(&self, reference: GcRef) -> VmResult<Option<GcRef>> {
        Ok(self
            .sequence(reference)?
            .i64_array()
            .map(|(buffer, _)| buffer))
    }
}

enum SequenceRead {
    Inline { value: Option<Value>, len: usize },
    I64Array { buffer: GcRef, len: usize },
}

fn sequence_read(heap: &RuntimeHeap, reference: GcRef, index: usize) -> VmResult<SequenceRead> {
    match heap.object(reference)? {
        HeapObject::Seq(value) => {
            if let Some((buffer, len)) = value.i64_array() {
                Ok(SequenceRead::I64Array { buffer, len })
            } else {
                Ok(SequenceRead::Inline {
                    value: value.get_cloned(index),
                    len: value.len(),
                })
            }
        }
        _ => Err(invalid_heap_ref(reference, "sequence")),
    }
}

fn invalid_sequence_index(index: usize, len: usize) -> VmError {
    VmError::new(VmErrorKind::InvalidSequenceIndex {
        index: i64::try_from(index).unwrap_or(i64::MAX),
        len,
    })
}
