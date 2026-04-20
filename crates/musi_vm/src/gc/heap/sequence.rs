use music_seam::TypeId;

use crate::VmValueKind::Seq;
use crate::error::{VmError, VmErrorKind};
use crate::types::VmResult;
use crate::value::{GcRef, SequenceValue, Value, ValueList};

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
            HeapObject::Seq(value) => Ok(value.items.len()),
            HeapObject::PackedSeq2x2(_) | HeapObject::PackedSeq2x2Row(_) => Ok(2),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    pub(crate) fn sequence_ty(&self, reference: GcRef) -> VmResult<TypeId> {
        match self.object(reference)? {
            HeapObject::Seq(value) => Ok(value.ty),
            HeapObject::PackedSeq2x2(value) => Ok(value.ty),
            HeapObject::PackedSeq2x2Row(value) => Ok(value.ty),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    pub(crate) fn sequence_get_cloned(&self, reference: GcRef, index: usize) -> VmResult<Value> {
        match self.object(reference)? {
            HeapObject::Seq(value) => value.items.get(index).cloned().ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidSequenceIndex {
                    index: i64::try_from(index).unwrap_or(i64::MAX),
                    len: value.items.len(),
                })
            }),
            HeapObject::PackedSeq2x2(value) => value
                .row_refs
                .get(index)
                .copied()
                .map(Value::Seq)
                .ok_or_else(|| {
                    VmError::new(VmErrorKind::InvalidSequenceIndex {
                        index: i64::try_from(index).unwrap_or(i64::MAX),
                        len: 2,
                    })
                }),
            HeapObject::PackedSeq2x2Row(value) => {
                if index >= 2 {
                    return Err(VmError::new(VmErrorKind::InvalidSequenceIndex {
                        index: i64::try_from(index).unwrap_or(i64::MAX),
                        len: 2,
                    }));
                }
                let grid = self.packed_grid(value.grid)?;
                Ok(Value::Int(grid.cell_for(value.logical_row, index)))
            }
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    pub(crate) fn sequence_set(
        &mut self,
        reference: GcRef,
        index: usize,
        value: Value,
    ) -> VmResult {
        self.mark_write_barrier(reference)?;
        match self.object(reference)? {
            HeapObject::PackedSeq2x2(_) => {
                self.promote_packed_grid(reference)?;
                return self.sequence_set(reference, index, value);
            }
            HeapObject::PackedSeq2x2Row(_) => {
                if index >= 2 {
                    return Err(VmError::new(VmErrorKind::InvalidSequenceIndex {
                        index: i64::try_from(index).unwrap_or(i64::MAX),
                        len: 2,
                    }));
                }
                let int = fast_int(&value)?;
                let HeapObject::PackedSeq2x2Row(row) = self.object(reference)? else {
                    return Err(invalid_heap_ref(reference, "packed row"));
                };
                let grid_ref = row.grid;
                let logical_row = row.logical_row;
                let grid = self.packed_grid_mut(grid_ref)?;
                grid.set_cell_for(logical_row, index, int);
                return Ok(());
            }
            _ => {}
        }
        let HeapObject::Seq(sequence) = self.object_mut(reference)? else {
            return Err(invalid_heap_ref(reference, "sequence"));
        };
        let len = sequence.items.len();
        let Some(slot) = sequence.items.get_mut(index) else {
            return Err(VmError::new(VmErrorKind::InvalidSequenceIndex {
                index: i64::try_from(index).unwrap_or(i64::MAX),
                len,
            }));
        };
        *slot = value;
        Ok(())
    }

    pub(crate) fn sequence_items_cloned(&self, reference: GcRef) -> VmResult<Vec<Value>> {
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

    fn promote_packed_grid(&mut self, grid_ref: GcRef) -> VmResult {
        let (ty, row_refs) = {
            let grid = self.packed_grid(grid_ref)?;
            (grid.ty, grid.row_refs)
        };
        let row0 = self.promote_packed_row_to_plain(row_refs[0])?;
        let row1 = if row_refs[1] == row_refs[0] {
            row0
        } else {
            self.promote_packed_row_to_plain(row_refs[1])?
        };
        let mut items = ValueList::new();
        items.push(Value::Seq(row0));
        items.push(Value::Seq(row1));
        *self.object_mut(grid_ref)? = HeapObject::Seq(SequenceValue::new(ty, items));
        Ok(())
    }

    fn promote_packed_row_to_plain(&mut self, row_ref: GcRef) -> VmResult<GcRef> {
        let (ty, grid_ref, logical_row) = match self.object(row_ref)? {
            HeapObject::PackedSeq2x2Row(row) => (row.ty, row.grid, row.logical_row),
            HeapObject::Seq(_) => return Ok(row_ref),
            _ => return Err(invalid_heap_ref(row_ref, "packed row")),
        };
        let grid = self.packed_grid(grid_ref)?;
        let mut items = ValueList::new();
        items.push(Value::Int(grid.cell_for(logical_row, 0)));
        items.push(Value::Int(grid.cell_for(logical_row, 1)));
        *self.object_mut(row_ref)? = HeapObject::Seq(SequenceValue::new(ty, items));
        Ok(row_ref)
    }
}
