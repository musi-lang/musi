use crate::error::{VmError, VmErrorKind};
use crate::types::VmResult;
use crate::value::{ClosureValue, ContinuationValue, DataValue, GcRef, ModuleValue};
use music_term::SyntaxTerm;

use super::super::error::{invalid_heap_ref, stale_heap_ref};
use super::super::object::{HeapObject, PackedSeq2x2};
use super::super::space::HeapSlot;
use super::{RuntimeHeap, Seq2x2ArgCache};

impl RuntimeHeap {
    pub(crate) fn bind_seq2x2_packed_arg(&mut self, reference: GcRef) -> VmResult<Seq2x2ArgCache> {
        let (grid_slot, grid_generation, row_refs) = match self.object(reference)? {
            HeapObject::PackedSeq2x2(grid) => {
                let slot = self.slot(reference)?;
                (reference.slot(), slot.generation, grid.row_refs)
            }
            _ => return Err(invalid_heap_ref(reference, "packed sequence([2][2]Int)")),
        };
        let row0_slot = self.slot(row_refs[0])?;
        let row1_slot = self.slot(row_refs[1])?;
        let cache = Seq2x2ArgCache {
            isolate: reference.isolate(),
            grid_slot,
            grid_generation,
            row0_slot: row_refs[0].slot(),
            row0_generation: row0_slot.generation,
            row1_slot: row_refs[1].slot(),
            row1_generation: row1_slot.generation,
        };
        self.pin_cache_slot(grid_slot, grid_generation)?;
        self.pin_cache_slot(cache.row0_slot, cache.row0_generation)?;
        self.pin_cache_slot(cache.row1_slot, cache.row1_generation)?;
        Ok(cache)
    }

    pub(crate) fn unpin_seq2x2_packed_arg(&mut self, cache: Seq2x2ArgCache) {
        self.unpin_cache_slot(cache.grid_slot, cache.grid_generation);
        self.unpin_cache_slot(cache.row0_slot, cache.row0_generation);
        self.unpin_cache_slot(cache.row1_slot, cache.row1_generation);
    }

    pub(crate) fn validate_ref(&self, reference: GcRef) -> VmResult {
        self.slot(reference).map(|_| ())
    }

    fn pin_cache_slot(&mut self, slot_index: usize, generation: u32) -> VmResult {
        let Some(slot) = self.slots.get_mut(slot_index) else {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "stale cached pin slot".into(),
            }));
        };
        if slot.generation != generation || slot.object.is_none() {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "stale cached pin generation".into(),
            }));
        }
        slot.pin_count = slot.pin_count.saturating_add(1);
        slot.is_pinned = slot.pin_count > 0;
        Ok(())
    }

    fn unpin_cache_slot(&mut self, slot_index: usize, generation: u32) {
        let Some(slot) = self.slots.get_mut(slot_index) else {
            return;
        };
        if slot.generation != generation || slot.object.is_none() {
            return;
        }
        slot.pin_count = slot.pin_count.saturating_sub(1);
        slot.is_pinned = slot.pin_count > 0;
    }

    fn checked_slot_index(&self, reference: GcRef) -> VmResult<usize> {
        if reference.isolate() != self.isolate {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "cross-isolate heap reference".into(),
            }));
        }
        Ok(reference.slot())
    }

    pub(in crate::gc) fn object(&self, reference: GcRef) -> VmResult<&HeapObject> {
        self.slot(reference)?
            .object
            .as_ref()
            .ok_or_else(|| stale_heap_ref(reference))
    }

    pub(in crate::gc) fn object_mut(&mut self, reference: GcRef) -> VmResult<&mut HeapObject> {
        self.slot_mut(reference)?
            .object
            .as_mut()
            .ok_or_else(|| stale_heap_ref(reference))
    }

    pub(crate) fn string(&self, reference: GcRef) -> VmResult<&str> {
        match self.object(reference)? {
            HeapObject::String(text) => Ok(text),
            _ => Err(invalid_heap_ref(reference, "string")),
        }
    }

    pub(crate) fn syntax(&self, reference: GcRef) -> VmResult<&SyntaxTerm> {
        match self.object(reference)? {
            HeapObject::Syntax(term) => Ok(term),
            _ => Err(invalid_heap_ref(reference, "syntax")),
        }
    }

    pub(crate) fn data(&self, reference: GcRef) -> VmResult<&DataValue> {
        match self.object(reference)? {
            HeapObject::Data(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "data")),
        }
    }

    pub(crate) fn data_mut(&mut self, reference: GcRef) -> VmResult<&mut DataValue> {
        self.mark_write_barrier(reference)?;
        match self.object_mut(reference)? {
            HeapObject::Data(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "data")),
        }
    }

    pub(crate) fn closure(&self, reference: GcRef) -> VmResult<&ClosureValue> {
        match self.object(reference)? {
            HeapObject::Closure(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "closure")),
        }
    }

    pub(crate) fn module(&self, reference: GcRef) -> VmResult<&ModuleValue> {
        match self.object(reference)? {
            HeapObject::Module(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "module")),
        }
    }

    pub(crate) fn continuation(&self, reference: GcRef) -> VmResult<&ContinuationValue> {
        match self.object(reference)? {
            HeapObject::Continuation(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "continuation")),
        }
    }

    pub(in crate::gc) fn slot(&self, reference: GcRef) -> VmResult<&HeapSlot> {
        let slot = self
            .slots
            .get(self.checked_slot_index(reference)?)
            .ok_or_else(|| stale_heap_ref(reference))?;
        if slot.is_live_generation(reference) {
            Ok(slot)
        } else {
            Err(stale_heap_ref(reference))
        }
    }

    pub(in crate::gc) fn slot_mut(&mut self, reference: GcRef) -> VmResult<&mut HeapSlot> {
        let slot_index = self.checked_slot_index(reference)?;
        let slot = self
            .slots
            .get_mut(slot_index)
            .ok_or_else(|| stale_heap_ref(reference))?;
        if slot.is_live_generation(reference) {
            Ok(slot)
        } else {
            Err(stale_heap_ref(reference))
        }
    }

    pub(super) fn packed_grid(&self, reference: GcRef) -> VmResult<&PackedSeq2x2> {
        match self.object(reference)? {
            HeapObject::PackedSeq2x2(grid) => Ok(grid),
            _ => Err(invalid_heap_ref(reference, "packed grid")),
        }
    }

    pub(super) fn packed_grid_mut(&mut self, reference: GcRef) -> VmResult<&mut PackedSeq2x2> {
        match self.object_mut(reference)? {
            HeapObject::PackedSeq2x2(grid) => Ok(grid),
            _ => Err(invalid_heap_ref(reference, "packed grid")),
        }
    }
}
