use music_term::SyntaxTerm;

use crate::error::{VmError, VmErrorKind};
use crate::types::VmResult;
use crate::value::{
    ClosureValue, ContinuationValue, DataValue, GcRef, ModuleValue, SequenceValue, Value,
};

use super::error::{invalid_heap_ref, stale_heap_ref};
use super::object::HeapObject;
use super::space::{HeapAllocation, HeapSlot, IMMIX_LINE_BYTES, IMMIX_LINES_PER_BLOCK, ImmixBlock};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapOptions {
    pub max_object_bytes: Option<usize>,
}

#[derive(Debug, Default)]
pub struct RuntimeHeap {
    pub(super) slots: Vec<HeapSlot>,
    pub(super) free_slots: Vec<usize>,
    pub(super) blocks: Vec<ImmixBlock>,
    pub(super) allocated_bytes: usize,
}

impl RuntimeHeap {
    #[must_use]
    pub(crate) const fn allocated_bytes(&self) -> usize {
        self.allocated_bytes
    }

    pub(crate) fn alloc_string(
        &mut self,
        text: impl Into<Box<str>>,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        self.alloc_value(HeapObject::String(text.into()), Value::String, options)
    }

    pub(crate) fn alloc_syntax(
        &mut self,
        term: SyntaxTerm,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        self.alloc_value(HeapObject::Syntax(term), Value::Syntax, options)
    }

    pub(crate) fn alloc_sequence(
        &mut self,
        value: SequenceValue,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        self.alloc_value(HeapObject::Seq(value), Value::Seq, options)
    }

    pub(crate) fn alloc_data(
        &mut self,
        value: DataValue,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        self.alloc_value(HeapObject::Data(value), Value::Data, options)
    }

    pub(crate) fn alloc_closure(
        &mut self,
        value: ClosureValue,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        self.alloc_value(HeapObject::Closure(value), Value::Closure, options)
    }

    pub(crate) fn alloc_module(
        &mut self,
        value: ModuleValue,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        self.alloc_value(HeapObject::Module(value), Value::Module, options)
    }

    pub(crate) fn alloc_continuation(
        &mut self,
        value: ContinuationValue,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        self.alloc_value(
            HeapObject::Continuation(Box::new(value)),
            Value::Continuation,
            options,
        )
    }

    fn alloc_value(
        &mut self,
        object: HeapObject,
        build: impl FnOnce(GcRef) -> Value,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        let bytes = object.bytes();
        if options.max_object_bytes.is_some_and(|limit| bytes > limit) {
            return Err(VmError::new(VmErrorKind::HeapObjectTooLarge {
                bytes,
                limit: options.max_object_bytes.unwrap_or_default(),
            }));
        }
        let allocation = self.allocate(bytes);
        let slot = self.free_slots.pop().unwrap_or(self.slots.len());
        let generation = self
            .slots
            .get(slot)
            .map_or(0, |slot| slot.generation.wrapping_add(1));
        if slot == self.slots.len() {
            self.slots
                .push(HeapSlot::live(generation, object, allocation));
        } else if let Some(target) = self.slots.get_mut(slot) {
            *target = HeapSlot::live(generation, object, allocation);
        }
        self.allocated_bytes = self.allocated_bytes.saturating_add(bytes);
        Ok(build(GcRef::new(slot, generation)))
    }
}

impl RuntimeHeap {
    pub(crate) fn contains(&self, reference: GcRef) -> bool {
        self.slot(reference).is_ok()
    }

    pub(super) fn object(&self, reference: GcRef) -> VmResult<&HeapObject> {
        self.slot(reference)?
            .object
            .as_ref()
            .ok_or_else(|| stale_heap_ref(reference))
    }

    pub(super) fn object_mut(&mut self, reference: GcRef) -> VmResult<&mut HeapObject> {
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

    pub(crate) fn sequence(&self, reference: GcRef) -> VmResult<&SequenceValue> {
        match self.object(reference)? {
            HeapObject::Seq(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    pub(crate) fn sequence_mut(&mut self, reference: GcRef) -> VmResult<&mut SequenceValue> {
        match self.object_mut(reference)? {
            HeapObject::Seq(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    pub(crate) fn data(&self, reference: GcRef) -> VmResult<&DataValue> {
        match self.object(reference)? {
            HeapObject::Data(value) => Ok(value),
            _ => Err(invalid_heap_ref(reference, "data")),
        }
    }

    pub(crate) fn data_mut(&mut self, reference: GcRef) -> VmResult<&mut DataValue> {
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

    pub(crate) fn refresh_allocation(&mut self, reference: GcRef) -> VmResult {
        let new_bytes = self.object(reference)?.bytes();
        let old_bytes = self.slot(reference)?.bytes;
        if old_bytes == new_bytes {
            return Ok(());
        }
        if new_bytes > old_bytes {
            self.allocated_bytes = self
                .allocated_bytes
                .saturating_add(new_bytes.saturating_sub(old_bytes));
        } else {
            self.allocated_bytes = self
                .allocated_bytes
                .saturating_sub(old_bytes.saturating_sub(new_bytes));
        }
        self.slot_mut(reference)?.bytes = new_bytes;
        let _ = self.compact_live_allocations();
        Ok(())
    }

    pub(super) fn slot(&self, reference: GcRef) -> VmResult<&HeapSlot> {
        let slot = self
            .slots
            .get(reference.slot())
            .ok_or_else(|| stale_heap_ref(reference))?;
        if slot.is_live_generation(reference) {
            Ok(slot)
        } else {
            Err(stale_heap_ref(reference))
        }
    }

    pub(super) fn slot_mut(&mut self, reference: GcRef) -> VmResult<&mut HeapSlot> {
        let slot = self
            .slots
            .get_mut(reference.slot())
            .ok_or_else(|| stale_heap_ref(reference))?;
        if slot.is_live_generation(reference) {
            Ok(slot)
        } else {
            Err(stale_heap_ref(reference))
        }
    }
}

impl RuntimeHeap {
    pub(super) fn live_object_count(&self) -> usize {
        self.slots
            .iter()
            .filter(|slot| slot.object.is_some())
            .count()
    }

    pub(super) fn allocate(&mut self, bytes: usize) -> HeapAllocation {
        let line_count = bytes.div_ceil(IMMIX_LINE_BYTES).max(1);
        self.allocate_lines(line_count)
    }

    pub(super) fn allocate_lines(&mut self, line_count: usize) -> HeapAllocation {
        if line_count > IMMIX_LINES_PER_BLOCK {
            return HeapAllocation::Large;
        }
        for (block_index, block) in self.blocks.iter_mut().enumerate() {
            if let Some(start_line) = block.reserve_lines(line_count) {
                return HeapAllocation::Immix {
                    block: block_index,
                    start_line,
                    line_count,
                };
            }
        }
        let block_index = self.blocks.len();
        let mut block = ImmixBlock::default();
        let start_line = block.reserve_lines(line_count).unwrap_or_default();
        self.blocks.push(block);
        HeapAllocation::Immix {
            block: block_index,
            start_line,
            line_count,
        }
    }
}

/// Runtime heap collection counters.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapCollectionStats {
    /// Estimated live heap bytes before collection.
    pub before_bytes: usize,
    /// Estimated live heap bytes after collection.
    pub after_bytes: usize,
    /// Tracked heap objects before collection.
    pub before_objects: usize,
    /// Tracked heap objects after collection.
    pub after_objects: usize,
    /// Estimated bytes reclaimed by collection.
    pub reclaimed_bytes: usize,
    /// Heap objects reclaimed by collection.
    pub reclaimed_objects: usize,
    /// Immix blocks with no live lines after collection.
    pub free_blocks: usize,
    /// Live objects assigned new Immix line ranges during compaction.
    pub evacuated_objects: usize,
}
