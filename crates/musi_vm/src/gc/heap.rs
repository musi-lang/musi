use std::cell::Cell;
use std::marker::PhantomData;
use std::sync::atomic::{AtomicU64, Ordering};

use music_term::SyntaxTerm;

use crate::error::{VmError, VmErrorKind};
use crate::types::VmResult;
use crate::value::{
    ClosureValue, ContinuationValue, DataValue, GcRef, IsolateId, ModuleValue, SequenceValue, Value,
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
    pub(super) isolate: IsolateId,
    pub(super) slots: Vec<HeapSlot>,
    pub(super) free_slots: Vec<usize>,
    pub(super) blocks: Vec<ImmixBlock>,
    pub(super) current_block: Option<usize>,
    pub(super) allocated_bytes: usize,
    single_thread_marker: PhantomData<Cell<()>>,
}

static NEXT_ISOLATE_ID: AtomicU64 = AtomicU64::new(1);

impl Default for IsolateId {
    fn default() -> Self {
        Self::new(NEXT_ISOLATE_ID.fetch_add(1, Ordering::Relaxed))
    }
}

impl RuntimeHeap {
    #[must_use]
    pub fn new() -> Self {
        Self {
            isolate: IsolateId::default(),
            slots: Vec::new(),
            free_slots: Vec::new(),
            blocks: Vec::new(),
            current_block: None,
            allocated_bytes: 0,
            single_thread_marker: PhantomData,
        }
    }

    #[must_use]
    pub(crate) const fn isolate(&self) -> IsolateId {
        self.isolate
    }

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
        Ok(build(GcRef::new(self.isolate, slot, generation)))
    }
}

impl RuntimeHeap {
    pub(crate) fn validate_ref(&self, reference: GcRef) -> VmResult {
        self.slot(reference).map(|_| ())
    }

    fn checked_slot_index(&self, reference: GcRef) -> VmResult<usize> {
        if reference.isolate() != self.isolate {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "cross-isolate heap reference".into(),
            }));
        }
        Ok(reference.slot())
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

    pub(super) fn slot(&self, reference: GcRef) -> VmResult<&HeapSlot> {
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

    pub(super) fn slot_mut(&mut self, reference: GcRef) -> VmResult<&mut HeapSlot> {
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
        self.allocate_lines_excluding(line_count, &[])
    }

    pub(super) fn allocate_lines_excluding(
        &mut self,
        line_count: usize,
        excluded_blocks: &[usize],
    ) -> HeapAllocation {
        if line_count > IMMIX_LINES_PER_BLOCK {
            return HeapAllocation::Large;
        }
        if let Some(block_index) = self.current_block
            && !excluded_blocks.contains(&block_index)
            && let Some(block) = self.blocks.get_mut(block_index)
            && let Some(start_line) = block.reserve_lines(line_count)
        {
            return HeapAllocation::Immix {
                block: block_index,
                start_line,
                line_count,
            };
        }
        for (block_index, block) in self.blocks.iter_mut().enumerate() {
            if excluded_blocks.contains(&block_index) {
                continue;
            }
            if let Some(start_line) = block.reserve_lines(line_count) {
                self.current_block = Some(block_index);
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
        self.current_block = Some(block_index);
        HeapAllocation::Immix {
            block: block_index,
            start_line,
            line_count,
        }
    }

    pub(super) fn mark_allocation(&mut self, allocation: HeapAllocation) {
        let HeapAllocation::Immix {
            block,
            start_line,
            line_count,
        } = allocation
        else {
            return;
        };
        if let Some(block) = self.blocks.get_mut(block) {
            block.mark_lines(start_line, line_count);
        }
    }

    pub(super) fn release_allocation(&mut self, allocation: HeapAllocation) {
        let HeapAllocation::Immix {
            block,
            start_line,
            line_count,
        } = allocation
        else {
            return;
        };
        if let Some(block) = self.blocks.get_mut(block) {
            block.release_lines(start_line, line_count);
        }
    }

    pub(super) fn finish_line_sweep(&mut self) {
        for block in &mut self.blocks {
            block.finish_collection();
        }
        if self
            .current_block
            .is_some_and(|block| self.blocks.get(block).is_none_or(ImmixBlock::is_free))
        {
            self.current_block = None;
        }
    }

    pub(super) fn fragmented_blocks(&self) -> Vec<usize> {
        self.blocks
            .iter()
            .enumerate()
            .filter_map(|(index, block)| {
                let live = block.live_lines();
                let free = block.free_lines();
                (live > 0 && free > live).then_some(index)
            })
            .collect()
    }

    pub(super) fn free_blocks(&self) -> usize {
        self.blocks.iter().filter(|block| block.is_free()).count()
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
