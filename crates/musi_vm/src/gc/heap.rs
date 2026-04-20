use std::cell::Cell;
use std::marker::PhantomData;
use std::sync::atomic::{AtomicU64, Ordering};

use music_seam::TypeId;
use music_term::SyntaxTerm;

use crate::VmValueKind::{Int, Nat, Seq};
use crate::error::{VmError, VmErrorKind};
use crate::program::RuntimeSeq2Mutation;
use crate::types::VmResult;
use crate::value::{
    ClosureValue, ContinuationValue, DataValue, GcRef, IsolateId, ModuleValue, SequenceValue,
    Value, ValueList,
};

use super::error::{invalid_heap_ref, stale_heap_ref};
use super::object::HeapObject;
use super::space::{
    HeapAllocation, HeapSlot, HeapSpace, IMMIX_CARD_LINES, IMMIX_CARDS_PER_BLOCK, IMMIX_LINE_BYTES,
    IMMIX_LINES_PER_BLOCK, ImmixBlock,
};

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
    pub(super) current_young_block: Option<usize>,
    pub(super) current_mature_block: Option<usize>,
    pub(super) mature_card_table: Vec<[bool; IMMIX_CARDS_PER_BLOCK]>,
    pub(super) remembered_large_slots: Vec<usize>,
    pub(super) allocated_bytes: usize,
    pub(super) young_allocated_bytes: usize,
    single_thread_marker: PhantomData<Cell<()>>,
}

pub(super) const YOUNG_TARGET_BYTES: usize = 256 * 1024;
pub(super) const LARGE_PROMOTE_BYTES: usize = 4096;
pub(super) const PROMOTE_SURVIVE_THRESHOLD: u8 = 2;

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
            current_young_block: None,
            current_mature_block: None,
            mature_card_table: Vec::new(),
            remembered_large_slots: Vec::new(),
            allocated_bytes: 0,
            young_allocated_bytes: 0,
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

    pub(crate) fn alloc_pair_sequence(
        &mut self,
        ty: TypeId,
        first: Value,
        second: Value,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        let mut items = ValueList::new();
        items.push(first);
        items.push(second);
        self.alloc_sequence(SequenceValue::new(ty, items), options)
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
        let space = HeapSpace::Young;
        let allocation = self.allocate_in_space(space, bytes);
        let slot = self.free_slots.pop().unwrap_or(self.slots.len());
        let generation = self
            .slots
            .get(slot)
            .map_or(0, |slot| slot.generation.wrapping_add(1));
        if slot == self.slots.len() {
            self.slots.push(HeapSlot::live_with_bytes(
                generation, space, object, allocation, bytes,
            ));
        } else if let Some(target) = self.slots.get_mut(slot) {
            *target = HeapSlot::live_with_bytes(generation, space, object, allocation, bytes);
        }
        self.allocated_bytes = self.allocated_bytes.saturating_add(bytes);
        self.young_allocated_bytes = self.young_allocated_bytes.saturating_add(bytes);
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
        self.mark_write_barrier(reference)?;
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

    pub(crate) fn mark_write_barrier(&mut self, reference: GcRef) -> VmResult {
        let (space, allocation) = {
            let slot = self.slot(reference)?;
            (slot.space, slot.allocation)
        };
        if space != HeapSpace::Mature {
            return Ok(());
        }
        self.mark_allocation_cards(allocation, reference.slot());
        Ok(())
    }

    fn mark_allocation_cards(&mut self, allocation: HeapAllocation, slot_index: usize) {
        match allocation {
            HeapAllocation::Immix {
                space: HeapSpace::Mature,
                block,
                start_line,
                line_count,
            } => {
                let Some(card_index) = self.mature_card_index(block) else {
                    return;
                };
                if let Some(cards) = self.mature_card_table.get_mut(card_index) {
                    let first = start_line / IMMIX_CARD_LINES;
                    let end_line = start_line.saturating_add(line_count).saturating_sub(1);
                    let last = end_line / IMMIX_CARD_LINES;
                    for card in cards
                        .iter_mut()
                        .take(last.min(IMMIX_CARDS_PER_BLOCK.saturating_sub(1)) + 1)
                        .skip(first)
                    {
                        *card = true;
                    }
                }
            }
            HeapAllocation::Large {
                space: HeapSpace::Mature,
            } => {
                if !self.remembered_large_slots.contains(&slot_index) {
                    self.remembered_large_slots.push(slot_index);
                }
            }
            HeapAllocation::Immix {
                space: HeapSpace::Young,
                ..
            }
            | HeapAllocation::Large {
                space: HeapSpace::Young,
            } => {}
        }
    }

    pub(super) fn mature_card_index(&self, block_index: usize) -> Option<usize> {
        let block = self.blocks.get(block_index)?;
        if block.space != HeapSpace::Mature {
            return None;
        }
        let mature_before = self.blocks[..block_index]
            .iter()
            .filter(|candidate| candidate.space == HeapSpace::Mature)
            .count();
        Some(mature_before)
    }
}

impl RuntimeHeap {
    pub(super) fn live_object_count(&self) -> usize {
        self.slots
            .iter()
            .filter(|slot| slot.object.is_some())
            .count()
    }

    pub(super) fn allocate_in_space(&mut self, space: HeapSpace, bytes: usize) -> HeapAllocation {
        let line_count = bytes.div_ceil(IMMIX_LINE_BYTES).max(1);
        self.allocate_lines_excluding(space, line_count, &[])
    }

    pub(super) fn allocate_lines_excluding(
        &mut self,
        space: HeapSpace,
        line_count: usize,
        excluded_blocks: &[usize],
    ) -> HeapAllocation {
        if line_count > IMMIX_LINES_PER_BLOCK {
            return HeapAllocation::Large { space };
        }
        let current_block = match space {
            HeapSpace::Young => self.current_young_block,
            HeapSpace::Mature => self.current_mature_block,
        };
        if let Some(block_index) = current_block
            && !excluded_blocks.contains(&block_index)
            && let Some(block) = self.blocks.get_mut(block_index)
            && block.space == space
            && let Some(start_line) = block.reserve_lines(line_count)
        {
            return HeapAllocation::Immix {
                space,
                block: block_index,
                start_line,
                line_count,
            };
        }
        for (block_index, block) in self.blocks.iter_mut().enumerate() {
            if excluded_blocks.contains(&block_index) {
                continue;
            }
            if block.space != space {
                continue;
            }
            if let Some(start_line) = block.reserve_lines(line_count) {
                match space {
                    HeapSpace::Young => self.current_young_block = Some(block_index),
                    HeapSpace::Mature => self.current_mature_block = Some(block_index),
                }
                return HeapAllocation::Immix {
                    space,
                    block: block_index,
                    start_line,
                    line_count,
                };
            }
        }
        let block_index = self.blocks.len();
        let mut block = ImmixBlock::new(space);
        let start_line = block.reserve_lines(line_count).unwrap_or_default();
        self.blocks.push(block);
        if space == HeapSpace::Mature {
            self.mature_card_table.push([false; IMMIX_CARDS_PER_BLOCK]);
        }
        match space {
            HeapSpace::Young => self.current_young_block = Some(block_index),
            HeapSpace::Mature => self.current_mature_block = Some(block_index),
        }
        HeapAllocation::Immix {
            space,
            block: block_index,
            start_line,
            line_count,
        }
    }

    pub(super) fn mark_allocation(&mut self, allocation: HeapAllocation) {
        let HeapAllocation::Immix {
            space: _,
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
            space: _,
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
            .current_young_block
            .is_some_and(|block| self.blocks.get(block).is_none_or(ImmixBlock::is_free))
        {
            self.current_young_block = None;
        }
        if self
            .current_mature_block
            .is_some_and(|block| self.blocks.get(block).is_none_or(ImmixBlock::is_free))
        {
            self.current_mature_block = None;
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

    pub(crate) const fn should_collect_young(&self) -> bool {
        self.young_allocated_bytes >= YOUNG_TARGET_BYTES
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

impl RuntimeHeap {
    pub(crate) fn fast_seq2_mutation(
        &mut self,
        grid: GcRef,
        plan: RuntimeSeq2Mutation,
    ) -> VmResult<i64> {
        if is_2x2_seq2_plan(plan) {
            return self.fast_seq2_mutation_2x2(grid, plan);
        }
        let (init_row, update_source_row, update_target_row, finish_left_row, finish_right_row) = {
            let outer = self.sequence(grid)?;
            (
                seq_ref_at(outer, plan.init_first)?,
                seq_ref_at(outer, plan.update_source_first)?,
                seq_ref_at(outer, plan.update_target_first)?,
                seq_ref_at(outer, plan.finish_left_first)?,
                seq_ref_at(outer, plan.finish_right_first)?,
            )
        };

        let (init_items, init_len) = self.sequence_items_mut_ptr(init_row)?;
        let (source_items, source_len) = self.sequence_items_mut_ptr(update_source_row)?;
        let (target_items, target_len) = self.sequence_items_mut_ptr(update_target_row)?;
        let (left_items, left_len) = self.sequence_items_mut_ptr(finish_left_row)?;
        let (right_items, right_len) = self.sequence_items_mut_ptr(finish_right_row)?;
        let init_index = fast_sequence_index(plan.init_second, init_len)?;
        let source_index = fast_sequence_index(plan.update_source_second, source_len)?;
        let target_index = fast_sequence_index(plan.update_target_second, target_len)?;
        let left_index = fast_sequence_index(plan.finish_left_second, left_len)?;
        let right_index = fast_sequence_index(plan.finish_right_second, right_len)?;

        // SAFETY: each pointer comes from a live same-isolate sequence slot, and each index
        // is checked against the matching sequence length before use. Raw pointers allow
        // aliased rows while preserving source-order reads/writes.
        unsafe {
            write_value_at(
                init_items,
                init_index,
                Value::Int(i64::from(plan.init_value)),
            );
        };
        // SAFETY: source_index checked against source_len above.
        let source = fast_int(unsafe { value_at(source_items, source_index) })?;
        let next = source
            .checked_add(i64::from(plan.update_add))
            .ok_or_else(arithmetic_overflow)?;
        // SAFETY: target_index checked against target_len above.
        unsafe { write_value_at(target_items, target_index, Value::Int(next)) };
        // SAFETY: finish indices checked against their row lengths above.
        let left = fast_int(unsafe { value_at(left_items, left_index) })?;
        let right = fast_int(unsafe { value_at(right_items, right_index) })?;
        left.checked_add(right).ok_or_else(arithmetic_overflow)
    }

    fn fast_seq2_mutation_2x2(&mut self, grid: GcRef, plan: RuntimeSeq2Mutation) -> VmResult<i64> {
        let (row0, row1) = self.seq2_rows_2x2(grid)?;
        let (row0_items, row0_len) = self.sequence_items_mut_ptr(row0)?;
        let (row1_items, row1_len) = self.sequence_items_mut_ptr(row1)?;
        let row0_slot = fast_sequence_index(1, row0_len)?;
        let row1_slot = fast_sequence_index(0, row1_len)?;
        // SAFETY: slots are validated against row lengths above.
        unsafe {
            write_value_at(
                row0_items,
                row0_slot,
                Value::Int(i64::from(plan.init_value)),
            );
        }
        let source = fast_int(
            // SAFETY: slot is validated against row length above.
            unsafe { value_at(row0_items, row0_slot) },
        )?;
        let next = source
            .checked_add(i64::from(plan.update_add))
            .ok_or_else(arithmetic_overflow)?;
        // SAFETY: slot is validated against row length above.
        unsafe { write_value_at(row1_items, row1_slot, Value::Int(next)) };
        let left = fast_int(
            // SAFETY: slot is validated against row length above.
            unsafe { value_at(row0_items, row0_slot) },
        )?;
        let right = fast_int(
            // SAFETY: slot is validated against row length above.
            unsafe { value_at(row1_items, row1_slot) },
        )?;
        left.checked_add(right).ok_or_else(arithmetic_overflow)
    }

    fn seq2_rows_2x2(&self, grid: GcRef) -> VmResult<(GcRef, GcRef)> {
        let outer = self.sequence(grid)?;
        let first = outer.items.first().ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidSequenceIndex {
                index: 0,
                len: outer.items.len(),
            })
        })?;
        let second = outer.items.get(1).ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidSequenceIndex {
                index: 1,
                len: outer.items.len(),
            })
        })?;
        let Value::Seq(row0) = first else {
            return Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: Seq,
                found: first.kind(),
            }));
        };
        let Value::Seq(row1) = second else {
            return Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: Seq,
                found: second.kind(),
            }));
        };
        Ok((*row0, *row1))
    }

    fn sequence_items_mut_ptr(&mut self, reference: GcRef) -> VmResult<(*mut Value, usize)> {
        self.mark_write_barrier(reference)?;
        match self.object_mut(reference)? {
            HeapObject::Seq(value) => Ok((value.items.as_mut_ptr(), value.items.len())),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }
}

const fn is_2x2_seq2_plan(plan: RuntimeSeq2Mutation) -> bool {
    plan.init_first == 0
        && plan.init_second == 1
        && plan.update_target_first == 1
        && plan.update_target_second == 0
        && plan.update_source_first == 0
        && plan.update_source_second == 1
        && plan.finish_left_first == 0
        && plan.finish_left_second == 1
        && plan.finish_right_first == 1
        && plan.finish_right_second == 0
}

fn fast_sequence_index(index: i16, len: usize) -> VmResult<usize> {
    let Ok(slot) = usize::try_from(index) else {
        return Err(VmError::new(VmErrorKind::InvalidSequenceIndex {
            index: i64::from(index),
            len,
        }));
    };
    if slot >= len {
        return Err(VmError::new(VmErrorKind::InvalidSequenceIndex {
            index: i64::from(index),
            len,
        }));
    }
    Ok(slot)
}

fn seq_ref_at(sequence: &SequenceValue, index: i16) -> VmResult<GcRef> {
    let index = fast_sequence_index(index, sequence.items.len())?;
    // SAFETY: index is checked against sequence length directly above.
    match unsafe { sequence.items.get_unchecked(index) } {
        Value::Seq(reference) => Ok(*reference),
        other => Err(VmError::new(VmErrorKind::InvalidValueKind {
            expected: Seq,
            found: other.kind(),
        })),
    }
}

fn fast_int(value: &Value) -> VmResult<i64> {
    match value {
        Value::Int(value) => Ok(*value),
        Value::Nat(value) => i64::try_from(*value).map_err(|_| {
            VmError::new(VmErrorKind::InvalidValueKind {
                expected: Int,
                found: Nat,
            })
        }),
        other => Err(VmError::new(VmErrorKind::InvalidValueKind {
            expected: Int,
            found: other.kind(),
        })),
    }
}

const unsafe fn value_at<'a>(items: *const Value, index: usize) -> &'a Value {
    // SAFETY: caller guarantees index is in bounds for `items`.
    let slot = unsafe { items.add(index) };
    // SAFETY: caller guarantees the selected element is live for the returned borrow.
    unsafe { &*slot }
}

const unsafe fn write_value_at(items: *mut Value, index: usize, value: Value) {
    // SAFETY: caller guarantees index is in bounds for `items`.
    let slot = unsafe { items.add(index) };
    // SAFETY: caller guarantees unique write access for the selected element.
    unsafe { slot.write(value) };
}

fn arithmetic_overflow() -> VmError {
    VmError::new(VmErrorKind::ArithmeticFailed {
        detail: "signed integer overflow".into(),
    })
}
