use std::hint::unreachable_unchecked;

use crate::program::RuntimeSeq2Mutation;
use crate::types::VmResult;
use crate::value::{GcRef, SequenceValue, Value};

use super::super::error::invalid_heap_ref;
use super::super::object::HeapObject;
use super::RuntimeHeap;
use super::util::{arithmetic_overflow, fast_int};
use crate::VmValueKind::Seq;
use crate::error::{VmError, VmErrorKind};

impl RuntimeHeap {
    pub(crate) fn fast_seq2_mutation_2x2_kernel(
        &mut self,
        grid: GcRef,
        init_value: i16,
        update_add: i16,
    ) -> VmResult<i64> {
        self.fast_seq2_mutation_2x2(
            grid,
            RuntimeSeq2Mutation {
                grid_local: 0,
                init_first: 0,
                init_second: 1,
                init_value,
                update_target_first: 1,
                update_target_second: 0,
                update_source_first: 0,
                update_source_second: 1,
                update_add,
                finish_left_first: 0,
                finish_left_second: 1,
                finish_right_first: 1,
                finish_right_second: 0,
            },
        )
    }

    pub(crate) fn fast_seq2_mutation(
        &mut self,
        grid: GcRef,
        plan: RuntimeSeq2Mutation,
    ) -> VmResult<i64> {
        if plan.is_2x2() {
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
        if let HeapObject::PackedSeq2x2(packed) = self.object_mut(grid)? {
            packed.set_cell_for(0, 1, i64::from(plan.init_value));
            let source = packed.cell_for(0, 1);
            let next = source
                .checked_add(i64::from(plan.update_add))
                .ok_or_else(arithmetic_overflow)?;
            packed.set_cell_for(1, 0, next);
            let left = packed.cell_for(0, 1);
            let right = packed.cell_for(1, 0);
            return left.checked_add(right).ok_or_else(arithmetic_overflow);
        }
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
        let row0 = self.sequence_seq_at(grid, 0)?;
        let row1 = self.sequence_seq_at(grid, 1)?;
        Ok((row0, row1))
    }

    fn sequence_items_mut_ptr(&mut self, reference: GcRef) -> VmResult<(*mut Value, usize)> {
        self.mark_write_barrier(reference)?;
        match self.object_mut(reference)? {
            HeapObject::Seq(value) => Ok((value.items.as_mut_ptr(), value.items.len())),
            _ => Err(invalid_heap_ref(reference, "sequence")),
        }
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub(crate) fn fast_seq2_mutation_2x2_pinned(
        &mut self,
        cache: super::Seq2x2ArgCache,
        init_value: i64,
        update_add: i64,
    ) -> VmResult<i64> {
        // SAFETY: cache comes from active pinned lease handle tied to VM borrow.
        let root_slot = unsafe { self.slots.get_unchecked_mut(cache.grid_slot) };
        // SAFETY: pinned lease guarantees live packed grid object for slot lifetime.
        let object = unsafe { root_slot.object.as_mut().unwrap_unchecked() };
        let HeapObject::PackedSeq2x2(packed) = object else {
            // SAFETY: packed lease handle only binds packed grid slots.
            unsafe { unreachable_unchecked() };
        };
        packed.set_cell_for(0, 1, init_value);
        let source = packed.cell_for(0, 1);
        let next = source
            .checked_add(update_add)
            .ok_or_else(arithmetic_overflow)?;
        packed.set_cell_for(1, 0, next);
        let left = packed.cell_for(0, 1);
        let right = packed.cell_for(1, 0);
        left.checked_add(right).ok_or_else(arithmetic_overflow)
    }
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
