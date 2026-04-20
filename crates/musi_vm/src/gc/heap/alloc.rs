use music_seam::TypeId;
use music_term::SyntaxTerm;

use crate::types::VmResult;
use crate::value::{
    ClosureValue, ContinuationValue, DataValue, GcRef, ModuleValue, SequenceValue, Value, ValueList,
};

use super::super::error::invalid_heap_ref;
use super::super::object::{HeapObject, PackedSeq2x2, PackedSeq2x2Row};
use super::super::space::{HeapSlot, HeapSpace};
use super::util::fast_int;
use super::{HeapOptions, RuntimeHeap};
use crate::error::{VmError, VmErrorKind};

impl RuntimeHeap {
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
        if let Some(packed) = self.try_alloc_packed_seq2x2(&value, options)? {
            return Ok(Value::Seq(packed));
        }
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
        let reference = self.alloc_object_ref(object, options)?;
        Ok(build(reference))
    }

    fn alloc_object_ref(&mut self, object: HeapObject, options: &HeapOptions) -> VmResult<GcRef> {
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
        Ok(GcRef::new(self.isolate, slot, generation))
    }

    fn try_alloc_packed_seq2x2(
        &mut self,
        value: &SequenceValue,
        options: &HeapOptions,
    ) -> VmResult<Option<GcRef>> {
        if value.items.len() != 2 {
            return Ok(None);
        }
        let Value::Seq(first_row) = value.items[0] else {
            return Ok(None);
        };
        let Value::Seq(second_row) = value.items[1] else {
            return Ok(None);
        };
        let first_cells = self.seq_len2_int_cells(first_row)?;
        let second_cells = self.seq_len2_int_cells(second_row)?;
        let row_map = if first_row == second_row {
            [0, 0]
        } else {
            [0, 1]
        };
        let dead = GcRef::new(self.isolate, usize::MAX, u32::MAX);
        let grid_ref = self.alloc_object_ref(
            HeapObject::PackedSeq2x2(PackedSeq2x2 {
                ty: value.ty,
                cells: [first_cells, second_cells],
                row_refs: [dead; 2],
                row_map,
            }),
            options,
        )?;
        let row0_ref = self.alloc_object_ref(
            HeapObject::PackedSeq2x2Row(PackedSeq2x2Row {
                ty: value.ty,
                grid: grid_ref,
                logical_row: 0,
            }),
            options,
        )?;
        let row1_ref = if first_row == second_row {
            row0_ref
        } else {
            self.alloc_object_ref(
                HeapObject::PackedSeq2x2Row(PackedSeq2x2Row {
                    ty: value.ty,
                    grid: grid_ref,
                    logical_row: 1,
                }),
                options,
            )?
        };
        let HeapObject::PackedSeq2x2(grid) = self.object_mut(grid_ref)? else {
            return Ok(None);
        };
        grid.row_refs = [row0_ref, row1_ref];
        Ok(Some(grid_ref))
    }

    fn seq_len2_int_cells(&self, reference: GcRef) -> VmResult<[i64; 2]> {
        let sequence = self.sequence(reference)?;
        if sequence.items.len() != 2 {
            return Err(invalid_heap_ref(reference, "sequence(len=2)"));
        }
        let first = fast_int(&sequence.items[0])?;
        let second = fast_int(&sequence.items[1])?;
        Ok([first, second])
    }
}
