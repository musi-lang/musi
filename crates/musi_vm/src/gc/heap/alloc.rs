use music_seam::TypeId;
use music_term::SyntaxTerm;

use crate::types::VmResult;
use crate::value::{
    ClosureValue, ContinuationValue, DataValue, GcRef, I64ArrayValue, ModuleValue, SequenceValue,
    Value, ValueList,
};

use super::super::object::HeapObject;
use super::super::space::{HeapAllocation, HeapSlot, HeapSpace};
use super::{HeapOptions, RuntimeHeap};
use crate::error::{VmError, VmErrorKind};

const SEQ8_FAST_POOL_SLOTS: usize = 4096;

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
        let value = match value.take_i64_array_cells() {
            Ok((ty, cells)) => {
                let len = cells.len();
                let buffer = self
                    .alloc_object_ref(HeapObject::I64Array(I64ArrayValue::new(cells)), options)?;
                SequenceValue::from_i64_array(ty, buffer, len)
            }
            Err(value) => value,
        };
        self.alloc_value(HeapObject::Seq(value), Value::Seq, options)
    }

    pub(crate) fn alloc_i64_array_sequence(
        &mut self,
        ty: TypeId,
        cells: [i64; 8],
        options: &HeapOptions,
    ) -> VmResult<(Value, GcRef)> {
        let len = cells.len();
        let buffer =
            self.alloc_object_ref(HeapObject::I64Array(I64ArrayValue::new(cells)), options)?;
        let value = self.alloc_sequence_with_i64_array(ty, buffer, len, options)?;
        Ok((value, buffer))
    }

    pub(crate) fn alloc_shared_i64_array_sequence(
        &mut self,
        ty: TypeId,
        cells: [i64; 8],
        options: &HeapOptions,
    ) -> VmResult<(Value, GcRef)> {
        let len = cells.len();
        let buffer =
            self.alloc_object_ref(HeapObject::I64Array(I64ArrayValue::shared(cells)), options)?;
        let value = self.alloc_sequence_with_i64_array(ty, buffer, len, options)?;
        Ok((value, buffer))
    }

    pub(crate) fn alloc_sequence_with_i64_array(
        &mut self,
        ty: TypeId,
        buffer: GcRef,
        len: usize,
        options: &HeapOptions,
    ) -> VmResult<Value> {
        let reference = self.alloc_unlined_object_ref(
            HeapObject::Seq(SequenceValue::from_i64_array(ty, buffer, len)),
            options,
        )?;
        Ok(Value::Seq(reference))
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub(crate) fn alloc_sequence_with_i64_array_unchecked(
        &mut self,
        ty: TypeId,
        buffer: GcRef,
        len: usize,
    ) -> Value {
        let reference = self.alloc_unlined_object_ref_unchecked(
            HeapObject::Seq(SequenceValue::from_i64_array(ty, buffer, len)),
            HeapObject::sequence_i64_array_bytes(),
        );
        Value::Seq(reference)
    }

    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub(crate) fn alloc_sequence_with_i64_array_pooled_unchecked(
        &mut self,
        ty: TypeId,
        buffer: GcRef,
        len: usize,
    ) -> Value {
        if self.seq8_fast_slots.len() < SEQ8_FAST_POOL_SLOTS {
            let value = self.alloc_sequence_with_i64_array_unchecked(ty, buffer, len);
            if let Value::Seq(reference) = value {
                self.seq8_fast_slots.push(reference.slot());
            }
            return value;
        }
        let slot = self.seq8_fast_slots[self.seq8_fast_cursor];
        self.seq8_fast_cursor = (self.seq8_fast_cursor + 1) & (SEQ8_FAST_POOL_SLOTS - 1);
        self.seq8_fast_generation = self.seq8_fast_generation.wrapping_add(1);
        let generation = self.seq8_fast_generation;
        let target = &mut self.slots[slot];
        let bytes = HeapObject::sequence_i64_array_bytes();
        let was_free = target.object.is_none();
        target.generation = generation;
        target.object = Some(HeapObject::Seq(SequenceValue::from_i64_array(
            ty, buffer, len,
        )));
        target.survive_count = 0;
        target.is_marked = false;
        if was_free {
            self.allocated_bytes = self.allocated_bytes.saturating_add(bytes);
            self.young_allocated_bytes = self.young_allocated_bytes.saturating_add(bytes);
        }
        Value::Seq(GcRef::new(self.isolate, slot, generation))
    }

    #[must_use]
    pub(crate) const fn has_full_seq8_fast_pool(&self) -> bool {
        self.seq8_fast_slots.len() == SEQ8_FAST_POOL_SLOTS
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

    fn alloc_unlined_object_ref(
        &mut self,
        object: HeapObject,
        options: &HeapOptions,
    ) -> VmResult<GcRef> {
        let bytes = object.bytes();
        if options.max_object_bytes.is_some_and(|limit| bytes > limit) {
            return Err(VmError::new(VmErrorKind::HeapObjectTooLarge {
                bytes,
                limit: options.max_object_bytes.unwrap_or_default(),
            }));
        }
        let space = HeapSpace::Young;
        let allocation = HeapAllocation::Large { space };
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

    #[allow(clippy::inline_always)]
    #[inline(always)]
    fn alloc_unlined_object_ref_unchecked(&mut self, object: HeapObject, bytes: usize) -> GcRef {
        let space = HeapSpace::Young;
        let allocation = HeapAllocation::Large { space };
        let slot = self.free_slots.pop().unwrap_or(self.slots.len());
        let generation = self
            .slots
            .get(slot)
            .map_or(0, |slot| slot.generation.wrapping_add(1));
        if slot == self.slots.len() {
            self.slots.push(HeapSlot::live_with_bytes(
                generation, space, object, allocation, bytes,
            ));
        } else {
            self.slots[slot] =
                HeapSlot::live_with_bytes(generation, space, object, allocation, bytes);
        }
        self.allocated_bytes = self.allocated_bytes.saturating_add(bytes);
        self.young_allocated_bytes = self.young_allocated_bytes.saturating_add(bytes);
        GcRef::new(self.isolate, slot, generation)
    }
}
