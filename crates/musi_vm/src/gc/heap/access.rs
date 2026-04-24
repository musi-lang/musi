use crate::error::{VmError, VmErrorKind};
use crate::types::VmResult;
use crate::value::{ClosureValue, ContinuationValue, DataValue, GcRef, ModuleValue, SequenceValue};
use music_term::SyntaxTerm;
use std::ptr::from_mut;

use super::super::error::{invalid_heap_ref, stale_heap_ref};
use super::super::object::HeapObject;
use super::super::space::HeapSlot;
use super::{RuntimeHeap, Seq2x2ArgGuard, SequenceGuard};

impl RuntimeHeap {
    pub(crate) fn validate_ref(&self, reference: GcRef) -> VmResult {
        self.slot(reference).map(|_| ())
    }

    pub(crate) fn bind_seq2x2_arg(&self, reference: GcRef) -> VmResult<Seq2x2ArgGuard> {
        if self.sequence_len(reference)? != 2 {
            return Err(invalid_heap_ref(reference, "sequence(len=2)"));
        }
        let row0 = self.sequence_seq_at(reference, 0)?;
        let row1 = self.sequence_seq_at(reference, 1)?;
        if self.sequence_int_pair(row0).is_err() || self.sequence_int_pair(row1).is_err() {
            return Err(invalid_heap_ref(reference, "sequence([2][2]Int)"));
        }
        Ok(Seq2x2ArgGuard {
            grid: self.sequence_guard(reference)?,
            row0: self.sequence_guard(row0)?,
            row1: self.sequence_guard(row1)?,
        })
    }

    pub(crate) fn sequence_guard(&self, reference: GcRef) -> VmResult<SequenceGuard> {
        let slot = self.slot(reference)?;
        let HeapObject::Seq(sequence) = slot
            .object
            .as_ref()
            .ok_or_else(|| stale_heap_ref(reference))?
        else {
            return Err(invalid_heap_ref(reference, "sequence"));
        };
        Ok(SequenceGuard {
            slot: reference.slot(),
            generation: slot.generation,
            layout_version: sequence.layout_version(),
        })
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

    pub(crate) fn guarded_sequence_mut(
        &mut self,
        guard: SequenceGuard,
    ) -> VmResult<&mut SequenceValue> {
        let Some(slot) = self.slots.get_mut(guard.slot) else {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "stale sequence guard slot".into(),
            }));
        };
        if slot.generation != guard.generation {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "stale sequence guard generation".into(),
            }));
        }
        let Some(object) = slot.object.as_mut() else {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "stale sequence guard object".into(),
            }));
        };
        let HeapObject::Seq(sequence) = object else {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "sequence guard not sequence".into(),
            }));
        };
        if sequence.layout_version() != guard.layout_version {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "sequence guard layout changed".into(),
            }));
        }
        Ok(sequence)
    }

    pub(crate) fn guarded_int_pair_ptr(&mut self, guard: SequenceGuard) -> VmResult<*mut [i64; 2]> {
        let sequence = self.guarded_sequence_mut(guard)?;
        let pair = sequence.int_pair_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "sequence guard not int pair".into(),
            })
        })?;
        Ok(from_mut(pair))
    }
}
