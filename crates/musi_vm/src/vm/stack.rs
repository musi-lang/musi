use std::iter::repeat_n;

use crate::{VmIndexSpace, VmStackKind};
use music_seam::{Instruction, ProcedureId};

use super::{Value, ValueList, VmError, VmErrorKind, VmResult};

use super::Vm;
use super::state::{CallFrame, StepOutcome};

impl Vm {
    pub(crate) fn next_instruction(&mut self) -> VmResult<Instruction> {
        self.before_instruction()?;
        let (module_slot, procedure, ip) = {
            let frame = self.frames.last().ok_or_else(|| {
                VmError::new(VmErrorKind::StackEmpty {
                    stack: VmStackKind::CallFrame,
                })
            })?;
            (frame.module_slot, frame.procedure, frame.ip)
        };
        let loaded_procedure = self
            .module(module_slot)?
            .program
            .loaded_procedure(procedure)?;
        let instruction = loaded_procedure
            .instructions
            .get(ip)
            .cloned()
            .ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidBranchTarget {
                    procedure: loaded_procedure.name.clone(),
                    label: Some(u16::MAX),
                    index: None,
                    len: None,
                })
            })?;
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        frame.ip = frame.ip.saturating_add(1);
        Ok(instruction)
    }

    pub(crate) fn push_frame(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: ValueList,
    ) -> VmResult {
        if self
            .options
            .stack_frame_limit
            .is_some_and(|limit| self.frames.len() >= limit)
        {
            return Err(VmError::new(VmErrorKind::StackFrameLimitExceeded {
                frames: self.frames.len().saturating_add(1),
                limit: self.options.stack_frame_limit.unwrap_or_default(),
            }));
        }
        let loaded = self
            .module(module_slot)?
            .program
            .loaded_procedure(procedure)?
            .clone();
        let param_count = usize::from(loaded.params);
        if args.len() != param_count {
            return Err(VmError::new(VmErrorKind::CallArityMismatch {
                callee: loaded.name,
                expected: param_count,
                found: args.len(),
            }));
        }

        let local_count = usize::from(loaded.locals.max(loaded.params));
        let mut locals = repeat_n(Value::Unit, local_count).collect::<ValueList>();
        for (slot, arg) in args.into_iter().enumerate() {
            if let Some(local) = locals.get_mut(slot) {
                *local = arg;
            }
        }
        self.frames.push(CallFrame::new(
            module_slot,
            procedure,
            locals,
            ValueList::new(),
        ));
        self.after_value_mutation()?;
        Ok(())
    }

    pub(crate) fn return_from_frame(&mut self) -> VmResult<StepOutcome> {
        let result = if self
            .frames
            .last()
            .is_some_and(|frame| frame.stack.is_empty())
        {
            Value::Unit
        } else {
            self.pop_value()?
        };
        let _ = self.frames.pop();
        if self.frames.is_empty() {
            Ok(StepOutcome::Return(result))
        } else {
            self.push_value(result)?;
            Ok(StepOutcome::Continue)
        }
    }

    pub(crate) fn push_value(&mut self, value: Value) -> VmResult {
        self.observe_heap_value(&value)?;
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        frame.stack.push(value);
        self.after_value_mutation()?;
        Ok(())
    }

    pub(crate) fn pop_value(&mut self) -> VmResult<Value> {
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        frame.stack.pop().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::Operand,
            })
        })
    }

    pub(crate) fn pop_args(&mut self, len: usize) -> VmResult<ValueList> {
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        if frame.stack.len() < len {
            return Err(VmError::new(VmErrorKind::OperandCountMismatch {
                needed: len,
                available: frame.stack.len(),
            }));
        }
        let split = frame.stack.len().saturating_sub(len);
        Ok(frame.stack.drain(split..).collect())
    }

    pub(crate) fn pop_seq_args(&mut self) -> VmResult<ValueList> {
        let value = self.pop_value()?;
        let seq = Self::expect_seq(value)?;
        Ok(self.heap.sequence(seq)?.items.clone())
    }

    pub(crate) fn pop_index_list(&mut self, len: i16) -> VmResult<smallvec::SmallVec<[i64; 4]>> {
        let len = usize::try_from(len).map_err(|_| {
            VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "index count is negative".into(),
            })
        })?;
        self.pop_args(len)?
            .into_iter()
            .map(|value| Self::expect_int(&value))
            .collect()
    }

    fn checked_local_slot(&self, slot: u16) -> VmResult<usize> {
        let Some(frame) = self.frames.last() else {
            return Err(VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            }));
        };
        let index = usize::from(slot);
        if index < frame.locals.len() {
            Ok(index)
        } else {
            Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Local,
                owner: None,
                index: i64::from(slot),
                len: frame.locals.len(),
            }))
        }
    }

    pub(crate) fn local(&self, slot: u16) -> VmResult<&Value> {
        let index = self.checked_local_slot(slot)?;
        Ok(&self.frames.last().expect("checked local frame").locals[index])
    }

    pub(crate) fn local_mut(&mut self, slot: u16) -> VmResult<&mut Value> {
        let index = self.checked_local_slot(slot)?;
        Ok(&mut self.frames.last_mut().expect("checked local frame").locals[index])
    }

    pub(crate) fn jump_to(&mut self, label: u16) -> VmResult {
        let (module_slot, procedure) = {
            let frame = self.frames.last().ok_or_else(|| {
                VmError::new(VmErrorKind::StackEmpty {
                    stack: VmStackKind::CallFrame,
                })
            })?;
            (frame.module_slot, frame.procedure)
        };
        let loaded_procedure = self
            .module(module_slot)?
            .program
            .loaded_procedure(procedure)?;
        let ip = *loaded_procedure.labels.get(&label).ok_or_else(|| {
            VmError::new(VmErrorKind::InvalidBranchTarget {
                procedure: loaded_procedure.name.clone(),
                label: Some(label),
                index: None,
                len: None,
            })
        })?;
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        frame.ip = ip;
        Ok(())
    }
}
