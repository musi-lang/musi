use crate::VmStackKind;

use super::state::StepOutcome;
use super::{Value, ValueList, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
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
        let Some(mut frame) = self.frames.pop() else {
            return Err(VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            }));
        };
        frame.recycle();
        self.spare_frames.push(frame);
        if self
            .return_depth
            .is_some_and(|depth| self.frames.len() == depth)
        {
            return Ok(StepOutcome::Return(result));
        }
        if self.frames.is_empty() {
            Ok(StepOutcome::Return(result))
        } else {
            self.push_value(result)?;
            Ok(StepOutcome::Continue)
        }
    }

    pub(crate) fn push_value(&mut self, value: Value) -> VmResult {
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        frame.stack.push(value);
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
}
