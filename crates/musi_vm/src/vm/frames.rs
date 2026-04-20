use crate::VmStackKind;
use music_seam::ProcedureId;

use super::state::CallFrame;
use super::{RuntimeCallShape, Value, ValueList, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn push_frame(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: ValueList,
    ) -> VmResult {
        self.ensure_frame_capacity()?;
        let (_, local_count) = self.check_call_args(module_slot, procedure, args.len())?;
        let mut locals = args;
        locals.resize(local_count, Value::Unit);
        let frame = self.call_frame(module_slot, procedure, locals, ValueList::new())?;
        self.frames.push(frame);
        Ok(())
    }

    pub(crate) fn push_frame_from_arg_slice_with_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: &[Value],
        param_count: usize,
        local_count: usize,
    ) -> VmResult {
        self.ensure_frame_capacity()?;
        if args.len() != param_count {
            return self.call_arity_error(module_slot, procedure, param_count, args.len());
        }
        let mut frame = self.empty_call_frame(module_slot, procedure)?;
        frame.locals.extend(args.iter().cloned());
        frame.locals.resize(local_count, Value::Unit);
        self.frames.push(frame);
        Ok(())
    }

    pub(crate) fn push_frame_from_stack(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
    ) -> VmResult {
        self.ensure_frame_capacity()?;
        let (param_count, local_count) = self.procedure_shape(module_slot, procedure)?;
        self.push_frame_from_stack_shape(module_slot, procedure, param_count, local_count)
    }

    pub(crate) fn push_frame_from_stack_with_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        shape: RuntimeCallShape,
    ) -> VmResult {
        self.ensure_frame_capacity()?;
        self.push_frame_from_stack_shape(
            module_slot,
            procedure,
            shape.param_count(),
            shape.local_count(),
        )
    }

    fn push_frame_from_stack_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        param_count: usize,
        local_count: usize,
    ) -> VmResult {
        self.ensure_operand_count(param_count)?;
        let mut callee = self.empty_call_frame(module_slot, procedure)?;
        callee.locals.resize(local_count, Value::Unit);
        let source_frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        for slot in (0..param_count).rev() {
            callee.locals[slot] = source_frame.stack.pop().ok_or_else(|| {
                VmError::new(VmErrorKind::StackEmpty {
                    stack: VmStackKind::Operand,
                })
            })?;
        }
        self.frames.push(callee);
        Ok(())
    }

    pub(crate) fn push_frame_with_prefix_from_stack_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        prefix: &[Value],
        param_count: usize,
        local_count: usize,
    ) -> VmResult {
        self.ensure_frame_capacity()?;
        if prefix.len() > param_count {
            return self.call_arity_error(module_slot, procedure, param_count, prefix.len());
        }
        let arg_count = param_count - prefix.len();
        self.ensure_operand_count(arg_count)?;
        let mut callee = self.empty_call_frame(module_slot, procedure)?;
        callee.locals.extend(prefix.iter().cloned());
        callee.locals.resize(local_count, Value::Unit);
        let source_frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        for slot in (prefix.len()..param_count).rev() {
            callee.locals[slot] = source_frame.stack.pop().ok_or_else(|| {
                VmError::new(VmErrorKind::StackEmpty {
                    stack: VmStackKind::Operand,
                })
            })?;
        }
        self.frames.push(callee);
        Ok(())
    }

    pub(crate) fn push_frame_with_prefix_and_args_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        prefix: &[Value],
        args: &[Value],
        shape: RuntimeCallShape,
    ) -> VmResult {
        self.ensure_frame_capacity()?;
        let param_count = shape.param_count();
        let found = prefix.len().saturating_add(args.len());
        if found != param_count {
            return self.call_arity_error(module_slot, procedure, param_count, found);
        }
        let mut frame = self.empty_call_frame(module_slot, procedure)?;
        frame.locals.extend(prefix.iter().cloned());
        frame.locals.extend(args.iter().cloned());
        frame.locals.resize(shape.local_count(), Value::Unit);
        self.frames.push(frame);
        Ok(())
    }

    pub(crate) fn replace_frame_from_stack(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
    ) -> VmResult {
        let (param_count, local_count) = self.procedure_shape(module_slot, procedure)?;
        self.replace_frame_from_stack_shape(module_slot, procedure, param_count, local_count)
    }

    pub(crate) fn replace_frame_from_stack_with_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        shape: RuntimeCallShape,
    ) -> VmResult {
        self.replace_frame_from_stack_shape(
            module_slot,
            procedure,
            shape.param_count(),
            shape.local_count(),
        )
    }

    fn replace_frame_from_stack_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        param_count: usize,
        local_count: usize,
    ) -> VmResult {
        if self.can_reuse_current_frame(module_slot, procedure, local_count) {
            return self.reuse_current_frame_from_stack(param_count);
        }
        let locals = self.pop_stack_args_into_locals(param_count, local_count)?;
        let replacement = self.call_frame(module_slot, procedure, locals, ValueList::new())?;
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        *frame = replacement;
        Ok(())
    }

    fn can_reuse_current_frame(
        &self,
        module_slot: usize,
        procedure: ProcedureId,
        local_count: usize,
    ) -> bool {
        self.frames.last().is_some_and(|frame| {
            frame.module_slot == module_slot
                && frame.procedure == procedure
                && frame.locals.len() == local_count
        })
    }

    fn reuse_current_frame_from_stack(&mut self, param_count: usize) -> VmResult {
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        if frame.stack.len() < param_count {
            return Err(VmError::new(VmErrorKind::OperandCountMismatch {
                needed: param_count,
                available: frame.stack.len(),
            }));
        }
        for slot in (0..param_count).rev() {
            frame.locals[slot] = frame.stack.pop().ok_or_else(|| {
                VmError::new(VmErrorKind::StackEmpty {
                    stack: VmStackKind::Operand,
                })
            })?;
        }
        reset_non_param_locals(frame, param_count);
        frame.stack.clear();
        frame.ip = 0;
        Ok(())
    }

    pub(crate) fn procedure_shape(
        &self,
        module_slot: usize,
        procedure: ProcedureId,
    ) -> VmResult<(usize, usize)> {
        let loaded = self
            .module(module_slot)?
            .program
            .loaded_procedure(procedure)?;
        Ok((
            usize::from(loaded.params),
            usize::from(loaded.locals.max(loaded.params)),
        ))
    }

    fn call_arity_error<T>(
        &self,
        module_slot: usize,
        procedure: ProcedureId,
        expected: usize,
        found: usize,
    ) -> VmResult<T> {
        let name = self
            .module(module_slot)?
            .program
            .loaded_procedure(procedure)?
            .name
            .clone();
        Err(VmError::new(VmErrorKind::CallArityMismatch {
            callee: name,
            expected,
            found,
        }))
    }

    fn check_call_args(
        &self,
        module_slot: usize,
        procedure: ProcedureId,
        arg_count: usize,
    ) -> VmResult<(usize, usize)> {
        let loaded = self
            .module(module_slot)?
            .program
            .loaded_procedure(procedure)?;
        let param_count = usize::from(loaded.params);
        if arg_count != param_count {
            return Err(VmError::new(VmErrorKind::CallArityMismatch {
                callee: loaded.name.clone(),
                expected: param_count,
                found: arg_count,
            }));
        }
        Ok((param_count, usize::from(loaded.locals.max(loaded.params))))
    }

    fn ensure_frame_capacity(&self) -> VmResult {
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
        Ok(())
    }

    fn ensure_operand_count(&self, needed: usize) -> VmResult {
        let frame = self.frames.last().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        if frame.stack.len() < needed {
            return Err(VmError::new(VmErrorKind::OperandCountMismatch {
                needed,
                available: frame.stack.len(),
            }));
        }
        Ok(())
    }

    fn pop_stack_args_into_locals(
        &mut self,
        param_count: usize,
        local_count: usize,
    ) -> VmResult<ValueList> {
        let frame = self.frames.last_mut().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        if frame.stack.len() < param_count {
            return Err(VmError::new(VmErrorKind::OperandCountMismatch {
                needed: param_count,
                available: frame.stack.len(),
            }));
        }
        let mut locals = ValueList::new();
        locals.resize(local_count, Value::Unit);
        for slot in (0..param_count).rev() {
            locals[slot] = frame.stack.pop().ok_or_else(|| {
                VmError::new(VmErrorKind::StackEmpty {
                    stack: VmStackKind::Operand,
                })
            })?;
        }
        Ok(locals)
    }
}

fn reset_non_param_locals(frame: &mut CallFrame, param_count: usize) {
    for local in frame.locals.iter_mut().skip(param_count) {
        *local = Value::Unit;
    }
}
