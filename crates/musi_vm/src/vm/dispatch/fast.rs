use super::int::pop_int_from_stack;
use super::*;

impl Vm {
    pub(super) fn exec_fast_ldloc(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        let RuntimeOperand::Local(slot) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let frame = self.current_frame_mut()?;
        let index = usize::from(slot);
        let Some(value) = frame.locals.get(index).cloned() else {
            return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Local,
                owner: None,
                index: i64::from(slot),
                len: frame.locals.len(),
            }));
        };
        frame.stack.push(value);
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_stloc(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        let RuntimeOperand::Local(slot) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let frame = self.current_frame_mut()?;
        let value = frame.stack.pop().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::Operand,
            })
        })?;
        let index = usize::from(slot);
        let len = frame.locals.len();
        let Some(local) = frame.locals.get_mut(index) else {
            return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Local,
                owner: None,
                index: i64::from(slot),
                len,
            }));
        };
        *local = value;
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_ldsmi(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        let RuntimeOperand::I16(value) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        self.current_frame_mut()?
            .stack
            .push(Value::Int(i64::from(value)));
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_ldglob(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        let RuntimeOperand::Global(slot) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let module_slot = self.current_module_slot()?;
        let raw_slot = usize::try_from(slot.raw()).unwrap_or(usize::MAX);
        let value = self
            .module(module_slot)?
            .globals
            .get(raw_slot)
            .cloned()
            .ok_or_else(|| {
                VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::Global,
                    owner: None,
                    index: i64::try_from(raw_slot).unwrap_or(i64::MAX),
                    len: self
                        .module(module_slot)
                        .map_or(0, |module| module.globals.len()),
                })
            })?;
        self.current_frame_mut()?.stack.push(value);
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_stglob(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        let RuntimeOperand::Global(slot) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let value = self.pop_value()?;
        let module_slot = self.current_module_slot()?;
        let globals = &mut self.module_mut(module_slot)?.globals;
        let raw_slot = usize::try_from(slot.raw()).unwrap_or(usize::MAX);
        let len = globals.len();
        let Some(global) = globals.get_mut(raw_slot) else {
            return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::Global,
                owner: None,
                index: i64::try_from(raw_slot).unwrap_or(i64::MAX),
                len,
            }));
        };
        *global = value;
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_ldconst(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        let RuntimeOperand::Constant(constant) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let module_slot = self.current_module_slot()?;
        let constant_value = self
            .module(module_slot)?
            .program
            .artifact()
            .constants
            .get(constant)
            .value
            .clone();
        let value = self.constant_value(module_slot, &constant_value)?;
        self.current_frame_mut()?.stack.push(value);
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_int_op(
        &mut self,
        runtime: &RuntimeInstruction,
        op: impl FnOnce(i64, i64) -> Option<i64>,
    ) -> VmResult<StepOutcome> {
        let frame = self.current_frame()?;
        let stack_len = frame.stack.len();
        let has_int_args = stack_len >= 2
            && matches!(frame.stack.get(stack_len - 1), Some(Value::Int(_)))
            && matches!(frame.stack.get(stack_len - 2), Some(Value::Int(_)));
        if !has_int_args {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return self.execute_instr(&instruction);
        }
        let frame = self.current_frame_mut()?;
        let right = pop_int_from_stack(frame)?;
        let left = pop_int_from_stack(frame)?;
        let result = op(left, right).ok_or_else(|| {
            VmError::new(VmErrorKind::ArithmeticFailed {
                detail: "signed integer overflow".into(),
            })
        })?;
        frame.stack.push(Value::Int(result));
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_br(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let Some(target) = runtime.branch_target else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return self.exec_branch(&instruction);
        };
        self.jump_to_ip(target)?;
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_brfalse(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        let Some(target) = runtime.branch_target else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return self.exec_branch(&instruction);
        };
        let cond = self.pop_value()?;
        match self.bool_flag(&cond) {
            Some(false) => self.jump_to_ip(target)?,
            Some(true) => {}
            None => return Err(Self::invalid_value_kind(VmValueKind::Bool, &cond)),
        }
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_call(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        let RuntimeOperand::Procedure(procedure) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let module_slot = self.current_module_slot()?;
        if runtime.call_mode == RuntimeCallMode::Tail && self.options.stack_frame_limit.is_none() {
            if let Some(shape) = runtime.call_shape {
                self.replace_frame_from_stack_with_shape(module_slot, procedure, shape)?;
            } else {
                self.replace_frame_from_stack(module_slot, procedure)?;
            }
        } else if let Some(shape) = runtime.call_shape {
            self.push_frame_from_stack_with_shape(module_slot, procedure, shape)?;
        } else {
            self.push_frame_from_stack(module_slot, procedure)?;
        }
        Ok(StepOutcome::Continue)
    }

    pub(super) fn exec_fast_tail_call(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        let RuntimeOperand::Procedure(procedure) = runtime.operand else {
            let instruction = self.current_raw_instruction(runtime.raw_index)?;
            return Err(Self::invalid_operand(&instruction));
        };
        let module_slot = self.current_module_slot()?;
        if let Some(shape) = runtime.call_shape {
            self.replace_frame_from_stack_with_shape(module_slot, procedure, shape)?;
        } else {
            self.replace_frame_from_stack(module_slot, procedure)?;
        }
        Ok(StepOutcome::Continue)
    }
}
