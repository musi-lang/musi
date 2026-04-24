use crate::{VmIndexSpace, VmStackKind};
use music_seam::{EffectId, Instruction, Opcode, Operand};

use super::{
    CallFrame, ContinuationFrame, ContinuationHandler, EffectCall, EffectHandler, GcRef,
    RuntimeInstruction, RuntimeOperand, StepOutcome, Value, ValueList, Vm, VmError, VmErrorKind,
    VmResult,
};

impl Vm {
    pub(crate) fn exec_effect(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::HdlPush => {
                let Operand::EffectId(effect) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let frame_depth = self.frames.len().checked_sub(1).ok_or_else(|| {
                    VmError::new(VmErrorKind::StackEmpty {
                        stack: VmStackKind::CallFrame,
                    })
                })?;
                let pop_ip = self.find_matching_handler_pop(frame_depth)?;
                self.push_effect_handler(effect, pop_ip)
            }
            Opcode::HdlPop => self.pop_effect_handler(),
            Opcode::Raise => {
                let Operand::Effect { effect, op } = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                if op & 0x8000 != 0 {
                    self.invoke_effect_from_seq(effect, op & 0x7FFF)
                } else {
                    self.invoke_effect_from_stack(effect, op)
                }
            }
            Opcode::Resume => self.resume_active_effect(),
            _ => Err(Self::invalid_dispatch(instruction, "effect")),
        }
    }

    pub(crate) fn exec_fast_effect(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        match runtime.opcode {
            Opcode::HdlPush => {
                let RuntimeOperand::EffectId(effect) = runtime.operand else {
                    let instruction = self.current_raw_instruction(runtime.raw_index)?;
                    return Err(Self::invalid_operand(&instruction));
                };
                let Some(pop_ip) = runtime.branch_target else {
                    let frame_depth = self.frames.len().checked_sub(1).ok_or_else(|| {
                        VmError::new(VmErrorKind::StackEmpty {
                            stack: VmStackKind::CallFrame,
                        })
                    })?;
                    return self
                        .push_effect_handler(effect, self.find_matching_handler_pop(frame_depth)?);
                };
                self.push_effect_handler(effect, pop_ip)
            }
            Opcode::HdlPop => self.pop_effect_handler(),
            Opcode::Raise => {
                let RuntimeOperand::Effect { effect, op } = runtime.operand else {
                    let instruction = self.current_raw_instruction(runtime.raw_index)?;
                    return Err(Self::invalid_operand(&instruction));
                };
                if op & 0x8000 != 0 {
                    self.invoke_effect_from_seq(effect, op & 0x7FFF)
                } else {
                    self.invoke_effect_from_stack(effect, op)
                }
            }
            Opcode::Resume => self.resume_active_effect(),
            _ => {
                let instruction = self.current_raw_instruction(runtime.raw_index)?;
                Err(Self::invalid_dispatch(&instruction, "effect"))
            }
        }
    }

    fn push_effect_handler(&mut self, effect: EffectId, pop_ip: usize) -> VmResult<StepOutcome> {
        let handler = self.pop_value()?;
        let frame_depth = self.frames.len().checked_sub(1).ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        let stack_depth = self.frames.last().map_or(0, |frame| frame.stack.len());
        let handler_id = self.next_handler_id;
        self.next_handler_id = self.next_handler_id.saturating_add(1);
        self.handlers.push(
            EffectHandler::new(handler_id, effect, handler).with_stack_state(
                frame_depth,
                stack_depth,
                pop_ip,
            ),
        );
        Ok(StepOutcome::Continue)
    }

    fn pop_effect_handler(&mut self) -> VmResult<StepOutcome> {
        let Some(handler) = self.handlers.pop() else {
            return Err(VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::Handler,
            }));
        };
        let body_result = self.pop_value()?;
        self.restore_handler_stack_depth(&handler)?;
        let value_clause = self.handler_clause_closure(&handler, 0)?;
        let result = self.call_value(value_clause, &[body_result])?;
        if self
            .continuation_target_handler
            .is_some_and(|target| target == handler.handler_id)
        {
            return Ok(StepOutcome::Return(result));
        }
        self.push_value(result)?;
        Ok(StepOutcome::Continue)
    }

    fn invoke_effect_from_stack(&mut self, effect: EffectId, op: u16) -> VmResult<StepOutcome> {
        let module_slot = self.current_module_slot()?;
        let args = self.pop_effect_args(module_slot, effect, op)?;
        let result = self.invoke_effect(module_slot, effect, op, &args)?;
        self.push_value(result)?;
        Ok(StepOutcome::Continue)
    }

    fn invoke_effect_from_seq(&mut self, effect: EffectId, op: u16) -> VmResult<StepOutcome> {
        let module_slot = self.current_module_slot()?;
        let args = self.pop_seq_args()?;
        let result = self.invoke_effect(module_slot, effect, op, &args)?;
        self.push_value(result)?;
        Ok(StepOutcome::Continue)
    }

    fn resume_active_effect(&mut self) -> VmResult<StepOutcome> {
        let value = self.pop_value()?;
        let continuation = self.active_resumes.last().copied().ok_or_else(|| {
            VmError::new(VmErrorKind::EffectRejected {
                effect: "<active>".into(),
                op: None,
                reason: "resume needs active continuation".into(),
            })
        })?;
        let result = self.invoke_continuation(continuation, value)?;
        self.push_value(result)?;
        Ok(StepOutcome::Continue)
    }

    pub(crate) fn invoke_effect(
        &mut self,
        module_slot: usize,
        effect: EffectId,
        op: u16,
        args: &[Value],
    ) -> VmResult<Value> {
        let op_index = usize::from(op);
        {
            let module = self.module(module_slot)?;
            let descriptor = module.program.artifact().effects.get(effect);
            if op_index >= descriptor.ops.len() {
                let effect_name: Box<str> = module.program.string_text(descriptor.name).into();
                return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::EffectOp,
                    owner: Some(effect_name),
                    index: i64::from(op),
                    len: descriptor.ops.len(),
                }));
            }
        }
        if let Some(handler_index) = self
            .handlers
            .last()
            .and_then(|handler| {
                (handler.effect == effect).then_some(self.handlers.len().saturating_sub(1))
            })
            .or_else(|| {
                self.handlers
                    .iter()
                    .rposition(|handler| handler.effect == effect)
            })
        {
            let (handler_id, handler_effect, handler_value, frame_depth, stack_depth, pop_ip) =
                self.handlers
                    .get(handler_index)
                    .map(|handler| {
                        (
                            handler.handler_id,
                            handler.effect,
                            handler.handler.clone(),
                            handler.frame_depth,
                            handler.stack_depth,
                            handler.pop_ip,
                        )
                    })
                    .ok_or_else(|| {
                        VmError::new(VmErrorKind::EffectRejected {
                            effect: format!("{}", effect.raw()).into(),
                            op: Some(format!("{op}").into()),
                            reason: "matching handler is missing".into(),
                        })
                    })?;
            let closure = self.handler_clause_closure_from_value(
                handler_effect,
                &handler_value,
                op_index.saturating_add(1),
            )?;
            let continuation = self.capture_continuation(handler_index)?;
            let mut clause_args = args.iter().cloned().collect::<ValueList>();
            clause_args.push(Value::Continuation(continuation));
            self.frames.truncate(frame_depth.saturating_add(1));
            self.handlers.truncate(handler_index.saturating_add(1));
            self.restore_handler_stack_state(handler_id, frame_depth, stack_depth)?;
            self.active_resumes.push(continuation);
            let result = self.call_value(closure, &clause_args);
            let _ = self.active_resumes.pop();
            let result = result?;
            self.finish_handled_effect_state(
                handler_index,
                handler_id,
                frame_depth,
                stack_depth,
                pop_ip,
                result,
            )
        } else {
            let module = self.module(module_slot)?;
            let descriptor = module.program.artifact().effects.get(effect);
            let effect_name: Box<str> = module.program.string_text(descriptor.name).into();
            let op_desc = descriptor.ops.get(op_index).ok_or_else(|| {
                VmError::new(VmErrorKind::IndexOutOfBounds {
                    space: VmIndexSpace::EffectOp,
                    owner: Some(effect_name.clone()),
                    index: i64::from(op),
                    len: descriptor.ops.len(),
                })
            })?;
            let effect_call = EffectCall {
                program: module.program.clone(),
                effect,
                module: module.spec.as_ref().into(),
                effect_name,
                op,
                op_name: module.program.string_text(op_desc.name).into(),
                param_tys: op_desc.param_tys.clone(),
                result_ty: op_desc.result_ty,
                is_comptime_safe: op_desc.is_comptime_safe,
            };
            self.call_host_effect(&effect_call, args)
        }
    }

    pub(crate) fn pop_effect_args(
        &mut self,
        module_slot: usize,
        effect: EffectId,
        op: u16,
    ) -> VmResult<ValueList> {
        let descriptor = self
            .module(module_slot)?
            .program
            .artifact()
            .effects
            .get(effect);
        let effect_name: Box<str> = self
            .module(module_slot)?
            .program
            .string_text(descriptor.name)
            .into();
        let op_desc = descriptor.ops.get(usize::from(op)).ok_or_else(|| {
            VmError::new(VmErrorKind::IndexOutOfBounds {
                space: VmIndexSpace::EffectOp,
                owner: Some(effect_name),
                index: i64::from(op),
                len: descriptor.ops.len(),
            })
        })?;
        self.pop_args(op_desc.param_tys.len())
    }

    pub(crate) fn handler_clause_closure(
        &self,
        handler: &EffectHandler,
        index: usize,
    ) -> VmResult<Value> {
        self.handler_clause_closure_from_value(handler.effect, &handler.handler, index)
    }

    pub(crate) fn handler_clause_closure_from_value(
        &self,
        effect: EffectId,
        handler_value: &Value,
        index: usize,
    ) -> VmResult<Value> {
        let handler_value = handler_value.clone();
        let handler_data = Self::expect_data(handler_value)?;
        let data_ref = self.heap.data(handler_data)?;
        let closure = data_ref.fields.get(index).cloned().ok_or_else(|| {
            VmError::new(VmErrorKind::EffectRejected {
                effect: format!("{}", effect.raw()).into(),
                op: None,
                reason: format!("handler clause `{index}` is missing").into(),
            })
        })?;
        Ok(closure)
    }

    pub(crate) fn restore_handler_stack_depth(&mut self, handler: &EffectHandler) -> VmResult {
        self.restore_handler_stack_state(
            handler.handler_id,
            handler.frame_depth,
            handler.stack_depth,
        )
    }

    pub(crate) fn restore_handler_stack_state(
        &mut self,
        handler_id: u64,
        frame_depth: usize,
        stack_depth: usize,
    ) -> VmResult {
        let frame = self.frames.get_mut(frame_depth).ok_or_else(|| {
            VmError::new(VmErrorKind::HandlerFrameMissing {
                handler_id,
                frame_depth,
            })
        })?;
        frame.stack.truncate(stack_depth);
        Ok(())
    }

    pub(crate) fn capture_continuation(&mut self, handler_index: usize) -> VmResult<GcRef> {
        let handler = self.handlers.get(handler_index).ok_or_else(|| {
            VmError::new(VmErrorKind::EffectRejected {
                effect: "<handler>".into(),
                op: None,
                reason: "matching handler is missing".into(),
            })
        })?;
        let frames = self.frames[handler.frame_depth..]
            .iter()
            .cloned()
            .map(ContinuationFrame::from)
            .collect();
        let handlers = self.handlers[handler_index..]
            .iter()
            .cloned()
            .map(ContinuationHandler::from)
            .collect();
        match self.alloc_continuation(frames, handlers)? {
            Value::Continuation(continuation) => Ok(continuation),
            _ => Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "continuation construction failed".into(),
            })),
        }
    }

    pub(crate) fn finish_handled_effect_state(
        &mut self,
        handler_index: usize,
        handler_id: u64,
        frame_depth: usize,
        stack_depth: usize,
        pop_ip: usize,
        result: Value,
    ) -> VmResult<Value> {
        self.frames.truncate(frame_depth.saturating_add(1));
        self.handlers.truncate(handler_index);
        let frame = self.frames.get_mut(frame_depth).ok_or_else(|| {
            VmError::new(VmErrorKind::HandlerFrameMissing {
                handler_id,
                frame_depth,
            })
        })?;
        frame.stack.truncate(stack_depth);
        frame.set_ip(pop_ip.saturating_add(1));
        Ok(result)
    }

    pub(crate) fn find_matching_handler_pop(&self, frame_depth: usize) -> VmResult<usize> {
        let frame = self.frames.get(frame_depth).ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        let procedure = self
            .module(frame.module_slot)?
            .program
            .loaded_procedure(frame.procedure)?;
        let mut depth = 0usize;
        for (index, instruction) in procedure.instructions.iter().enumerate().skip(frame.ip) {
            match instruction.opcode {
                Opcode::HdlPush => depth = depth.saturating_add(1),
                Opcode::HdlPop if depth == 0 => return Ok(index),
                Opcode::HdlPop => depth = depth.saturating_sub(1),
                _ => {}
            }
        }
        Err(VmError::new(VmErrorKind::MissingMatchingHandlerPop {
            procedure: procedure.name.clone(),
        }))
    }
}

impl From<ContinuationFrame> for CallFrame {
    fn from(frame: ContinuationFrame) -> Self {
        Self::new(
            frame.module_slot,
            frame.procedure,
            frame.locals,
            frame.stack,
        )
        .with_ip(frame.ip)
    }
}

impl From<CallFrame> for ContinuationFrame {
    fn from(frame: CallFrame) -> Self {
        Self::new(
            frame.module_slot,
            frame.procedure,
            frame.locals,
            frame.stack,
        )
        .with_ip(frame.ip)
    }
}

impl From<ContinuationHandler> for EffectHandler {
    fn from(handler: ContinuationHandler) -> Self {
        Self::new(handler.handler_id, handler.effect, handler.handler).with_stack_state(
            handler.frame_depth,
            handler.stack_depth,
            handler.pop_ip,
        )
    }
}

impl From<EffectHandler> for ContinuationHandler {
    fn from(handler: EffectHandler) -> Self {
        Self::new(handler.handler_id, handler.effect, handler.handler).with_stack_state(
            handler.frame_depth,
            handler.stack_depth,
            handler.pop_ip,
        )
    }
}
