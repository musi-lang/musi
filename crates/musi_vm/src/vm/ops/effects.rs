use std::rc::Rc;

use music_seam::{EffectId, Instruction, Opcode, Operand};

use super::{
    CallFrame, ContinuationFrame, ContinuationHandler, ContinuationValuePtr, EffectCall,
    EffectHandler, StepOutcome, Value, ValueList, Vm, VmError, VmErrorKind, VmResult,
};

impl Vm {
    pub(crate) fn exec_effect(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::HdlPush => {
                let Operand::EffectId(effect) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let handler = self.pop_value()?;
                let frame_depth = self
                    .frames
                    .len()
                    .checked_sub(1)
                    .ok_or_else(|| VmError::new(VmErrorKind::EmptyCallFrameStack))?;
                let stack_depth = self.frames.last().map_or(0, |frame| frame.stack.len());
                let pop_ip = self.find_matching_handler_pop(frame_depth)?;
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
            Opcode::HdlPop => {
                let Some(handler) = self.handlers.pop() else {
                    return Err(VmError::new(VmErrorKind::EmptyHandlerStack));
                };
                let body_result = self.pop_value()?;
                self.restore_handler_stack_depth(&handler)?;
                let value_clause = Self::handler_clause_closure(&handler, 0)?;
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
            Opcode::EffInvk => {
                let Operand::Effect { effect, op } = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let args = self.pop_effect_args(module_slot, effect, op)?;
                let result = self.invoke_effect(module_slot, effect, op, &args)?;
                self.push_value(result)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::EffInvkSeq => {
                let Operand::Effect { effect, op } = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let args = self.pop_seq_args()?;
                let result = self.invoke_effect(module_slot, effect, op, &args)?;
                self.push_value(result)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::EffResume => {
                let value = self.pop_value()?;
                let continuation = self.active_resumes.last().cloned().ok_or_else(|| {
                    VmError::new(VmErrorKind::EffectRejected {
                        effect: "<active>".into(),
                        op: None,
                        reason: "resume requires active continuation".into(),
                    })
                })?;
                let result = self.invoke_continuation(&continuation, value)?;
                self.push_value(result)?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "effect")),
        }
    }

    pub(crate) fn invoke_effect(
        &mut self,
        module_slot: usize,
        effect: EffectId,
        op: u16,
        args: &[Value],
    ) -> VmResult<Value> {
        let module = self.module(module_slot)?;
        let descriptor = module.program.artifact().effects.get(effect);
        let effect_name: Box<str> = module.program.string_text(descriptor.name).into();
        let op_desc = descriptor.ops.get(usize::from(op)).ok_or_else(|| {
            VmError::new(VmErrorKind::EffectOpOutOfBounds {
                effect: effect_name.clone(),
                op_index: op,
                op_count: descriptor.ops.len(),
            })
        })?;
        if let Some(handler_index) = self
            .handlers
            .iter()
            .rposition(|handler| handler.effect == effect)
        {
            let handler = self.handlers.get(handler_index).cloned().ok_or_else(|| {
                VmError::new(VmErrorKind::EffectRejected {
                    effect: effect_name.clone(),
                    op: Some(module.program.string_text(op_desc.name).into()),
                    reason: "matching handler is missing".into(),
                })
            })?;
            let closure =
                Self::handler_clause_closure(&handler, usize::from(op).saturating_add(1))?;
            let continuation = self.capture_continuation(handler_index)?;
            let mut clause_args = args.to_vec();
            clause_args.push(Value::Continuation(Rc::clone(&continuation)));
            self.frames.truncate(handler.frame_depth.saturating_add(1));
            self.handlers.truncate(handler_index.saturating_add(1));
            self.restore_handler_stack_depth(&handler)?;
            self.active_resumes.push(continuation);
            let result = self.call_value(closure, &clause_args);
            let _ = self.active_resumes.pop();
            let result = result?;
            self.finish_handled_effect(handler_index, result)
        } else {
            let effect_call = EffectCall {
                program: module.program.clone(),
                effect,
                module: module.spec.clone(),
                effect_name,
                op,
                op_name: module.program.string_text(op_desc.name).into(),
                param_tys: op_desc.param_tys.clone(),
                result_ty: op_desc.result_ty,
            };
            self.host.handle_effect(&effect_call, args)
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
            VmError::new(VmErrorKind::EffectOpOutOfBounds {
                effect: effect_name,
                op_index: op,
                op_count: descriptor.ops.len(),
            })
        })?;
        self.pop_args(op_desc.param_tys.len())
    }

    pub(crate) fn handler_clause_closure(handler: &EffectHandler, index: usize) -> VmResult<Value> {
        let handler_value = handler.handler.clone();
        let handler_data = Self::expect_data(handler_value)?;
        let data_ref = handler_data.borrow();
        let closure = data_ref.fields.get(index).cloned().ok_or_else(|| {
            VmError::new(VmErrorKind::EffectRejected {
                effect: format!("{}", handler.effect.raw()).into(),
                op: None,
                reason: format!("handler clause `{index}` is missing").into(),
            })
        })?;
        Ok(closure)
    }

    pub(crate) fn restore_handler_stack_depth(&mut self, handler: &EffectHandler) -> VmResult {
        let frame = self.frames.get_mut(handler.frame_depth).ok_or_else(|| {
            VmError::new(VmErrorKind::HandlerFrameMissing {
                handler_id: handler.handler_id,
                frame_depth: handler.frame_depth,
            })
        })?;
        frame.stack.truncate(handler.stack_depth);
        Ok(())
    }

    pub(crate) fn capture_continuation(
        &self,
        handler_index: usize,
    ) -> VmResult<ContinuationValuePtr> {
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
        match Value::continuation(frames, handlers) {
            Value::Continuation(continuation) => Ok(continuation),
            _ => Err(VmError::new(VmErrorKind::ProgramShapeInvalid {
                detail: "continuation construction failed".into(),
            })),
        }
    }

    pub(crate) fn finish_handled_effect(
        &mut self,
        handler_index: usize,
        result: Value,
    ) -> VmResult<Value> {
        let handler = self.handlers.get(handler_index).cloned().ok_or_else(|| {
            VmError::new(VmErrorKind::EffectRejected {
                effect: "<handler>".into(),
                op: None,
                reason: "matching handler is missing".into(),
            })
        })?;
        self.frames.truncate(handler.frame_depth.saturating_add(1));
        self.handlers.truncate(handler_index);
        let frame = self.frames.get_mut(handler.frame_depth).ok_or_else(|| {
            VmError::new(VmErrorKind::HandlerFrameMissing {
                handler_id: handler.handler_id,
                frame_depth: handler.frame_depth,
            })
        })?;
        frame.stack.truncate(handler.stack_depth);
        frame.ip = handler.pop_ip.saturating_add(1);
        Ok(result)
    }

    pub(crate) fn find_matching_handler_pop(&self, frame_depth: usize) -> VmResult<usize> {
        let frame = self
            .frames
            .get(frame_depth)
            .ok_or_else(|| VmError::new(VmErrorKind::EmptyCallFrameStack))?;
        let method = self
            .module(frame.module_slot)?
            .program
            .loaded_method(frame.method)?;
        let mut depth = 0usize;
        for (index, instruction) in method.instructions.iter().enumerate().skip(frame.ip) {
            match instruction.opcode {
                Opcode::HdlPush => depth = depth.saturating_add(1),
                Opcode::HdlPop if depth == 0 => return Ok(index),
                Opcode::HdlPop => depth = depth.saturating_sub(1),
                _ => {}
            }
        }
        Err(VmError::new(VmErrorKind::MatchingHandlerPopMissing {
            method: method.name.clone(),
        }))
    }
}

impl From<ContinuationFrame> for CallFrame {
    fn from(frame: ContinuationFrame) -> Self {
        Self::new(frame.module_slot, frame.method, frame.locals, frame.stack).with_ip(frame.ip)
    }
}

impl From<CallFrame> for ContinuationFrame {
    fn from(frame: CallFrame) -> Self {
        Self::new(frame.module_slot, frame.method, frame.locals, frame.stack).with_ip(frame.ip)
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
