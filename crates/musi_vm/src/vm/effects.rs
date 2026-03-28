use super::*;

impl Vm {
    pub(super) fn dispatch_effect(
        &mut self,
        op: Opcode,
        method_idx: usize,
        pc: &mut usize,
    ) -> VmResult {
        match op {
            Opcode::EffHdlPush => {
                let effect_id = self.read_u16(method_idx, pc);
                let op_id = self.read_u16(method_idx, pc);
                let skip_offset = self.read_i16(method_idx, pc);
                let handler_pc = *pc;
                let saved_stack_depth = self
                    .frames
                    .last()
                    .ok_or(VmError::StackUnderflow)?
                    .stack_depth();
                let handler_frame_depth = self.frames.len() - 1;
                self.effect_handlers.push(EffectHandler {
                    effect_id,
                    op_id,
                    handler_frame_depth,
                    handler_pc,
                    saved_stack_depth,
                });
                *pc = pc.wrapping_add_signed(isize::from(skip_offset));
            }
            Opcode::EffHdlPop => {
                let _ = self.effect_handlers.pop().ok_or(VmError::NoEffectHandler)?;
            }
            Opcode::EffInvk => {
                let effect_idx = self.read_u16(method_idx, pc);
                let op_id = self.read_u16(method_idx, pc);
                let payload = self
                    .frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .pop()?;
                if let Some(resume_value) = self.dispatch_host_effect(effect_idx, op_id, payload)? {
                    self.frames
                        .last_mut()
                        .ok_or(VmError::StackUnderflow)?
                        .push(resume_value);
                    return Ok(());
                }
                let handler_pos = self
                    .effect_handlers
                    .iter()
                    .rposition(|h| h.effect_id == effect_idx && h.op_id == op_id)
                    .ok_or(VmError::NoEffectHandler)?;
                let handler = self.effect_handlers.remove(handler_pos);
                let captured_handlers: Vec<EffectHandler> =
                    self.effect_handlers.drain(handler_pos..).collect();
                let cont_frames = self.frames[handler.handler_frame_depth + 1..].to_vec();
                let resume_pc = *pc;
                let cont_idx =
                    self.heap
                        .alloc_continuation(cont_frames, resume_pc, captured_handlers);
                self.frames.truncate(handler.handler_frame_depth + 1);
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                frame.truncate_stack(handler.saved_stack_depth);
                frame.push(payload);
                frame.push(Value::from_ptr(cont_idx));
                *pc = handler.handler_pc;
            }
            Opcode::EffCont => {
                let flag = self.read_u8(method_idx, pc);
                let frame = self.frames.last_mut().ok_or(VmError::StackUnderflow)?;
                let value = if flag != 0 { frame.pop()? } else { Value::UNIT };
                let cont_ptr = frame.pop()?;
                if !cont_ptr.is_ptr() {
                    return Err(VmError::TypeError {
                        expected: "Continuation",
                        found: "non-ptr",
                    });
                }
                let cont_idx = cont_ptr.as_ptr_idx();
                let obj = self.heap.get(cont_idx).ok_or(VmError::TypeError {
                    expected: "Continuation",
                    found: "non-continuation",
                })?;
                let HeapObject::Continuation(cont) = obj else {
                    return Err(VmError::TypeError {
                        expected: "Continuation",
                        found: "non-continuation",
                    });
                };
                let cont_frames = cont.frames.clone();
                let resume_pc = cont.resume_pc;
                let restored_handlers = cont.captured_handlers.clone();
                self.frames.extend(cont_frames);
                self.effect_handlers.extend(restored_handlers);
                *pc = resume_pc;
                self.frames
                    .last_mut()
                    .ok_or(VmError::StackUnderflow)?
                    .push(value);
            }
            _ => return Err(VmError::UnsupportedOpcode(op)),
        }
        Ok(())
    }

    pub(super) fn dispatch_host_effect(
        &mut self,
        effect_id: u16,
        op_id: u16,
        payload: Value,
    ) -> VmResult<Option<Value>> {
        let Some(mut host) = self.host.take() else {
            return Err(VmError::MissingHost);
        };
        let result = host.handle_effect(self, effect_id, op_id, payload);
        self.host = Some(host);
        result
    }
}
