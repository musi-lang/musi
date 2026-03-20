//! Call/return and upvalue-close helpers.

use crate::error::{malformed, VmError};
use crate::heap::UpvalueCell;
use crate::value::Value;
use crate::vm::frame::{Continuation, Frame};
use crate::vm::{StepResult, Vm, MAX_CALL_DEPTH};
use crate::VmResult;

impl Vm {
    pub(super) fn close_frame_upvalues(&mut self) {
        let frame_idx = self.call_stack.len() - 1;
        let upvalue_ptrs: Vec<usize> = self.call_stack[frame_idx].open_upvalues.clone();
        for upv_ptr in upvalue_ptrs {
            if let Ok(cell) = self.heap.get_upvalue_mut(upv_ptr) {
                if let UpvalueCell::Open { slot, .. } = *cell {
                    let val = self.call_stack[frame_idx].locals[slot];
                    *cell = UpvalueCell::Closed(val);
                }
            }
        }
        self.open_upvalue_map.retain(|&(fd, _), _| fd != frame_idx);
    }

    // Callers use `?` to propagate; keeping Result keeps the call sites uniform.
    #[expect(clippy::unnecessary_wraps)]
    pub(super) fn do_return(&mut self, value: Value) -> VmResult<StepResult> {
        self.close_frame_upvalues();
        let _ = self.call_stack.pop();
        if let Some(caller) = self.call_stack.last_mut() {
            caller.stack.push(value);
            Ok(StepResult::Continue)
        } else {
            Ok(StepResult::Returned(value))
        }
    }

    pub(super) fn do_call_with_stack_args(
        &mut self,
        fn_id: u32,
        closure_ref: Option<Value>,
    ) -> VmResult<StepResult> {
        if self.call_stack.len() >= MAX_CALL_DEPTH {
            return Err(VmError::StackOverflow);
        }

        let fn_idx = usize::try_from(fn_id).unwrap_or(usize::MAX);
        let func = self
            .module
            .fn_by_index(fn_idx)
            .ok_or_else(|| malformed!("call to unknown fn_id {}", fn_id))?;
        let param_count = usize::from(func.param_count);
        let local_count = usize::from(func.param_count) + usize::from(func.local_count);
        let max_stack = usize::from(func.max_stack);

        let caller_stack_len = self.call_stack.last().map_or(0, |f| f.stack.len());
        if caller_stack_len < param_count {
            return Err(malformed!(
                "call to fn {}: need {} args, stack has {}",
                fn_id,
                param_count,
                caller_stack_len
            ));
        }
        let start = caller_stack_len - param_count;
        let args: Vec<Value> = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("empty call stack"))?
            .stack
            .drain(start..)
            .collect();

        let mut locals = vec![Value::UNIT; local_count];
        for (i, v) in args.into_iter().enumerate() {
            locals[i] = v;
        }

        self.call_stack.push(Frame {
            fn_idx,
            ip: 0,
            locals,
            stack: Vec::with_capacity(max_stack.max(4)),
            marker_stack: vec![],
            closure_ref,
            open_upvalues: vec![],
        });
        Ok(StepResult::Continue)
    }

    pub(super) fn do_tail_call(
        &mut self,
        fn_id: u32,
        closure_ref: Option<Value>,
    ) -> VmResult<StepResult> {
        let fn_idx = usize::try_from(fn_id).unwrap_or(usize::MAX);
        let func = self
            .module
            .fn_by_index(fn_idx)
            .ok_or_else(|| malformed!("tail call to unknown fn_id {}", fn_id))?;
        let param_count = usize::from(func.param_count);
        let local_count = usize::from(func.param_count) + usize::from(func.local_count);

        if self.call_stack.is_empty() {
            return Err(malformed!("tail call with empty call stack"));
        }
        self.close_frame_upvalues();

        let frame = self.current_frame()?;
        let stack_len = frame.stack.len();
        if stack_len < param_count {
            return Err(malformed!(
                "tail call to fn {}: need {} args, stack has {}",
                fn_id,
                param_count,
                stack_len
            ));
        }
        let start = stack_len - param_count;
        let args: Vec<Value> = frame.stack.drain(start..).collect();

        frame.fn_idx = fn_idx;
        frame.ip = 0;
        frame.stack.clear();
        frame.marker_stack.clear();
        frame.closure_ref = closure_ref;
        frame.open_upvalues.clear();

        frame.locals.resize(local_count, Value::UNIT);
        for v in &mut frame.locals {
            *v = Value::UNIT;
        }
        for (i, v) in args.into_iter().enumerate() {
            frame.locals[i] = v;
        }

        Ok(StepResult::Continue)
    }

    pub(super) fn exec_cont_save_cross_frame(
        &mut self,
        effect_id: u8,
        op_id: u32,
    ) -> VmResult<StepResult> {
        let handler_idx =
            self.call_stack
                .iter()
                .enumerate()
                .rev()
                .skip(1)
                .find_map(|(idx, frame)| {
                    frame
                        .marker_stack
                        .iter()
                        .rev()
                        .find(|f| f.effect_id == effect_id)
                        .map(|_| idx)
                });

        let h_idx = handler_idx.ok_or(VmError::NoHandler { effect_id })?;

        let marker = self.call_stack[h_idx]
            .marker_stack
            .iter()
            .rev()
            .find(|f| f.effect_id == effect_id)
            .ok_or_else(|| malformed!("handler not found"))?;
        let handler_fn_id = marker.handler_fn_id;
        let one_shot = marker.one_shot;

        for fi in (h_idx + 1)..self.call_stack.len() {
            let upvalue_ptrs: Vec<usize> = self.call_stack[fi].open_upvalues.clone();
            for upv_ptr in upvalue_ptrs {
                if let Ok(cell) = self.heap.get_upvalue_mut(upv_ptr) {
                    if let UpvalueCell::Open { slot, .. } = *cell {
                        let val = self.call_stack[fi].locals[slot];
                        *cell = UpvalueCell::Closed(val);
                    }
                }
            }
            self.open_upvalue_map.retain(|&(fd, _), _| fd != fi);
        }

        let captured: Vec<Frame> = self.call_stack.drain(h_idx + 1..).collect();
        self.continuations.push(Continuation {
            frames: captured,
            op_id,
            resume_count: 0,
            one_shot,
        });

        self.do_call_with_stack_args(handler_fn_id, None)
    }

    /// Same-frame effect dispatch: handler and effect site are in the same activation
    /// record, so no frames need to be captured. A zero-frame continuation is pushed so
    /// the `one_shot` hint is preserved for any `EFF_RES` that follows.
    pub(super) fn exec_cont_save_same_frame(
        &mut self,
        handler_fn_id: u32,
        op_id: u32,
        one_shot: bool,
    ) -> VmResult<StepResult> {
        self.continuations.push(Continuation {
            frames: vec![],
            op_id,
            resume_count: 0,
            one_shot,
        });
        self.do_call_with_stack_args(handler_fn_id, None)
    }

    pub(super) fn exec_cont_resume(&mut self) -> VmResult<StepResult> {
        let resume_value = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("eff.res with empty call stack"))?
            .stack
            .pop()
            .ok_or_else(|| malformed!("eff.res on empty operand stack"))?;

        let cont_idx = self
            .continuations
            .len()
            .checked_sub(1)
            .ok_or_else(|| malformed!("eff.res with no captured continuation"))?;

        let op_is_fatal = self
            .module
            .effects
            .iter()
            .flat_map(|eff| eff.ops.iter())
            .any(|op| op.id == self.continuations[cont_idx].op_id && op.fatal());
        if op_is_fatal {
            return Err(VmError::FatalEffectResumed {
                op_id: self.continuations[cont_idx].op_id,
            });
        }

        // One-shot optimization: move frames on the first resume of a one-shot
        // continuation; clone for multi-shot or subsequent resumes.
        let frames = if self.continuations[cont_idx].one_shot
            && self.continuations[cont_idx].resume_count == 0
        {
            self.continuations[cont_idx].resume_count += 1;
            self.continuations.pop().expect("checked above").frames
        } else {
            self.continuations[cont_idx].resume_count += 1;
            self.continuations[cont_idx].frames.clone()
        };

        let _ = self.call_stack.pop();
        self.call_stack.extend(frames);

        let top = self
            .call_stack
            .last_mut()
            .ok_or_else(|| malformed!("eff.res: no frame after restoring continuation"))?;
        top.stack.push(resume_value);

        Ok(StepResult::Continue)
    }
}
