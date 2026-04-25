use super::*;

impl Vm {
    pub(crate) fn invoke_procedure_from_args_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: &[Value],
        param_count: usize,
        local_count: usize,
    ) -> VmResult<Value> {
        self.push_frame_from_arg_slice_with_shape(
            module_slot,
            procedure,
            args,
            param_count,
            local_count,
        )?;
        self.run_current_state()
    }

    pub(crate) fn invoke_procedure_with_prefix_args_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        prefix: &[Value],
        args: &[Value],
        shape: RuntimeCallShape,
    ) -> VmResult<Value> {
        self.push_frame_with_prefix_and_args_shape(module_slot, procedure, prefix, args, shape)?;
        self.run_current_state()
    }

    pub(crate) fn invoke_procedure_in_context(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: ValueList,
        base_depth: usize,
    ) -> VmResult<Value> {
        self.push_frame(module_slot, procedure, args)?;
        let saved_return_depth = self.return_depth;
        self.return_depth = Some(base_depth);
        let result = self.run_current_state();
        self.return_depth = saved_return_depth;
        result
    }

    pub(crate) fn invoke_procedure_in_context_from_args_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: &[Value],
        base_depth: usize,
        param_count: usize,
        local_count: usize,
    ) -> VmResult<Value> {
        self.push_frame_from_arg_slice_with_shape(
            module_slot,
            procedure,
            args,
            param_count,
            local_count,
        )?;
        let saved_return_depth = self.return_depth;
        self.return_depth = Some(base_depth);
        let result = self.run_current_state();
        self.return_depth = saved_return_depth;
        result
    }

    pub(crate) fn invoke_procedure_in_context_with_prefix_args_shape(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        prefix: &[Value],
        args: &[Value],
        base_depth: usize,
        shape: RuntimeCallShape,
    ) -> VmResult<Value> {
        self.push_frame_with_prefix_and_args_shape(module_slot, procedure, prefix, args, shape)?;
        let saved_return_depth = self.return_depth;
        self.return_depth = Some(base_depth);
        let result = self.run_current_state();
        self.return_depth = saved_return_depth;
        result
    }

    pub(crate) fn invoke_continuation(
        &mut self,
        continuation: GcRef,
        value: Value,
    ) -> VmResult<Value> {
        let saved_frames = mem::take(&mut self.frames);
        let saved_handlers = mem::take(&mut self.handlers);
        let saved_active_resumes = mem::take(&mut self.active_resumes);
        let saved_target = self.continuation_target_handler;
        let saved_return_depth = self.return_depth;

        let (frames, handlers, target_handler) = {
            let continuation = self.heap.continuation(continuation)?;
            let frames = continuation
                .frames
                .iter()
                .cloned()
                .map(CallFrame::from)
                .collect::<CallFrameList>();
            let handlers = continuation
                .handlers
                .iter()
                .cloned()
                .map(EffectHandler::from)
                .collect::<EffectHandlerList>();
            let target_handler = continuation
                .handlers
                .first()
                .map(|handler| handler.handler_id);
            (frames, handlers, target_handler)
        };

        self.frames = frames;
        self.handlers = handlers;
        self.active_resumes = Vec::new();
        self.continuation_target_handler = target_handler;
        self.return_depth = None;
        self.refresh_frame_runtime_codes()?;
        let result = match self.frames.last_mut() {
            Some(frame) => {
                frame.stack.push(value);
                self.run_current_state()
            }
            None => Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "continuation frame list is empty".into(),
            })),
        };

        self.frames = saved_frames;
        self.handlers = saved_handlers;
        self.active_resumes = saved_active_resumes;
        self.continuation_target_handler = saved_target;
        self.return_depth = saved_return_depth;
        result
    }
}
