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
}
