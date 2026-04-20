use super::options::DEFAULT_AUTO_COLLECT_THRESHOLD_BYTES;
use super::{
    ForeignValue, ProcedureValue, RuntimeCallShape, Value, Vm, VmError, VmErrorKind, VmResult,
};

impl Vm {
    /// Calls one runtime value if it is callable.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if initialization is missing or value is not callable.
    pub fn call_value(&mut self, value: Value, args: &[Value]) -> VmResult<Value> {
        self.ensure_initialized()?;
        self.observe_heap_value(&value)?;
        for arg in args {
            self.observe_heap_value(arg)?;
        }
        let base_depth = self.frames.len();
        let result = match value {
            Value::Procedure(procedure) => self.call_procedure_value(procedure, args, base_depth),
            Value::Closure(closure) => {
                let (module_slot, procedure, params, locals, captures) = {
                    let closure = self.heap.closure(closure)?;
                    (
                        closure.module_slot,
                        closure.procedure,
                        closure.params,
                        closure.locals,
                        closure.captures.clone(),
                    )
                };
                let param_count = usize::from(params);
                let local_count = usize::from(locals.max(params));
                if captures.is_empty() {
                    if let Some(value) =
                        self.try_invoke_kernel_from_args(module_slot, procedure, args)?
                    {
                        return self.finish_call_value(base_depth, value);
                    }
                    if base_depth == 0 {
                        self.invoke_procedure_from_args_shape(
                            module_slot,
                            procedure,
                            args,
                            param_count,
                            local_count,
                        )
                    } else {
                        self.invoke_procedure_in_context_from_args_shape(
                            module_slot,
                            procedure,
                            args,
                            base_depth,
                            param_count,
                            local_count,
                        )
                    }
                } else if base_depth == 0 {
                    self.invoke_procedure_with_prefix_args_shape(
                        module_slot,
                        procedure,
                        &captures,
                        args,
                        RuntimeCallShape::new(params, locals),
                    )
                } else {
                    self.invoke_procedure_in_context_with_prefix_args_shape(
                        module_slot,
                        procedure,
                        &captures,
                        args,
                        base_depth,
                        RuntimeCallShape::new(params, locals),
                    )
                }
            }
            Value::Continuation(continuation) => {
                let [value] = args else {
                    return Err(VmError::new(VmErrorKind::CallArityMismatch {
                        callee: "continuation".into(),
                        expected: 1,
                        found: args.len(),
                    }));
                };
                self.invoke_continuation(continuation, value.clone())
            }
            Value::Foreign(foreign_value) => {
                let ForeignValue {
                    module_slot,
                    foreign,
                    type_args,
                } = foreign_value;
                let call = self.foreign_call(module_slot, foreign);
                let call = Self::specialize_foreign_call(call, &type_args);
                self.call_musi_intrinsic(module_slot, &call, args)
                    .unwrap_or_else(|| self.call_host_foreign(&call, args))
            }
            _ => Err(VmError::new(VmErrorKind::NonCallableValue {
                found: value.kind(),
            })),
        }?;
        self.retain_external_value(&result)?;
        if base_depth == 0 && self.should_collect_after_call() {
            let _ = self.collect_garbage();
        }
        Ok(result)
    }

    fn call_procedure_value(
        &mut self,
        procedure: ProcedureValue,
        args: &[Value],
        base_depth: usize,
    ) -> VmResult<Value> {
        if let Some(value) =
            self.try_invoke_kernel_from_args(procedure.module_slot, procedure.procedure, args)?
        {
            return self.finish_call_value(base_depth, value);
        }
        let param_count = usize::from(procedure.params);
        let local_count = usize::from(procedure.locals.max(procedure.params));
        if base_depth == 0 {
            self.invoke_procedure_from_args_shape(
                procedure.module_slot,
                procedure.procedure,
                args,
                param_count,
                local_count,
            )
        } else {
            self.invoke_procedure_in_context_from_args_shape(
                procedure.module_slot,
                procedure.procedure,
                args,
                base_depth,
                param_count,
                local_count,
            )
        }
    }

    fn finish_call_value(&mut self, base_depth: usize, result: Value) -> VmResult<Value> {
        self.retain_external_value(&result)?;
        if base_depth == 0 && self.should_collect_after_call() {
            let _ = self.collect_garbage();
        }
        Ok(result)
    }

    const fn should_collect_after_call(&self) -> bool {
        self.heap_dirty
            && (self.options.gc_stress
                || self.options.heap_limit_bytes.is_some()
                || self.heap.allocated_bytes() >= DEFAULT_AUTO_COLLECT_THRESHOLD_BYTES)
    }
}
