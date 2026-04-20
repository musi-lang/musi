use super::{RuntimeKernel, Value, Vm, VmError, VmErrorKind, VmResult};
use crate::VmValueKind::{Int, Nat};
use crate::value::GcRef;

#[derive(Debug, Clone)]
pub struct BoundExport {
    value: Value,
    kind: BoundExportKind,
}

#[derive(Debug, Clone, Copy)]
enum BoundExportKind {
    Dynamic,
    Kernel {
        module_slot: usize,
        kernel: RuntimeKernel,
    },
}

impl Vm {
    /// Binds one root export once and returns one reusable call handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if initialization is missing or export lookup fails.
    pub fn bind_export(&mut self, name: &str) -> VmResult<BoundExport> {
        self.ensure_initialized()?;
        let value = self.lookup_export_in_slot(0, name)?;
        self.retain_external_value(&value)?;
        let kind = self.bound_export_kind_for(&value)?;
        Ok(BoundExport { value, kind })
    }

    /// Calls one bound export through existing dynamic call path.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when invocation fails.
    pub fn call_bound(&mut self, export: &BoundExport, args: &[Value]) -> VmResult<Value> {
        self.call_value(export.value.clone(), args)
    }

    /// Calls one bound export with zero args and expects one integer-like result.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when invocation fails or result is not integer-like.
    pub fn call0_i64(&mut self, export: &BoundExport) -> VmResult<i64> {
        let value = self.call_bound_fast(export, &[])?;
        int_like_result(value)
    }

    /// Calls one bound export with one integer arg and expects one integer-like result.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when invocation fails or result is not integer-like.
    pub fn call1_i64_i64(&mut self, export: &BoundExport, arg: i64) -> VmResult<i64> {
        let args = [Value::Int(arg)];
        let value = self.call_bound_fast(export, &args)?;
        int_like_result(value)
    }

    /// Calls one bound export with one sequence arg and expects one integer-like result.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when invocation fails or result is not integer-like.
    pub fn call1_seq_i64(&mut self, export: &BoundExport, arg: GcRef) -> VmResult<i64> {
        let args = [Value::Seq(arg)];
        let value = self.call_bound_fast(export, &args)?;
        int_like_result(value)
    }

    fn call_bound_fast(&mut self, export: &BoundExport, args: &[Value]) -> VmResult<Value> {
        if let BoundExportKind::Kernel {
            module_slot,
            kernel,
        } = export.kind
        {
            self.count_instruction();
            if let Some(value) = self.exec_runtime_kernel(module_slot, kernel, args)? {
                return Ok(value);
            }
        }
        self.call_value(export.value.clone(), args)
    }

    fn bound_export_kind_for(&self, value: &Value) -> VmResult<BoundExportKind> {
        let Value::Procedure(procedure) = value else {
            return Ok(BoundExportKind::Dynamic);
        };
        let kernels_enabled = matches!(
            self.options.optimization_level,
            super::VmOptimizationLevel::Tiered
        ) && self.options.instruction_budget.is_none()
            && self.options.stack_frame_limit.is_none();
        if !kernels_enabled {
            return Ok(BoundExportKind::Dynamic);
        }
        let Some(kernel) = self
            .module(procedure.module_slot)?
            .program
            .loaded_procedure(procedure.procedure)?
            .runtime_kernel()
        else {
            return Ok(BoundExportKind::Dynamic);
        };
        Ok(BoundExportKind::Kernel {
            module_slot: procedure.module_slot,
            kernel,
        })
    }
}

fn int_like_result(value: Value) -> VmResult<i64> {
    match value {
        Value::Int(value) => Ok(value),
        Value::Nat(value) => i64::try_from(value).map_err(|_| {
            VmError::new(VmErrorKind::InvalidValueKind {
                expected: Int,
                found: Nat,
            })
        }),
        other => Err(VmError::new(VmErrorKind::InvalidValueKind {
            expected: Int,
            found: other.kind(),
        })),
    }
}
