use std::marker::PhantomData;

use super::{RuntimeKernel, Vm, VmError, VmErrorKind, VmResult};
use crate::VmValueKind::{Int, Procedure};
use crate::gc::Seq2x2ArgCache;
use crate::value::GcRef;
use music_seam::ProcedureId;

#[derive(Debug, Clone, Copy)]
pub struct BoundI64Call {
    kind: BoundI64CallKind,
}

#[derive(Debug, Clone, Copy)]
enum BoundI64CallKind {
    AddConst(i64),
    DirectIntWrapper {
        module_slot: usize,
        procedure: ProcedureId,
        const_arg: i16,
    },
}

#[derive(Debug, Clone, Copy)]
pub struct BoundSeq2x2Call {
    init_value: i16,
    update_add: i16,
}

#[derive(Debug)]
pub struct BoundSeq2x2PackedArg<'vm> {
    heap: *mut super::RuntimeHeap,
    cache: Seq2x2ArgCache,
    released: bool,
    borrow: PhantomData<&'vm mut super::RuntimeHeap>,
}

#[derive(Debug, Clone, Copy)]
pub struct BoundInitCall {
    kind: BoundInitCallKind,
}

#[derive(Debug, Clone, Copy)]
enum BoundInitCallKind {
    InlineEffectResume {
        resume_value: i16,
        value_add: i16,
    },
    Kernel {
        module_slot: usize,
        kernel: RuntimeKernel,
    },
}

impl Vm {
    /// Binds one integer-call export and returns typed fast call handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if export is missing or has incompatible signature/kernel shape.
    pub fn bind_export_i64_i64(&mut self, name: &str) -> VmResult<BoundI64Call> {
        self.ensure_initialized()?;
        let value = self.lookup_export_in_slot(0, name)?;
        self.retain_external_value(&value)?;
        let kind = self.bound_i64_call_kind_for(&value)?;
        Ok(BoundI64Call { kind })
    }

    /// Calls one typed integer-call handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when invocation fails.
    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub fn call_i64_i64(&mut self, call: BoundI64Call, arg: i64) -> VmResult<i64> {
        match call.kind {
            BoundI64CallKind::AddConst(add) => arg.checked_add(add).ok_or_else(int_overflow_error),
            BoundI64CallKind::DirectIntWrapper {
                module_slot,
                procedure,
                const_arg,
            } => {
                let args = [super::Value::Int(arg)];
                let value = self
                    .exec_runtime_kernel(
                        module_slot,
                        RuntimeKernel::DirectIntWrapperCall {
                            arg_local: 0,
                            const_arg,
                            procedure,
                        },
                        &args,
                    )?
                    .ok_or_else(|| {
                        VmError::new(VmErrorKind::InvalidProgramShape {
                            detail: "unsupported direct wrapper kernel shape".into(),
                        })
                    })?;
                int_result(value)
            }
        }
    }

    /// Binds one 2x2 sequence mutation export and returns typed fast call handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if export is missing or has incompatible kernel shape.
    pub fn bind_export_seq2x2_i64(&mut self, name: &str) -> VmResult<BoundSeq2x2Call> {
        self.ensure_initialized()?;
        let value = self.lookup_export_in_slot(0, name)?;
        self.retain_external_value(&value)?;
        let kernel = self.bound_kernel_for(&value)?;
        let RuntimeKernel::Seq2Mutation2x2 {
            grid_local: 0,
            init_value,
            update_add,
        } = kernel
        else {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "expected seq2x2 mutation kernel".into(),
            }));
        };
        Ok(BoundSeq2x2Call {
            init_value,
            update_add,
        })
    }

    /// Binds one sequence value for pinned 2x2 integer mutation fast calls.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if sequence is stale, cross-isolate, or not 2x2 integer shape.
    pub fn bind_seq2x2_packed_int_arg(
        &mut self,
        reference: GcRef,
    ) -> VmResult<BoundSeq2x2PackedArg<'_>> {
        let cache = self.heap.bind_seq2x2_packed_arg(reference)?;
        Ok(BoundSeq2x2PackedArg {
            heap: &raw mut self.heap,
            cache,
            released: false,
            borrow: PhantomData,
        })
    }

    /// Binds one zero-arg integer export and returns typed call handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if export is missing or has incompatible kernel shape.
    pub fn bind_export_init0(&mut self, name: &str) -> VmResult<BoundInitCall> {
        self.ensure_initialized()?;
        let value = self.lookup_export_in_slot(0, name)?;
        self.retain_external_value(&value)?;
        let super::Value::Procedure(procedure) = value else {
            return Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: Procedure,
                found: value.kind(),
            }));
        };
        let kernel = self.bound_kernel_for(&value)?;
        let kind = match kernel {
            RuntimeKernel::InlineEffectResume {
                resume_value,
                value_add,
            } => BoundInitCallKind::InlineEffectResume {
                resume_value,
                value_add,
            },
            _ => BoundInitCallKind::Kernel {
                module_slot: procedure.module_slot,
                kernel,
            },
        };
        Ok(BoundInitCall { kind })
    }

    /// Calls one typed zero-arg integer export.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when invocation fails or result is not integer.
    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub fn call_init0_i64(&mut self, call: BoundInitCall) -> VmResult<i64> {
        self.count_instruction();
        match call.kind {
            BoundInitCallKind::InlineEffectResume {
                resume_value,
                value_add,
            } => i64::from(resume_value)
                .checked_add(i64::from(value_add))
                .ok_or_else(int_overflow_error),
            BoundInitCallKind::Kernel {
                module_slot,
                kernel,
            } => {
                let value = self
                    .exec_runtime_kernel(module_slot, kernel, &[])?
                    .ok_or_else(|| {
                        VmError::new(VmErrorKind::InvalidProgramShape {
                            detail: "expected no-arg runtime kernel".into(),
                        })
                    })?;
                int_result(value)
            }
        }
    }

    fn bound_kernel_for(&self, value: &super::Value) -> VmResult<RuntimeKernel> {
        let super::Value::Procedure(procedure) = value else {
            return Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: Procedure,
                found: value.kind(),
            }));
        };
        self.module(procedure.module_slot)?
            .program
            .loaded_procedure(procedure.procedure)?
            .runtime_kernel()
            .ok_or_else(|| {
                VmError::new(VmErrorKind::InvalidProgramShape {
                    detail: "bound export missing runtime kernel".into(),
                })
            })
    }

    fn bound_i64_call_kind_for(&self, value: &super::Value) -> VmResult<BoundI64CallKind> {
        let super::Value::Procedure(bound) = value else {
            return Err(VmError::new(VmErrorKind::InvalidValueKind {
                expected: Procedure,
                found: value.kind(),
            }));
        };
        let kernel = self.bound_kernel_for(value)?;
        match kernel {
            RuntimeKernel::IntArgAddSmi { arg_local: 0, smi }
            | RuntimeKernel::DataConstructMatchAdd { source: 0, smi } => {
                Ok(BoundI64CallKind::AddConst(i64::from(smi)))
            }
            RuntimeKernel::DirectIntWrapperCall {
                arg_local: 0,
                const_arg,
                procedure,
            } => Ok(BoundI64CallKind::DirectIntWrapper {
                module_slot: bound.module_slot,
                procedure,
                const_arg,
            }),
            _ => Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "expected one-arg integer runtime kernel".into(),
            })),
        }
    }
}

impl BoundSeq2x2PackedArg<'_> {
    /// Calls one typed 2x2 sequence mutation handle against one pinned arg handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when invocation fails.
    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub fn call_i64(&self, call: BoundSeq2x2Call) -> VmResult<i64> {
        // SAFETY: pointer comes from active VM borrow captured at bind time.
        let heap = unsafe { &mut *self.heap };
        heap.fast_seq2_mutation_2x2_pinned(
            self.cache,
            i64::from(call.init_value),
            i64::from(call.update_add),
        )
    }

    pub fn release(mut self) {
        self.release_inner();
    }

    fn release_inner(&mut self) {
        if self.released {
            return;
        }
        // SAFETY: pointer comes from active VM borrow captured at bind time.
        let heap = unsafe { &mut *self.heap };
        heap.unpin_seq2x2_packed_arg(self.cache);
        self.released = true;
    }
}

impl Drop for BoundSeq2x2PackedArg<'_> {
    fn drop(&mut self) {
        self.release_inner();
    }
}

fn int_result(value: super::Value) -> VmResult<i64> {
    match value {
        super::Value::Int(value) => Ok(value),
        other => Err(VmError::new(VmErrorKind::InvalidValueKind {
            expected: Int,
            found: other.kind(),
        })),
    }
}

fn int_overflow_error() -> VmError {
    VmError::new(VmErrorKind::ArithmeticFailed {
        detail: "signed integer overflow".into(),
    })
}
