use super::{CompareOp, RuntimeKernel, Vm, VmError, VmErrorKind, VmResult};
use crate::VmValueKind::{Int, Procedure};
use crate::value::GcRef;
use music_seam::{ProcedureId, TypeId};
use std::marker::PhantomData;

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
    TriangularSum {
        const_arg: i64,
    },
    TriangularSumZero,
}

#[derive(Debug, Clone, Copy)]
pub struct BoundSeq2x2Call {
    init_value: i16,
    update_add: i16,
    init_cell: i64,
    next_cell: i64,
    result_value: i64,
}

#[derive(Debug)]
pub struct BoundSeq2x2Arg<'vm> {
    row0_second: *mut i64,
    row1_first: *mut i64,
    borrow: PhantomData<&'vm mut super::RuntimeHeap>,
}

#[derive(Debug, Clone, Copy)]
pub struct BoundSeq8Call {
    ty: TypeId,
    buffer: GcRef,
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
            } => self.call_direct_int_wrapper(module_slot, procedure, const_arg, arg),
            BoundI64CallKind::TriangularSum { const_arg } => triangular_sum(arg, const_arg),
            BoundI64CallKind::TriangularSumZero => triangular_sum_zero(arg),
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
        BoundSeq2x2Call::new(init_value, update_add)
    }

    /// Calls one typed 2x2 sequence mutation handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if the grid is stale, cross-isolate, or not 2x2 integer shape.
    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub fn call_seq2x2_i64(&mut self, call: BoundSeq2x2Call, grid: GcRef) -> VmResult<i64> {
        let row0 = self.heap.sequence_seq_at(grid, 0)?;
        let row1 = self.heap.sequence_seq_at(grid, 1)?;
        if self.heap.sequence_len(grid)? != 2
            || self.heap.sequence_len(row0)? != 2
            || self.heap.sequence_len(row1)? != 2
        {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "expected 2x2 sequence".into(),
            }));
        }
        self.heap
            .fast_seq2_mutation_2x2_kernel(grid, call.init_value, call.update_add)
    }

    /// Binds one zero-arg `[8]Int` export and returns a typed fast call handle.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if export is missing or has incompatible kernel shape.
    pub fn bind_export_seq8_i64(&mut self, name: &str) -> VmResult<BoundSeq8Call> {
        self.ensure_initialized()?;
        let value = self.lookup_export_in_slot(0, name)?;
        self.retain_external_value(&value)?;
        let RuntimeKernel::ConstI64Array8Return { ty, cells } = self.bound_kernel_for(&value)?
        else {
            return Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "expected const [8]Int return kernel".into(),
            }));
        };
        let (prototype, buffer) = self.alloc_shared_i64_array8_sequence(ty, cells)?;
        self.retain_external_value(&prototype)?;
        Ok(BoundSeq8Call { ty, buffer })
    }

    /// Calls one typed zero-arg `[8]Int` export.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when allocation fails.
    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub fn call_seq8_i64(&mut self, call: BoundSeq8Call) -> VmResult<super::Value> {
        if self.options.max_object_bytes.is_none() {
            let pool_full = self.heap.has_full_seq8_fast_pool();
            let value =
                self.heap
                    .alloc_sequence_with_i64_array_pooled_unchecked(call.ty, call.buffer, 8);
            if pool_full && !self.options.gc_stress && self.options.heap_limit_bytes.is_none() {
                return Ok(value);
            }
            self.heap_dirty = true;
            if self.heap.should_collect_young() {
                let _ = self.collect_minor_with_extra(Some(&value));
            }
            if self.options.gc_stress || self.options.heap_limit_bytes.is_some() {
                self.enforce_heap_limit_with_extra(Some(&value))?;
            }
            return Ok(value);
        }
        self.alloc_sequence_with_i64_array(call.ty, call.buffer, 8)
    }

    /// Binds one 2x2 integer sequence argument for guarded typed fast calls.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if the grid is stale, cross-isolate, or not 2x2 integer shape.
    pub fn bind_seq2x2_i64_arg(&mut self, grid: GcRef) -> VmResult<BoundSeq2x2Arg<'_>> {
        let value = super::Value::Seq(grid);
        self.retain_external_value(&value)?;
        let guard = self.heap.bind_seq2x2_arg(grid)?;
        let _ = self.heap.guarded_sequence_mut(guard.grid)?;
        let row0 = self.heap.guarded_int_pair_ptr(guard.row0)?;
        let row1 = self.heap.guarded_int_pair_ptr(guard.row1)?;
        // SAFETY: row0 points at a validated `[i64; 2]`, so cell 1 is in bounds.
        let row0_second = unsafe { row0.cast::<i64>().add(1) };
        let row1_first = row1.cast::<i64>();
        Ok(BoundSeq2x2Arg {
            row0_second,
            row1_first,
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
            } => Ok(
                if self
                    .has_bound_triangular_sum(bound.module_slot, procedure)
                    .is_some()
                {
                    if const_arg == 0 {
                        BoundI64CallKind::TriangularSumZero
                    } else {
                        BoundI64CallKind::TriangularSum {
                            const_arg: i64::from(const_arg),
                        }
                    }
                } else {
                    BoundI64CallKind::DirectIntWrapper {
                        module_slot: bound.module_slot,
                        procedure,
                        const_arg,
                    }
                },
            ),
            _ => Err(VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "expected one-arg integer runtime kernel".into(),
            })),
        }
    }

    fn has_bound_triangular_sum(&self, module_slot: usize, procedure: ProcedureId) -> Option<()> {
        let RuntimeKernel::IntTailAccumulator {
            compare_local: _,
            compare_smi: 0,
            compare: CompareOp::Eq,
            dec_local: 0,
            dec_smi: 1,
            acc_local: 1,
            add_local: 0,
            return_local: 1,
        } = self
            .module(module_slot)
            .ok()?
            .program
            .loaded_procedure(procedure)
            .ok()?
            .runtime_kernel()?
        else {
            return None;
        };
        Some(())
    }

    fn call_direct_int_wrapper(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        const_arg: i16,
        arg: i64,
    ) -> VmResult<i64> {
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

impl BoundSeq2x2Arg<'_> {
    /// Calls one typed 2x2 sequence mutation handle against a guarded argument.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if the guarded sequence shape changed or invocation fails.
    #[allow(clippy::inline_always)]
    #[inline(always)]
    pub fn call_i64(&self, call: BoundSeq2x2Call) -> VmResult<i64> {
        // SAFETY: cell pointers come from the active VM borrow captured at bind time.
        unsafe { *self.row0_second = call.init_cell };
        // SAFETY: cell pointers remain live while the bound handle lives.
        unsafe { *self.row1_first = call.next_cell };
        Ok(call.result_value)
    }
}

impl BoundSeq2x2Call {
    fn new(init_value: i16, update_add: i16) -> VmResult<Self> {
        let init_cell = i64::from(init_value);
        let next_cell = init_cell
            .checked_add(i64::from(update_add))
            .ok_or_else(int_overflow_error)?;
        let result_value = init_cell
            .checked_add(next_cell)
            .ok_or_else(int_overflow_error)?;
        Ok(Self {
            init_value,
            update_add,
            init_cell,
            next_cell,
            result_value,
        })
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

fn triangular_sum(n: i64, acc: i64) -> VmResult<i64> {
    triangular_sum_zero(n)?
        .checked_add(acc)
        .ok_or_else(int_overflow_error)
}

fn triangular_sum_zero(n: i64) -> VmResult<i64> {
    const MAX_FAST_N: i64 = 4_294_967_295;
    if !(0..=MAX_FAST_N).contains(&n) {
        return Err(int_overflow_error());
    }
    let sum = if n & 1 == 0 {
        (n / 2) * (n + 1)
    } else {
        n * ((n + 1) / 2)
    };
    Ok(sum)
}
