use crate::{VmIndexSpace, VmValueKind};
use music_seam::ProcedureId;

use super::{
    CompareOp, GcRef, RuntimeKernel, RuntimeSeq2Mutation, Value, Vm, VmError, VmErrorKind,
    VmOptimizationLevel, VmResult,
};

impl Vm {
    pub(crate) fn try_invoke_kernel_from_args(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        args: &[Value],
    ) -> VmResult<Option<Value>> {
        if !self.kernels_enabled() {
            return Ok(None);
        }
        let kernel = self
            .module(module_slot)?
            .program
            .loaded_procedure(procedure)?
            .runtime_kernel();
        let Some(kernel) = kernel else {
            return Ok(None);
        };
        self.count_instruction();
        self.exec_runtime_kernel(module_slot, kernel, args)
    }

    const fn kernels_enabled(&self) -> bool {
        matches!(self.options.optimization_level, VmOptimizationLevel::Tiered)
            && self.options.instruction_budget.is_none()
            && self.options.stack_frame_limit.is_none()
    }

    pub(super) fn exec_runtime_kernel(
        &mut self,
        module_slot: usize,
        kernel: RuntimeKernel,
        args: &[Value],
    ) -> VmResult<Option<Value>> {
        match kernel {
            RuntimeKernel::IntTailAccumulator {
                compare_local,
                compare_smi,
                compare,
                dec_local,
                dec_smi,
                acc_local,
                add_local,
                return_local,
            } => Self::exec_int_tail_accumulator(
                args,
                IntTailAccumulatorKernel {
                    compare_local,
                    compare_smi,
                    compare,
                    dec_local,
                    dec_smi,
                    acc_local,
                    add_local,
                    return_local,
                },
            ),
            RuntimeKernel::DirectIntWrapperCall {
                arg_local,
                const_arg,
                procedure,
            } => {
                let arg = kernel_arg(args, arg_local)?.clone();
                let args = [arg, Value::Int(i64::from(const_arg))];
                self.try_invoke_kernel_from_args(module_slot, procedure, &args)
            }
            RuntimeKernel::IntArgAddSmi { arg_local, smi } => {
                let int = expect_kernel_int(kernel_arg(args, arg_local)?)?;
                Ok(Some(Value::Int(
                    int.checked_add(i64::from(smi))
                        .ok_or_else(int_overflow_error)?,
                )))
            }
            RuntimeKernel::DataConstructMatchAdd { source, smi } => {
                let int = expect_kernel_int(kernel_arg(args, source)?)?;
                Ok(Some(Value::Int(
                    int.checked_add(i64::from(smi))
                        .ok_or_else(int_overflow_error)?,
                )))
            }
            RuntimeKernel::Seq2Mutation2x2 {
                grid_local,
                init_value,
                update_add,
            } => self.exec_seq2_mutation_2x2_kernel(args, grid_local, init_value, update_add),
            RuntimeKernel::Seq2Mutation(plan) => self.exec_seq2_mutation_kernel(args, plan),
            RuntimeKernel::InlineEffectResume {
                resume_value,
                value_add,
            } => Self::exec_inline_effect_resume_kernel(args, resume_value, value_add),
            RuntimeKernel::InlineEffectResumeClauses { .. } => Ok(None),
        }
    }

    fn exec_inline_effect_resume_kernel(
        args: &[Value],
        resume_value: i16,
        value_add: i16,
    ) -> VmResult<Option<Value>> {
        if !args.is_empty() {
            return Ok(None);
        }
        Ok(Some(Value::Int(
            i64::from(resume_value)
                .checked_add(i64::from(value_add))
                .ok_or_else(int_overflow_error)?,
        )))
    }

    fn exec_int_tail_accumulator(
        args: &[Value],
        kernel: IntTailAccumulatorKernel,
    ) -> VmResult<Option<Value>> {
        let needed = [
            kernel.compare_local,
            kernel.dec_local,
            kernel.acc_local,
            kernel.add_local,
            kernel.return_local,
        ]
        .into_iter()
        .map(usize::from)
        .max()
        .unwrap_or(0)
        .saturating_add(1);
        if needed > 8 || args.len() > 8 {
            return Ok(None);
        }
        let mut locals = [0i64; 8];
        for (slot, value) in args.iter().enumerate() {
            locals[slot] = expect_kernel_int(value)?;
        }
        if let Some(value) = sum_kernel_result(&locals, kernel)? {
            return Ok(Some(Value::Int(value)));
        }
        loop {
            if kernel.compare_local != kernel.dec_local {
                locals[usize::from(kernel.compare_local)] = locals[usize::from(kernel.dec_local)];
            }
            let compare_value = locals[usize::from(kernel.compare_local)];
            if compare_int_for_kernel(kernel.compare, compare_value, i64::from(kernel.compare_smi))
            {
                return Ok(Some(Value::Int(locals[usize::from(kernel.return_local)])));
            }
            let dec_value = locals[usize::from(kernel.dec_local)];
            let acc_value = locals[usize::from(kernel.acc_local)];
            let add_value = locals[usize::from(kernel.add_local)];
            let next_dec = dec_value
                .checked_sub(i64::from(kernel.dec_smi))
                .ok_or_else(int_overflow_error)?;
            let next_acc = acc_value
                .checked_add(add_value)
                .ok_or_else(int_overflow_error)?;
            locals[usize::from(kernel.dec_local)] = next_dec;
            locals[usize::from(kernel.acc_local)] = next_acc;
        }
    }

    fn exec_seq2_mutation_kernel(
        &mut self,
        args: &[Value],
        plan: RuntimeSeq2Mutation,
    ) -> VmResult<Option<Value>> {
        let grid = expect_kernel_seq(kernel_arg(args, plan.grid_local)?)?;
        self.heap
            .fast_seq2_mutation(grid, plan)
            .map(|value| Some(Value::Int(value)))
    }

    fn exec_seq2_mutation_2x2_kernel(
        &mut self,
        args: &[Value],
        grid_local: u16,
        init_value: i16,
        update_add: i16,
    ) -> VmResult<Option<Value>> {
        let grid = expect_kernel_seq(kernel_arg(args, grid_local)?)?;
        self.heap
            .fast_seq2_mutation_2x2_kernel(grid, init_value, update_add)
            .map(|value| Some(Value::Int(value)))
    }
}

fn sum_kernel_result(locals: &[i64], kernel: IntTailAccumulatorKernel) -> VmResult<Option<i64>> {
    if kernel.compare != CompareOp::Eq
        || kernel.compare_smi != 0
        || kernel.dec_smi != 1
        || kernel.add_local != kernel.dec_local
        || kernel.return_local != kernel.acc_local
    {
        return Ok(None);
    }
    let n = locals[usize::from(kernel.dec_local)];
    if n < 0 {
        return Ok(None);
    }
    let acc = locals[usize::from(kernel.acc_local)];
    let sum = n
        .checked_mul(n.checked_add(1).ok_or_else(int_overflow_error)?)
        .and_then(|value| value.checked_div(2))
        .ok_or_else(int_overflow_error)?;
    Ok(Some(acc.checked_add(sum).ok_or_else(int_overflow_error)?))
}

#[derive(Clone, Copy)]
struct IntTailAccumulatorKernel {
    compare_local: u16,
    compare_smi: i16,
    compare: CompareOp,
    dec_local: u16,
    dec_smi: i16,
    acc_local: u16,
    add_local: u16,
    return_local: u16,
}

fn kernel_arg(args: &[Value], local: u16) -> VmResult<&Value> {
    args.get(usize::from(local))
        .ok_or_else(|| local_index_error(local, args.len()))
}

fn expect_kernel_int(value: &Value) -> VmResult<i64> {
    match value {
        Value::Int(value) => Ok(*value),
        Value::Nat(number) => {
            i64::try_from(*number).map_err(|_| Vm::invalid_value_kind(VmValueKind::Int, value))
        }
        other => Err(Vm::invalid_value_kind(VmValueKind::Int, other)),
    }
}

const fn expect_kernel_seq(value: &Value) -> VmResult<GcRef> {
    match value {
        Value::Seq(reference) => Ok(*reference),
        other => Err(Vm::invalid_value_kind(VmValueKind::Seq, other)),
    }
}

fn local_index_error(local: u16, len: usize) -> VmError {
    VmError::new(VmErrorKind::IndexOutOfBounds {
        space: VmIndexSpace::Local,
        owner: None,
        index: i64::from(local),
        len,
    })
}

fn int_overflow_error() -> VmError {
    VmError::new(VmErrorKind::ArithmeticFailed {
        detail: "signed integer overflow".into(),
    })
}

const fn compare_int_for_kernel(compare: CompareOp, left: i64, right: i64) -> bool {
    match compare {
        CompareOp::Eq => left == right,
        CompareOp::Ne => left != right,
        CompareOp::Lt => left < right,
        CompareOp::Gt => left > right,
        CompareOp::Le => left <= right,
        CompareOp::Ge => left >= right,
    }
}
