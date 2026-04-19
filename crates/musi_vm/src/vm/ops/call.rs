use music_seam::{Instruction, Opcode, Operand, ProcedureId};

use crate::VmValueKind;
use crate::value::{ForeignValue, GcRef, ValueList};

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn call_arity(&self, module_slot: usize, procedure: ProcedureId) -> VmResult<usize> {
        Ok(usize::from(
            self.module(module_slot)?
                .program
                .artifact()
                .procedures
                .get(procedure)
                .params,
        ))
    }

    pub(crate) fn exec_call(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::Call => self.exec_procedure_call(instruction),
            Opcode::CallSeq => self.exec_seq_procedure_call(instruction),
            Opcode::CallCls => self.exec_closure_call(),
            Opcode::CallClsSeq => self.exec_seq_closure_call(),
            Opcode::CallTail => self.exec_tail_call(instruction),
            Opcode::Ret => self.return_from_frame(),
            Opcode::ClsNew => self.exec_closure_new(instruction),
            _ => Err(Self::invalid_dispatch(instruction, "call")),
        }
    }

    fn exec_procedure_call(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let procedure = Self::procedure_operand(instruction)?;
        let module_slot = self.current_module_slot()?;
        let params = self.call_arity(module_slot, procedure)?;
        let args = self.pop_args(params)?;
        self.push_frame(module_slot, procedure, args)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_procedure_call(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let procedure = Self::procedure_operand(instruction)?;
        let module_slot = self.current_module_slot()?;
        let args = self.pop_seq_args()?;
        self.push_frame(module_slot, procedure, args)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_closure_call(&mut self) -> VmResult<StepOutcome> {
        let callee = self.pop_value()?;
        match callee {
            Value::Closure(closure) => self.push_closure_call_frame(closure)?,
            Value::Foreign(foreign) => {
                let result = self.call_foreign_value(foreign)?;
                self.push_value(result)?;
            }
            other => return Err(Self::invalid_value_kind(VmValueKind::Closure, &other)),
        }
        Ok(StepOutcome::Continue)
    }

    fn exec_seq_closure_call(&mut self) -> VmResult<StepOutcome> {
        let callee = self.pop_value()?;
        match callee {
            Value::Closure(closure) => {
                let args = self.pop_seq_args()?;
                self.push_seq_closure_call_frame(closure, args)?;
            }
            Value::Foreign(foreign) => {
                let args = self.pop_seq_args()?;
                let result = self.call_foreign_value_with_args(foreign, &args)?;
                self.push_value(result)?;
            }
            other => return Err(Self::invalid_value_kind(VmValueKind::Closure, &other)),
        }
        Ok(StepOutcome::Continue)
    }

    fn exec_tail_call(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let procedure = Self::procedure_operand(instruction)?;
        let module_slot = self.current_module_slot()?;
        let params = self.call_arity(module_slot, procedure)?;
        let args = self.pop_args(params)?;
        let _ = self.frames.pop();
        self.push_frame(module_slot, procedure, args)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_closure_new(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let Operand::WideProcedureCaptures {
            procedure,
            captures,
        } = instruction.operand
        else {
            return Err(Self::invalid_operand(instruction));
        };
        let module_slot = self.current_module_slot()?;
        let capture_count = usize::from(captures);
        let captures = self.pop_args(capture_count)?;
        let value = self.alloc_closure(module_slot, procedure, captures)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn push_closure_call_frame(&mut self, closure: GcRef) -> VmResult {
        let closure = self.heap.closure(closure)?.clone();
        let total_params = self.call_arity(closure.module_slot, closure.procedure)?;
        let arg_count = total_params.saturating_sub(closure.captures.len());
        let args = self.pop_args(arg_count)?;
        let mut full_args = closure.captures.clone();
        full_args.extend(args);
        self.push_frame(closure.module_slot, closure.procedure, full_args)
    }

    fn push_seq_closure_call_frame(&mut self, closure: GcRef, args: ValueList) -> VmResult {
        let closure = self.heap.closure(closure)?.clone();
        let total_params = self.call_arity(closure.module_slot, closure.procedure)?;
        let arg_count = total_params.saturating_sub(closure.captures.len());
        if args.len() != arg_count {
            let name = self
                .module(closure.module_slot)?
                .program
                .loaded_procedure(closure.procedure)?
                .name
                .clone();
            return Err(VmError::new(VmErrorKind::CallArityMismatch {
                callee: name,
                expected: arg_count,
                found: args.len(),
            }));
        }
        let mut full_args = closure.captures.clone();
        full_args.extend(args);
        self.push_frame(closure.module_slot, closure.procedure, full_args)
    }

    fn procedure_operand(instruction: &Instruction) -> VmResult<ProcedureId> {
        let Operand::Procedure(procedure) = instruction.operand else {
            return Err(Self::invalid_operand(instruction));
        };
        Ok(procedure)
    }

    fn call_foreign_value(&mut self, foreign: ForeignValue) -> VmResult<Value> {
        let ForeignValue {
            module_slot,
            foreign,
            type_args,
        } = foreign;
        let call = self.foreign_call(module_slot, foreign);
        let call = Self::specialize_foreign_call(call, &type_args);
        let args = self.pop_args(call.param_tys().len())?;
        self.call_musi_intrinsic(module_slot, &call, &args)
            .unwrap_or_else(|| self.call_host_foreign(&call, &args))
    }

    fn call_foreign_value_with_args(
        &mut self,
        foreign: ForeignValue,
        args: &[Value],
    ) -> VmResult<Value> {
        let ForeignValue {
            module_slot,
            foreign,
            type_args,
        } = foreign;
        let call = self.foreign_call(module_slot, foreign);
        let call = Self::specialize_foreign_call(call, &type_args);
        if args.len() != call.param_tys().len() {
            return Err(VmError::new(VmErrorKind::CallArityMismatch {
                callee: call.name().into(),
                expected: call.param_tys().len(),
                found: args.len(),
            }));
        }
        self.call_musi_intrinsic(module_slot, &call, args)
            .unwrap_or_else(|| self.call_host_foreign(&call, args))
    }
}
