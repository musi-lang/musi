use music_seam::{Instruction, MethodId, Opcode, Operand};

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn call_arity(&self, module_slot: usize, method: MethodId) -> VmResult<usize> {
        Ok(usize::from(
            self.module(module_slot)?
                .program
                .artifact()
                .methods
                .get(method)
                .params,
        ))
    }

    pub(crate) fn exec_call(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::Call => {
                let Operand::Method(method) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let params = usize::from(
                    self.module(module_slot)?
                        .program
                        .artifact()
                        .methods
                        .get(method)
                        .params,
                );
                let args = self.pop_args(params)?;
                self.push_frame(module_slot, method, args)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CallSeq => {
                let Operand::Method(method) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let args = self.pop_seq_args()?;
                self.push_frame(module_slot, method, args)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CallCls => {
                let callee = self.pop_value()?;
                let closure = Self::expect_closure(callee)?;
                let closure = closure.borrow();
                let total_params = self.call_arity(closure.module_slot, closure.method)?;
                let arg_count = total_params.saturating_sub(closure.captures.len());
                let args = self.pop_args(arg_count)?;
                let mut full_args = closure.captures.clone();
                full_args.extend(args);
                self.push_frame(closure.module_slot, closure.method, full_args)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CallClsSeq => {
                let callee = self.pop_value()?;
                let closure = Self::expect_closure(callee)?;
                let closure = closure.borrow();
                let args = self.pop_seq_args()?;
                let total_params = self.call_arity(closure.module_slot, closure.method)?;
                let arg_count = total_params.saturating_sub(closure.captures.len());
                if args.len() != arg_count {
                    let name = self
                        .module(closure.module_slot)?
                        .program
                        .loaded_method(closure.method)?
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
                self.push_frame(closure.module_slot, closure.method, full_args)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CallTail => {
                let Operand::Method(method) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let params = usize::from(
                    self.module(module_slot)?
                        .program
                        .artifact()
                        .methods
                        .get(method)
                        .params,
                );
                let args = self.pop_args(params)?;
                let _ = self.frames.pop();
                self.push_frame(module_slot, method, args)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::Ret => self.return_from_frame(),
            Opcode::ClsNew => {
                let Operand::WideMethodCaptures { method, captures } = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let capture_count = usize::from(captures);
                let captures = self.pop_args(capture_count)?;
                self.push_value(Value::closure(module_slot, method, captures))?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "call")),
        }
    }
}
