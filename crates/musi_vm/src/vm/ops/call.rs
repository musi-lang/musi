use music_seam::{Instruction, Opcode, Operand, ProcedureId};

use crate::value::{ForeignValue, GcRef};
use crate::{VmStackKind, VmValueKind};

use crate::vm::{RuntimeCallShape, RuntimeInstruction, RuntimeOperand};

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn exec_call(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::Call => match instruction.operand {
                Operand::WideProcedureCaptures {
                    procedure,
                    captures: 0,
                } => self.exec_seq_procedure_call_for(procedure),
                Operand::None => self.exec_seq_cat(),
                _ => self.exec_procedure_call(instruction),
            },
            Opcode::CallInd => match instruction.operand {
                Operand::I16(_) => self.exec_seq_closure_call(),
                _ => self.exec_closure_call(),
            },
            Opcode::TailCall => self.exec_tail_call(instruction),
            Opcode::Ret => self.return_from_frame(),
            Opcode::NewFn => self.exec_closure_new(instruction),
            _ => Err(Self::invalid_dispatch(instruction, "call")),
        }
    }

    pub(crate) fn exec_fast_call_edge(
        &mut self,
        runtime: &RuntimeInstruction,
    ) -> VmResult<StepOutcome> {
        match runtime.opcode {
            Opcode::Call => {
                let RuntimeOperand::Procedure(procedure) = runtime.operand else {
                    let instruction = self.current_raw_instruction(runtime.raw_index)?;
                    return Err(Self::invalid_operand(&instruction));
                };
                let module_slot = self.current_module_slot()?;
                let args = self.pop_seq_args()?;
                self.push_frame(module_slot, procedure, args)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CallInd => match runtime.operand {
                RuntimeOperand::I16(_) => self.exec_seq_closure_call(),
                _ => self.exec_closure_call(),
            },
            Opcode::NewFn => {
                let RuntimeOperand::WideProcedureCaptures {
                    procedure,
                    captures,
                } = runtime.operand
                else {
                    let instruction = self.current_raw_instruction(runtime.raw_index)?;
                    return Err(Self::invalid_operand(&instruction));
                };
                self.exec_closure_new_with_parts(procedure, captures)
            }
            _ => {
                let instruction = self.current_raw_instruction(runtime.raw_index)?;
                Err(Self::invalid_dispatch(&instruction, "call"))
            }
        }
    }

    fn exec_procedure_call(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        let procedure = Self::procedure_operand(instruction)?;
        let module_slot = self.current_module_slot()?;
        if self.options.stack_frame_limit.is_none() && self.current_call_is_tail_position()? {
            self.replace_frame_from_stack(module_slot, procedure)?;
        } else {
            self.push_frame_from_stack(module_slot, procedure)?;
        }
        Ok(StepOutcome::Continue)
    }

    fn current_call_is_tail_position(&self) -> VmResult<bool> {
        let frame = self.frames.last().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        let procedure = self
            .module(frame.module_slot)?
            .program
            .loaded_procedure(frame.procedure)?;
        let Some(next) = procedure.instructions.get(frame.ip) else {
            return Ok(false);
        };
        if matches!(next.opcode, Opcode::Ret) {
            return Ok(true);
        }
        let Opcode::Br = next.opcode else {
            return Ok(false);
        };
        let Operand::Label(label) = next.operand else {
            return Ok(false);
        };
        let Some(target) = procedure.labels.get(&label).copied() else {
            return Ok(false);
        };
        Ok(procedure
            .instructions
            .get(target)
            .is_some_and(|instruction| matches!(instruction.opcode, Opcode::Ret)))
    }

    fn exec_seq_procedure_call_for(&mut self, procedure: ProcedureId) -> VmResult<StepOutcome> {
        let module_slot = self.current_module_slot()?;
        if self.frames.last().is_some_and(|frame| {
            frame
                .stack
                .last()
                .is_some_and(|value| matches!(value, Value::Seq(_)))
        }) {
            let args = self.pop_seq_args()?;
            self.push_frame(module_slot, procedure, args)?;
        } else {
            self.push_frame_from_stack(module_slot, procedure)?;
        }
        Ok(StepOutcome::Continue)
    }

    fn exec_closure_call(&mut self) -> VmResult<StepOutcome> {
        let callee = self.pop_value()?;
        self.exec_closure_call_value(callee)
    }

    fn exec_closure_call_value(&mut self, callee: Value) -> VmResult<StepOutcome> {
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
        if !self.frames.last().is_some_and(|frame| {
            frame
                .stack
                .last()
                .is_some_and(|value| matches!(value, Value::Seq(_)))
        }) {
            return self.exec_closure_call_value(callee);
        }
        match callee {
            Value::Closure(closure) => {
                let args = self.pop_seq_args()?;
                self.push_seq_closure_call_frame(closure, &args)?;
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
        self.replace_frame_from_stack(module_slot, procedure)?;
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
        self.exec_closure_new_with_parts(procedure, captures)
    }

    fn exec_closure_new_with_parts(
        &mut self,
        procedure: ProcedureId,
        captures: u8,
    ) -> VmResult<StepOutcome> {
        let module_slot = self.current_module_slot()?;
        let capture_count = usize::from(captures);
        let captures = self.pop_args(capture_count)?;
        let value = self.alloc_closure_owned(module_slot, procedure, captures)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn push_closure_call_frame(&mut self, closure: GcRef) -> VmResult {
        let (module_slot, procedure, captures, params, locals) = {
            let closure = self.heap.closure(closure)?;
            (
                closure.module_slot,
                closure.procedure,
                closure.captures.clone(),
                closure.params,
                closure.locals,
            )
        };
        self.push_frame_with_prefix_from_stack_shape(
            module_slot,
            procedure,
            &captures,
            usize::from(params),
            usize::from(locals.max(params)),
        )
    }

    fn push_seq_closure_call_frame(&mut self, closure: GcRef, args: &[Value]) -> VmResult {
        let (module_slot, procedure, captures, params, locals) = {
            let closure = self.heap.closure(closure)?;
            (
                closure.module_slot,
                closure.procedure,
                closure.captures.clone(),
                closure.params,
                closure.locals,
            )
        };
        self.push_frame_with_prefix_and_args_shape(
            module_slot,
            procedure,
            &captures,
            args,
            RuntimeCallShape::new(params, locals),
        )
    }

    pub(crate) fn procedure_operand(instruction: &Instruction) -> VmResult<ProcedureId> {
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
