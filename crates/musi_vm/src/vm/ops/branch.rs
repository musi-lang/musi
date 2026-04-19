use music_seam::{Instruction, Opcode, Operand};

use crate::VmStackKind;

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn exec_branch(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::Br => {
                let Operand::Label(label) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                self.jump_to(label)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::BrFalse => {
                let Operand::Label(label) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let cond = self.pop_value()?;
                if matches!(cond, Value::Unit) || self.bool_flag(&cond) == Some(false) {
                    self.jump_to(label)?;
                }
                Ok(StepOutcome::Continue)
            }
            Opcode::BrTbl => {
                let Operand::BranchTable(ref labels) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let index_value = self.pop_value()?;
                let index_raw = Self::expect_int(&index_value)?;
                let frame = self.frames.last().ok_or_else(|| {
                    VmError::new(VmErrorKind::StackEmpty {
                        stack: VmStackKind::CallFrame,
                    })
                })?;
                let procedure = self
                    .module(frame.module_slot)?
                    .program
                    .loaded_procedure(frame.procedure)?
                    .name
                    .clone();
                let index = usize::try_from(index_raw).map_err(|_| {
                    VmError::new(VmErrorKind::InvalidBranchTarget {
                        procedure: procedure.clone(),
                        label: None,
                        index: Some(index_raw),
                        len: Some(labels.len()),
                    })
                })?;
                let label = labels
                    .get(index)
                    .or_else(|| labels.last())
                    .copied()
                    .ok_or_else(|| {
                        VmError::new(VmErrorKind::InvalidBranchTarget {
                            procedure,
                            label: None,
                            index: Some(index_raw),
                            len: Some(labels.len()),
                        })
                    })?;
                self.jump_to(label)?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "branch")),
        }
    }
}
