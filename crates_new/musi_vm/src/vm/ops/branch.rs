use music_bc::{Instruction, Opcode, Operand};

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
                if matches!(cond, Value::Bool(false) | Value::Unit) {
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
                let frame = self
                    .frames
                    .last()
                    .ok_or_else(|| VmError::new(VmErrorKind::EmptyCallFrameStack))?;
                let method = self
                    .module(frame.module_slot)?
                    .program
                    .loaded_method(frame.method)?
                    .name
                    .clone();
                let index = usize::try_from(index_raw).map_err(|_| {
                    VmError::new(VmErrorKind::InvalidBranchIndex {
                        method: method.clone(),
                        index: index_raw,
                        len: labels.len(),
                    })
                })?;
                let label = labels
                    .get(index)
                    .or_else(|| labels.last())
                    .copied()
                    .ok_or_else(|| {
                        VmError::new(VmErrorKind::InvalidBranchIndex {
                            method,
                            index: index_raw,
                            len: labels.len(),
                        })
                    })?;
                self.jump_to(label)?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "branch")),
        }
    }
}
