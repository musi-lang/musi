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
                let index_value = self.pop_value()?;
                let index_raw = Self::expect_int(&index_value)?;
                let instruction_index = self.current_instruction_index()?;
                self.branch_table_jump_at(instruction_index, index_raw)?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "branch")),
        }
    }

    pub(crate) fn try_branch_table_after_data_tag(&mut self, tag: i64) -> VmResult<bool> {
        let Some(instruction_index) = self.next_branch_table_index()? else {
            return Ok(false);
        };
        self.push_value(Value::Int(tag))?;
        self.before_instruction()?;
        self.skip_next_instruction()?;
        let index_value = self.pop_value()?;
        let index_raw = Self::expect_int(&index_value)?;
        self.branch_table_jump_at(instruction_index, index_raw)?;
        Ok(true)
    }

    fn next_branch_table_index(&self) -> VmResult<Option<usize>> {
        let frame = self.frames.last().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        let loaded_procedure = self
            .module(frame.module_slot)?
            .program
            .loaded_procedure(frame.procedure)?;
        loaded_procedure
            .instructions
            .get(frame.ip)
            .map_or(Ok(None), |instruction| {
                Ok(matches!(instruction.opcode, Opcode::BrTbl).then_some(frame.ip))
            })
    }

    fn current_instruction_index(&self) -> VmResult<usize> {
        let frame = self.frames.last().ok_or_else(|| {
            VmError::new(VmErrorKind::StackEmpty {
                stack: VmStackKind::CallFrame,
            })
        })?;
        Ok(frame.ip.saturating_sub(1))
    }

    pub(crate) fn branch_table_jump_at(
        &mut self,
        instruction_index: usize,
        index_raw: i64,
    ) -> VmResult {
        let action = {
            let frame = self.frames.last().ok_or_else(|| {
                VmError::new(VmErrorKind::StackEmpty {
                    stack: VmStackKind::CallFrame,
                })
            })?;
            let loaded_procedure = self
                .module(frame.module_slot)?
                .program
                .loaded_procedure(frame.procedure)?;
            let instruction = loaded_procedure
                .instructions
                .get(instruction_index)
                .ok_or_else(|| {
                    VmError::new(VmErrorKind::InvalidBranchTarget {
                        procedure: loaded_procedure.name.clone(),
                        label: Some(u16::MAX),
                        index: None,
                        len: None,
                    })
                })?;
            let Operand::BranchTable(labels) = &instruction.operand else {
                return Err(Self::invalid_operand(instruction));
            };
            let index = usize::try_from(index_raw).map_err(|_| {
                Self::branch_table_index_error(
                    loaded_procedure.name.clone(),
                    index_raw,
                    labels.len(),
                )
            })?;
            let label = labels
                .get(index)
                .or_else(|| labels.last())
                .copied()
                .ok_or_else(|| {
                    Self::branch_table_index_error(
                        loaded_procedure.name.clone(),
                        index_raw,
                        labels.len(),
                    )
                })?;
            let target = loaded_procedure
                .runtime_branch_table(instruction_index)
                .and_then(|table| table.target_for(index));
            target.map_or(BranchJumpAction::Label(label), BranchJumpAction::Ip)
        };
        match action {
            BranchJumpAction::Ip(target) => self.jump_to_ip(target),
            BranchJumpAction::Label(label) => self.jump_to(label),
        }
    }

    const fn branch_table_index_error(procedure: Box<str>, index_raw: i64, len: usize) -> VmError {
        VmError::new(VmErrorKind::InvalidBranchTarget {
            procedure,
            label: None,
            index: Some(index_raw),
            len: Some(len),
        })
    }
}

enum BranchJumpAction {
    Ip(usize),
    Label(u16),
}
