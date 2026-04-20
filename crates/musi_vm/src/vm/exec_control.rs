use super::{Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) const fn before_instruction(&mut self) -> VmResult {
        if let Some(budget) = self.options.instruction_budget {
            if self.executed_instructions >= budget {
                return Err(VmError::new(VmErrorKind::InstructionBudgetExhausted {
                    budget,
                }));
            }
        }
        self.count_instruction();
        Ok(())
    }

    pub(crate) const fn count_instruction(&mut self) {
        self.executed_instructions += 1;
    }
}
