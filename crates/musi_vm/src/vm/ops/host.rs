use music_bc::{ForeignId, Instruction, Opcode, Operand};

use super::{ForeignCall, StepOutcome, Value, Vm, VmResult};

impl Vm {
    pub(crate) fn foreign_call(&self, module_slot: usize, foreign_id: ForeignId) -> ForeignCall {
        let module = &self.loaded_modules[module_slot];
        let foreign = module.program.artifact().foreigns.get(foreign_id);
        ForeignCall {
            foreign: foreign_id,
            name: module.program.string_text(foreign.name).into(),
            abi: module.program.string_text(foreign.abi).into(),
            symbol: module.program.string_text(foreign.symbol).into(),
            link: foreign
                .link
                .map(|link| module.program.string_text(link).into()),
        }
    }

    pub(crate) fn exec_host_edge(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::FfiCall => {
                let Operand::Foreign(foreign) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let params = usize::from(
                    self.module(module_slot)?
                        .program
                        .artifact()
                        .foreigns
                        .get(foreign)
                        .params,
                );
                let args = self.pop_args(params)?;
                let call = self.foreign_call(module_slot, foreign);
                let result = self.host.call_foreign(&call, &args)?;
                self.push_value(result)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::FfiCallSeq => {
                let Operand::Foreign(foreign) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let args = self.pop_seq_args()?;
                let call = self.foreign_call(module_slot, foreign);
                let result = self.host.call_foreign(&call, &args)?;
                self.push_value(result)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::ModLoad => {
                let spec_value = self.pop_value()?;
                let spec = Self::expect_string_value(spec_value)?;
                let slot = self.load_dynamic_module(spec.as_ref())?;
                self.push_value(Value::module(spec.as_ref(), slot))?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "host")),
        }
    }
}
