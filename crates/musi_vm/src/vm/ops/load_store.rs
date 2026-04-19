use std::rc::Rc;

use crate::VmIndexSpace;
use music_seam::{Instruction, Opcode, Operand};

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn exec_load_store(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::LdLoc => {
                let Operand::Local(slot) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                self.push_value(self.local(slot)?.clone())?;
                Ok(StepOutcome::Continue)
            }
            Opcode::StLoc => {
                let Operand::Local(slot) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let value = self.pop_value()?;
                self.observe_heap_value(&value)?;
                *self.local_mut(slot)? = value;
                self.after_value_mutation()?;
                Ok(StepOutcome::Continue)
            }
            Opcode::LdGlob => {
                let Operand::Global(slot) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let module_name = self.module(module_slot)?.spec.clone();
                let globals = &self.module(module_slot)?.globals;
                let raw_slot = usize::try_from(slot.raw()).unwrap_or(usize::MAX);
                let value = globals.get(raw_slot).cloned().ok_or_else(|| {
                    VmError::new(VmErrorKind::IndexOutOfBounds {
                        space: VmIndexSpace::Global,
                        owner: Some(module_name),
                        index: i64::try_from(raw_slot).unwrap_or(i64::MAX),
                        len: globals.len(),
                    })
                })?;
                self.push_value(value)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::StGlob => {
                let Operand::Global(slot) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let value = self.pop_value()?;
                let module_slot = self.current_module_slot()?;
                let module_name = self.module(module_slot)?.spec.clone();
                self.observe_heap_value(&value)?;
                let globals = &mut self.module_mut(module_slot)?.globals;
                let raw_slot = usize::try_from(slot.raw()).unwrap_or(usize::MAX);
                let len = globals.len();
                let global = globals.get_mut(raw_slot).ok_or_else(|| {
                    VmError::new(VmErrorKind::IndexOutOfBounds {
                        space: VmIndexSpace::Global,
                        owner: Some(module_name),
                        index: i64::try_from(raw_slot).unwrap_or(i64::MAX),
                        len,
                    })
                })?;
                *global = value;
                self.after_value_mutation()?;
                Ok(StepOutcome::Continue)
            }
            Opcode::LdConst => {
                let Operand::Constant(constant) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let value = {
                    let module = self.module(module_slot)?;
                    let constant = module.program.artifact().constants.get(constant);
                    self.constant_value(module_slot, &constant.value)?
                };
                self.push_value(value)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::LdSmi => {
                let Operand::I16(value) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                self.push_value(Value::Int(i64::from(value)))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::LdStr => {
                let Operand::String(value) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let text: Rc<str> = self.module(module_slot)?.program.string_text(value).into();
                self.push_value(Value::String(text))?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "load/store")),
        }
    }
}
