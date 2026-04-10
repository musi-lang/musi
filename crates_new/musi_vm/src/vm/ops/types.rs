use music_bc::{Instruction, Opcode, Operand, TypeId};

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn value_matches_type(&self, module_slot: usize, value: &Value, ty: TypeId) -> bool {
        let ty_name = self
            .module(module_slot)
            .map_or("", |module| module.program.type_name(ty));
        match ty_name {
            "Any" => true,
            "Unit" => matches!(value, Value::Unit),
            "Int" => matches!(value, Value::Int(_)),
            "Float" => matches!(value, Value::Float(_)),
            "Bool" => matches!(value, Value::Bool(_)),
            "String" | "CString" | "Syntax" => matches!(value, Value::String(_)),
            "Module" => matches!(value, Value::Module(_)),
            _ => match value {
                Value::Seq(seq) => seq.borrow().ty == ty,
                Value::Data(data) => data.borrow().ty == ty,
                Value::Type(id) => *id == ty,
                _ => false,
            },
        }
    }

    pub(crate) fn exec_type(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::TyId => {
                let Operand::Type(ty) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                self.push_value(Value::Type(ty))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::TyChk => {
                let Operand::Type(ty) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let value = self.pop_value()?;
                self.push_value(Value::Bool(self.value_matches_type(
                    module_slot,
                    &value,
                    ty,
                )))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::TyCast => {
                let Operand::Type(ty) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let value = self.pop_value()?;
                if self.value_matches_type(module_slot, &value, ty) {
                    self.push_value(value)?;
                    Ok(StepOutcome::Continue)
                } else {
                    Err(VmError::new(VmErrorKind::InvalidTypeCast {
                        expected: self.module(module_slot)?.program.type_name(ty).into(),
                        found: value.kind(),
                    }))
                }
            }
            _ => Err(Self::invalid_dispatch(instruction, "type")),
        }
    }
}
