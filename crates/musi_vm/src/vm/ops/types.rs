use music_seam::{Instruction, Opcode, Operand, TypeId};

use crate::{BitsValue, VmValueKind};

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn value_matches_type(&self, module_slot: usize, value: &Value, ty: TypeId) -> bool {
        let ty_name = self
            .module(module_slot)
            .map_or("", |module| module.program.type_name(ty));
        match ty_name {
            "Any" => true,
            "Unit" => matches!(value, Value::Unit),
            "Int" | "Int64" => matches!(value, Value::Int(_)),
            "Int8" => int_value_in_range(value, i64::from(i8::MIN), i64::from(i8::MAX)),
            "Int16" => int_value_in_range(value, i64::from(i16::MIN), i64::from(i16::MAX)),
            "Int32" => int_value_in_range(value, i64::from(i32::MIN), i64::from(i32::MAX)),
            "Nat" | "Nat64" => nat_value(value),
            "Nat8" => nat_value_in_range(value, u64::from(u8::MAX)),
            "Nat16" => nat_value_in_range(value, u64::from(u16::MAX)),
            "Nat32" => nat_value_in_range(value, u64::from(u32::MAX)),
            "Float" | "Float32" | "Float64" => matches!(value, Value::Float(_)),
            "String" | "CString" => matches!(value, Value::String(_)),
            "Syntax" => matches!(value, Value::Syntax(_)),
            "Module" => matches!(value, Value::Module(_)),
            _ if bits_width_from_type_name(ty_name).is_some() => {
                matches!(value, Value::Bits(bits) if Some(bits.width()) == bits_width_from_type_name(ty_name))
            }
            _ => match value {
                Value::Seq(seq) => self.heap.sequence_ty(*seq).is_ok_and(|seq_ty| seq_ty == ty),
                Value::Data(data) => self.heap.data(*data).is_ok_and(|data| data.ty == ty),
                Value::Type(id) => *id == ty,
                _ => false,
            },
        }
    }

    pub(crate) fn exec_type(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::LdType => {
                let Operand::Type(ty) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                self.push_value(Value::Type(ty))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::Call => {
                let Operand::I16(count) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let count =
                    usize::try_from(count).map_err(|_| Self::invalid_operand(instruction))?;
                let mut type_args = Vec::with_capacity(count);
                for _ in 0..count {
                    let value = self.pop_value()?;
                    let Value::Type(ty) = value else {
                        return Err(Self::invalid_value_kind(VmValueKind::Type, &value));
                    };
                    type_args.push(ty);
                }
                type_args.reverse();
                let value = self.pop_value()?;
                let applied = match value {
                    Value::Foreign(foreign) => {
                        Value::Foreign(foreign.with_type_args(type_args.into_boxed_slice()))
                    }
                    other => other,
                };
                self.push_value(applied)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::IsInst => {
                let Operand::Type(ty) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let value = self.pop_value()?;
                let matches_type = self.value_matches_type(module_slot, &value, ty);
                let value = self.bool_value(module_slot, matches_type)?;
                self.push_value(value)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::Cast => {
                let Operand::Type(ty) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let module_slot = self.current_module_slot()?;
                let value = self.pop_value()?;
                if self.value_matches_type(module_slot, &value, ty) {
                    self.push_value(value)?;
                    Ok(StepOutcome::Continue)
                } else if let Some(value) = self.cast_value(module_slot, &value, ty)? {
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

    fn cast_value(&self, module_slot: usize, value: &Value, ty: TypeId) -> VmResult<Option<Value>> {
        let ty_name = self.module(module_slot)?.program.type_name(ty);
        if let Some(width) = bits_width_from_type_name(ty_name) {
            return Ok(match value {
                Value::Nat(value) => Some(Value::Bits(BitsValue::from_u64(width, *value))),
                Value::Int(value) => u64::try_from(*value)
                    .ok()
                    .map(|value| Value::Bits(BitsValue::from_u64(width, value))),
                _ => None,
            });
        }
        match (ty_name, value) {
            ("Nat" | "Nat64", Value::Bits(bits)) => Ok(bits.to_u64().map(Value::Nat)),
            ("Int" | "Int64", Value::Bits(bits)) => Ok(bits
                .to_u64()
                .and_then(|value| i64::try_from(value).ok())
                .map(Value::Int)),
            _ => Ok(None),
        }
    }
}

fn bits_width_from_type_name(name: &str) -> Option<u32> {
    let inner = name.strip_prefix("Bits[")?.strip_suffix(']')?;
    let width = inner.parse::<u32>().ok()?;
    (width > 0).then_some(width)
}

fn int_value_in_range(value: &Value, min: i64, max: i64) -> bool {
    matches!(value, Value::Int(value) if (min..=max).contains(value))
}

const fn nat_value(value: &Value) -> bool {
    matches!(value, Value::Nat(_)) || matches!(value, Value::Int(value) if *value >= 0)
}

fn nat_value_in_range(value: &Value, max: u64) -> bool {
    match value {
        Value::Nat(value) => *value <= max,
        Value::Int(value) => u64::try_from(*value).is_ok_and(|value| value <= max),
        _ => false,
    }
}
