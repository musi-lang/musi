use music_bc::{Instruction, Opcode, Operand};

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn exec_data(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::DataNew => {
                let Operand::TypeLen { ty, len } = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let tag_value = self.pop_value()?;
                let tag = Self::expect_int(&tag_value)?;
                let fields = self.pop_args(usize::from(len))?;
                self.push_value(Value::data(ty, tag, fields))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::DataTag => {
                let data_value = self.pop_value()?;
                let data = Self::expect_data(data_value)?;
                self.push_value(Value::Int(data.borrow().tag))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::DataGet => {
                let index_value = self.pop_value()?;
                let index = Self::expect_int(&index_value)?;
                let data_value = self.pop_value()?;
                let data = Self::expect_data(data_value)?;
                let data_ref = data.borrow();
                let slot = usize::try_from(index).unwrap_or(usize::MAX);
                let value = data_ref.fields.get(slot).cloned().ok_or_else(|| {
                    VmError::new(VmErrorKind::InvalidDataIndex {
                        index,
                        len: data_ref.fields.len(),
                    })
                })?;
                drop(data_ref);
                self.push_value(value)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::DataSet => {
                let value = self.pop_value()?;
                let index_value = self.pop_value()?;
                let index = Self::expect_int(&index_value)?;
                let data_value = self.pop_value()?;
                let data = Self::expect_data(data_value)?;
                {
                    let mut data_mut = data.borrow_mut();
                    let len = data_mut.fields.len();
                    let slot = usize::try_from(index).unwrap_or(usize::MAX);
                    let field = data_mut.fields.get_mut(slot).ok_or_else(|| {
                        VmError::new(VmErrorKind::InvalidDataIndex { index, len })
                    })?;
                    *field = value;
                }
                self.push_value(Value::Data(data))?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "data")),
        }
    }
}
