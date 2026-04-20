use music_seam::{Instruction, Opcode, Operand, TypeId};

use super::{
    RuntimeInstruction, RuntimeOperand, StepOutcome, Value, ValueList, Vm, VmError, VmErrorKind,
    VmResult,
};

impl Vm {
    pub(crate) fn exec_data(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::DataNew => {
                let Operand::TypeLen { ty, len } = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                self.exec_data_new(ty, len)
            }
            Opcode::DataTag => self.exec_data_tag(),
            Opcode::DataGet => self.exec_data_get(),
            Opcode::DataSet => self.exec_data_set(),
            _ => Err(Self::invalid_dispatch(instruction, "data")),
        }
    }

    pub(crate) fn exec_fast_data(&mut self, runtime: &RuntimeInstruction) -> VmResult<StepOutcome> {
        match runtime.opcode {
            Opcode::DataNew => {
                let RuntimeOperand::TypeLen { ty, len } = runtime.operand else {
                    let instruction = self.current_raw_instruction(runtime.raw_index)?;
                    return Err(Self::invalid_operand(&instruction));
                };
                self.exec_data_new(ty, len)
            }
            Opcode::DataTag => self.exec_data_tag(),
            Opcode::DataGet => self.exec_data_get(),
            Opcode::DataSet => self.exec_data_set(),
            _ => {
                let instruction = self.current_raw_instruction(runtime.raw_index)?;
                Err(Self::invalid_dispatch(&instruction, "data"))
            }
        }
    }

    fn exec_data_new(&mut self, ty: TypeId, len: u16) -> VmResult<StepOutcome> {
        let tag_value = self.pop_value()?;
        let tag = Self::expect_int(&tag_value)?;
        let fields = if len == 1 {
            let mut fields = ValueList::new();
            fields.push(self.pop_value()?);
            fields
        } else {
            self.pop_args(usize::from(len))?
        };
        let value = self.alloc_data_owned(ty, tag, fields)?;
        self.push_value(value)?;
        Ok(StepOutcome::Continue)
    }

    fn exec_data_tag(&mut self) -> VmResult<StepOutcome> {
        let data_value = self.pop_value()?;
        let data = Self::expect_data(data_value)?;
        let tag = self.heap.data(data)?.tag;
        if self.try_branch_table_after_data_tag(tag)? {
            return Ok(StepOutcome::Continue);
        }
        self.push_value(Value::Int(tag))?;
        Ok(StepOutcome::Continue)
    }

    fn exec_data_get(&mut self) -> VmResult<StepOutcome> {
        let index = Self::expect_int(&self.pop_value()?)?;
        let data = Self::expect_data(self.pop_value()?)?;
        let data_ref = self.heap.data(data)?;
        let slot = usize::try_from(index).unwrap_or(usize::MAX);
        let Some(field) = data_ref.fields.get(slot) else {
            return Err(VmError::new(VmErrorKind::InvalidDataIndex {
                index,
                len: data_ref.fields.len(),
            }));
        };
        self.push_value(field.clone())?;
        Ok(StepOutcome::Continue)
    }

    fn exec_data_set(&mut self) -> VmResult<StepOutcome> {
        let value = self.pop_value()?;
        let index = Self::expect_int(&self.pop_value()?)?;
        let data = Self::expect_data(self.pop_value()?)?;
        {
            let data_mut = self.heap.data_mut(data)?;
            let len = data_mut.fields.len();
            let slot = usize::try_from(index).unwrap_or(usize::MAX);
            let Some(field) = data_mut.fields.get_mut(slot) else {
                return Err(VmError::new(VmErrorKind::InvalidDataIndex { index, len }));
            };
            *field = value;
        }
        self.push_value(Value::Data(data))?;
        Ok(StepOutcome::Continue)
    }
}
