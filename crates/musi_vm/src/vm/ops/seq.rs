use std::rc::Rc;

use music_seam::{Instruction, Opcode, Operand};

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn exec_seq(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::SeqNew => {
                let Operand::TypeLen { ty, len } = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let items = self.pop_args(usize::from(len))?;
                self.push_value(Value::sequence(ty, items))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::SeqGet => {
                let index_value = self.pop_value()?;
                let index = Self::expect_int(&index_value)?;
                let seq_value = self.pop_value()?;
                let seq = Self::expect_seq(seq_value)?;
                let seq_ref = seq.borrow();
                let slot = usize::try_from(index).unwrap_or(usize::MAX);
                let value = seq_ref.items.get(slot).cloned().ok_or_else(|| {
                    VmError::new(VmErrorKind::InvalidSequenceIndex {
                        index,
                        len: seq_ref.items.len(),
                    })
                })?;
                drop(seq_ref);
                self.push_value(value)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::SeqGetN => {
                let Operand::I16(len) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let indices = self.pop_index_list(len)?;
                let seq_value = self.pop_value()?;
                let seq = Self::expect_seq(seq_value)?;
                let value = Self::get_nested_seq(seq, &indices)?;
                self.push_value(value)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::SeqSet => {
                let value = self.pop_value()?;
                let index_value = self.pop_value()?;
                let index = Self::expect_int(&index_value)?;
                let seq_value = self.pop_value()?;
                let seq = Self::expect_seq(seq_value)?;
                {
                    let mut seq_mut = seq.borrow_mut();
                    let len = seq_mut.items.len();
                    let slot = usize::try_from(index).unwrap_or(usize::MAX);
                    let item = seq_mut.items.get_mut(slot).ok_or_else(|| {
                        VmError::new(VmErrorKind::InvalidSequenceIndex { index, len })
                    })?;
                    *item = value;
                }
                self.push_value(Value::Seq(seq))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::SeqSetN => {
                let Operand::I16(len) = instruction.operand else {
                    return Err(Self::invalid_operand(instruction));
                };
                let value = self.pop_value()?;
                let indices = self.pop_index_list(len)?;
                let seq_value = self.pop_value()?;
                let seq = Self::expect_seq(seq_value)?;
                Self::set_nested_seq(Rc::clone(&seq), &indices, value)?;
                self.push_value(Value::Seq(seq))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::SeqCat => {
                let right_value = self.pop_value()?;
                let right = Self::expect_seq(right_value)?;
                let left_value = self.pop_value()?;
                let left = Self::expect_seq(left_value)?;
                let left = left.borrow();
                let right = right.borrow();
                let mut items = left.items.clone();
                items.extend(right.items.iter().cloned());
                self.push_value(Value::sequence(left.ty, items))?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "sequence")),
        }
    }
}
