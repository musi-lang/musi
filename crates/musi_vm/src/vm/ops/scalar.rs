use std::rc::Rc;

use music_seam::{Instruction, Opcode};

use super::{StepOutcome, Value, Vm, VmResult};

impl Vm {
    pub(crate) fn binary_int_op(&mut self, op: impl FnOnce(i64, i64) -> i64) -> VmResult {
        let right_value = self.pop_value()?;
        let right = Self::expect_int(&right_value)?;
        let left_value = self.pop_value()?;
        let left = Self::expect_int(&left_value)?;
        self.push_value(Value::Int(op(left, right)))
    }

    pub(crate) fn binary_float_op(&mut self, op: impl FnOnce(f64, f64) -> f64) -> VmResult {
        let right_value = self.pop_value()?;
        let right = Self::expect_float(&right_value)?;
        let left_value = self.pop_value()?;
        let left = Self::expect_float(&left_value)?;
        self.push_value(Value::Float(op(left, right)))
    }

    pub(crate) fn compare_values(&mut self, op: impl FnOnce(&Value, &Value) -> bool) -> VmResult {
        let right = self.pop_value()?;
        let left = self.pop_value()?;
        let module_slot = self.current_module_slot()?;
        self.push_value(self.bool_value(module_slot, op(&left, &right))?)
    }

    pub(crate) fn compare_ord(&mut self, op: impl FnOnce(i64, i64) -> bool) -> VmResult {
        let right_value = self.pop_value()?;
        let right = Self::expect_int(&right_value)?;
        let left_value = self.pop_value()?;
        let left = Self::expect_int(&left_value)?;
        let module_slot = self.current_module_slot()?;
        self.push_value(self.bool_value(module_slot, op(left, right))?)
    }

    pub(crate) fn exec_scalar(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::IAdd => {
                self.binary_int_op(i64::saturating_add)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::ISub => {
                self.binary_int_op(i64::saturating_sub)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::IMul => {
                self.binary_int_op(i64::saturating_mul)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::IDiv => {
                self.binary_int_op(|left, right| left / right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::IRem => {
                self.binary_int_op(|left, right| left % right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::FAdd => {
                self.binary_float_op(|left, right| left + right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::FSub => {
                self.binary_float_op(|left, right| left - right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::FMul => {
                self.binary_float_op(|left, right| left * right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::FDiv => {
                self.binary_float_op(|left, right| left / right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::FRem => {
                self.binary_float_op(|left, right| left % right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::StrCat => {
                let right_value = self.pop_value()?;
                let right = Self::expect_string_value(right_value)?;
                let left_value = self.pop_value()?;
                let left = Self::expect_string_value(left_value)?;
                let text: Rc<str> = format!("{left}{right}").into();
                self.push_value(Value::String(text))?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpEq => {
                self.compare_values(PartialEq::eq)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpNe => {
                self.compare_values(PartialEq::ne)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpLt => {
                self.compare_ord(|left, right| left < right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpGt => {
                self.compare_ord(|left, right| left > right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpLe => {
                self.compare_ord(|left, right| left <= right)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpGe => {
                self.compare_ord(|left, right| left >= right)?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "scalar")),
        }
    }
}
