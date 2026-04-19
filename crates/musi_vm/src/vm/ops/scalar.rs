use music_seam::{Instruction, Opcode};

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn binary_int_op(&mut self, op: impl FnOnce(i64, i64) -> Option<i64>) -> VmResult {
        let right_value = self.pop_value()?;
        let right = Self::expect_int(&right_value)?;
        let left_value = self.pop_value()?;
        let left = Self::expect_int(&left_value)?;
        let result = op(left, right).ok_or_else(|| {
            VmError::new(VmErrorKind::ArithmeticFailed {
                detail: "signed integer overflow".into(),
            })
        })?;
        self.push_value(Value::Int(result))
    }

    pub(crate) fn binary_float_op(&mut self, op: impl FnOnce(f64, f64) -> f64) -> VmResult {
        let right_value = self.pop_value()?;
        let right = Self::expect_float(&right_value)?;
        let left_value = self.pop_value()?;
        let left = Self::expect_float(&left_value)?;
        self.push_value(Value::Float(op(left, right)))
    }

    pub(crate) fn compare_values(&mut self, op: impl FnOnce(bool) -> bool) -> VmResult {
        let right = self.pop_value()?;
        let left = self.pop_value()?;
        let module_slot = self.current_module_slot()?;
        let equal = self.values_equal(&left, &right);
        let value = self.bool_value(module_slot, op(equal))?;
        self.push_value(value)
    }

    pub(crate) fn compare_ord<OpInt, OpNat, OpFloat, OpString>(
        &mut self,
        op_int: OpInt,
        op_nat: OpNat,
        op_float: OpFloat,
        op_string: OpString,
    ) -> VmResult
    where
        OpInt: FnOnce(i64, i64) -> bool,
        OpNat: FnOnce(u64, u64) -> bool,
        OpFloat: FnOnce(f64, f64) -> bool,
        OpString: FnOnce(&str, &str) -> bool,
    {
        let right_value = self.pop_value()?;
        let left_value = self.pop_value()?;
        let value = match (&left_value, &right_value) {
            (Value::Int(left), Value::Int(right)) => op_int(*left, *right),
            (Value::Nat(left), Value::Nat(right)) => op_nat(*left, *right),
            (Value::Float(left), Value::Float(right)) => op_float(*left, *right),
            (Value::String(left), Value::String(right)) => {
                op_string(self.heap.string(*left)?, self.heap.string(*right)?)
            }
            _ => return Err(Self::invalid_value_kind(left_value.kind(), &right_value)),
        };
        let module_slot = self.current_module_slot()?;
        let value = self.bool_value(module_slot, value)?;
        self.push_value(value)
    }

    pub(crate) fn exec_scalar(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::IAdd => {
                self.binary_int_op(i64::checked_add)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::ISub => {
                self.binary_int_op(i64::checked_sub)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::IMul => {
                self.binary_int_op(i64::checked_mul)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::IDiv => {
                self.binary_int_op(i64::checked_div)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::IRem => {
                self.binary_int_op(i64::checked_rem)?;
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
                let right = self.expect_string_value(right_value)?;
                let left_value = self.pop_value()?;
                let left = self.expect_string_value(left_value)?;
                let text = format!("{left}{right}");
                let value = self.alloc_string(text)?;
                self.push_value(value)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpEq => {
                self.compare_values(|equal| equal)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpNe => {
                self.compare_values(|equal| !equal)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpLt => {
                self.compare_ord(
                    |left, right| left < right,
                    |left, right| left < right,
                    |left, right| left < right,
                    |left, right| left < right,
                )?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpGt => {
                self.compare_ord(
                    |left, right| left > right,
                    |left, right| left > right,
                    |left, right| left > right,
                    |left, right| left > right,
                )?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpLe => {
                self.compare_ord(
                    |left, right| left <= right,
                    |left, right| left <= right,
                    |left, right| left <= right,
                    |left, right| left <= right,
                )?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CmpGe => {
                self.compare_ord(
                    |left, right| left >= right,
                    |left, right| left >= right,
                    |left, right| left >= right,
                    |left, right| left >= right,
                )?;
                Ok(StepOutcome::Continue)
            }
            _ => Err(Self::invalid_dispatch(instruction, "scalar")),
        }
    }
}
