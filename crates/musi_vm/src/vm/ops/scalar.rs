use std::cmp::Ordering;

use music_seam::{Instruction, Opcode};

use crate::VmValueKind;

use super::{StepOutcome, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    fn numeric_op(
        &mut self,
        int_op: impl FnOnce(i64, i64) -> Option<i64>,
        float_op: impl FnOnce(f64, f64) -> f64,
    ) -> VmResult<StepOutcome> {
        let right_value = self.pop_value()?;
        let left_value = self.pop_value()?;
        match (left_value, right_value) {
            (Value::Int(left), Value::Int(right)) => {
                let result = int_op(left, right).ok_or_else(|| {
                    VmError::new(VmErrorKind::ArithmeticFailed {
                        detail: "signed integer overflow".into(),
                    })
                })?;
                self.push_value(Value::Int(result))?;
            }
            (Value::Nat(left), Value::Nat(right)) => {
                let left = i64::try_from(left).map_err(|_| {
                    VmError::new(VmErrorKind::ArithmeticFailed {
                        detail: "natural integer exceeds signed range".into(),
                    })
                })?;
                let right = i64::try_from(right).map_err(|_| {
                    VmError::new(VmErrorKind::ArithmeticFailed {
                        detail: "natural integer exceeds signed range".into(),
                    })
                })?;
                let result = int_op(left, right).ok_or_else(|| {
                    VmError::new(VmErrorKind::ArithmeticFailed {
                        detail: "signed integer overflow".into(),
                    })
                })?;
                self.push_value(Value::Int(result))?;
            }
            (Value::Float(left), Value::Float(right)) => {
                self.push_value(Value::Float(float_op(left, right)))?;
            }
            (left, right) => return Err(Self::invalid_value_kind(left.kind(), &right)),
        }
        Ok(StepOutcome::Continue)
    }

    pub(crate) fn compare_values(&mut self, op: impl FnOnce(bool) -> bool) -> VmResult {
        let right = self.pop_value()?;
        let left = self.pop_value()?;
        let module_slot = self.current_module_slot()?;
        let equal = self.values_equal(&left, &right);
        let value = self.bool_value(module_slot, op(equal))?;
        self.push_value(value)
    }

    pub(crate) fn compare_ord(&mut self, op: impl FnOnce(Ordering) -> bool) -> VmResult {
        let right_value = self.pop_value()?;
        let left_value = self.pop_value()?;
        let ordering = match (&left_value, &right_value) {
            (Value::Int(left), Value::Int(right)) => left.cmp(right),
            (Value::Nat(left), Value::Nat(right)) => left.cmp(right),
            (Value::Nat(left), Value::Int(right)) => {
                let Ok(right) = u64::try_from(*right) else {
                    let module_slot = self.current_module_slot()?;
                    let value = self.bool_value(module_slot, op(Ordering::Greater))?;
                    self.push_value(value)?;
                    return Ok(());
                };
                left.cmp(&right)
            }
            (Value::Int(left), Value::Nat(right)) => {
                let Ok(left) = u64::try_from(*left) else {
                    let module_slot = self.current_module_slot()?;
                    let value = self.bool_value(module_slot, op(Ordering::Less))?;
                    self.push_value(value)?;
                    return Ok(());
                };
                left.cmp(right)
            }
            (Value::Float(left), Value::Float(right)) => left.total_cmp(right),
            (Value::String(left), Value::String(right)) => {
                self.heap.string(*left)?.cmp(self.heap.string(*right)?)
            }
            _ => return Err(Self::invalid_value_kind(left_value.kind(), &right_value)),
        };
        let module_slot = self.current_module_slot()?;
        let value = self.bool_value(module_slot, op(ordering))?;
        self.push_value(value)
    }

    pub(crate) fn exec_scalar(&mut self, instruction: &Instruction) -> VmResult<StepOutcome> {
        match instruction.opcode {
            Opcode::Add => self.numeric_op(i64::checked_add, |left, right| left + right),
            Opcode::Sub => self.numeric_op(i64::checked_sub, |left, right| left - right),
            Opcode::Mul => self.numeric_op(i64::checked_mul, |left, right| left * right),
            Opcode::DivS => self.numeric_op(i64::checked_div, |left, right| left / right),
            Opcode::RemS => self.numeric_op(i64::checked_rem, |left, right| left % right),
            Opcode::Call => {
                let right_value = self.pop_value()?;
                let right = self.expect_string_value(right_value)?;
                let left_value = self.pop_value()?;
                let left = self.expect_string_value(left_value)?;
                let text = format!("{left}{right}");
                let value = self.alloc_string(text)?;
                self.push_value(value)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::Ceq => {
                self.compare_values(|equal| equal)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::Cne => {
                self.compare_values(|equal| !equal)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CltS => {
                self.compare_ord(Ordering::is_lt)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CgtS => {
                self.compare_ord(Ordering::is_gt)?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CleS => {
                self.compare_ord(|ordering| !ordering.is_gt())?;
                Ok(StepOutcome::Continue)
            }
            Opcode::CgeS => {
                self.compare_ord(|ordering| !ordering.is_lt())?;
                Ok(StepOutcome::Continue)
            }
            Opcode::And => self.logical_binary_op(BoolOp::And),
            Opcode::Or => self.logical_binary_op(BoolOp::Or),
            Opcode::Xor => self.logical_binary_op(BoolOp::Xor),
            Opcode::Not => self.logical_not(),
            _ => Err(Self::invalid_dispatch(instruction, "scalar")),
        }
    }

    fn logical_binary_op(&mut self, op: BoolOp) -> VmResult<StepOutcome> {
        let right = self.pop_value()?;
        let left = self.pop_value()?;
        let module_slot = self.current_module_slot()?;
        if let (Some(left), Some(right)) = (self.bool_flag(&left), self.bool_flag(&right)) {
            let value = match op {
                BoolOp::And => left && right,
                BoolOp::Or => left || right,
                BoolOp::Xor => left ^ right,
            };
            let value = self.bool_value(module_slot, value)?;
            self.push_value(value)?;
            return Ok(StepOutcome::Continue);
        }
        if let (Value::Bits(left), Value::Bits(right)) = (&left, &right) {
            let value = match op {
                BoolOp::And => left.and(right),
                BoolOp::Or => left.or(right),
                BoolOp::Xor => left.xor(right),
            }
            .ok_or_else(|| {
                VmError::new(VmErrorKind::ArithmeticFailed {
                    detail: "bits width mismatch".into(),
                })
            })?;
            self.push_value(Value::Bits(value))?;
            return Ok(StepOutcome::Continue);
        }
        Err(Self::invalid_value_kind(left.kind(), &right))
    }

    fn logical_not(&mut self) -> VmResult<StepOutcome> {
        let value = self.pop_value()?;
        if let Some(flag) = self.bool_flag(&value) {
            let module_slot = self.current_module_slot()?;
            let value = self.bool_value(module_slot, !flag)?;
            self.push_value(value)?;
            return Ok(StepOutcome::Continue);
        }
        if let Value::Bits(value) = value {
            self.push_value(Value::Bits(value.not()))?;
            return Ok(StepOutcome::Continue);
        }
        Err(Self::invalid_value_kind(VmValueKind::Bool, &value))
    }
}

#[derive(Clone, Copy)]
enum BoolOp {
    And,
    Or,
    Xor,
}
