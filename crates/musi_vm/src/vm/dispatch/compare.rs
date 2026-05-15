use super::*;

impl Vm {
    pub(super) fn exec_compare_branch(
        &mut self,
        compare: CompareOp,
        target: usize,
    ) -> VmResult<StepOutcome> {
        let right = self.pop_value()?;
        let left = self.pop_value()?;
        if self.compare_for_branch(compare, &left, &right)? {
            self.skip_next_instruction()?;
        } else {
            self.jump_to_ip(target)?;
        }
        Ok(StepOutcome::Continue)
    }
}

impl Vm {
    fn compare_for_branch(
        &self,
        compare: CompareOp,
        left: &Value,
        right: &Value,
    ) -> VmResult<bool> {
        match compare {
            CompareOp::Eq => Ok(self.values_equal(left, right)),
            CompareOp::Ne => Ok(!self.values_equal(left, right)),
            CompareOp::Lt => self.compare_order_for_branch(left, right, Ordering::is_lt),
            CompareOp::Gt => self.compare_order_for_branch(left, right, Ordering::is_gt),
            CompareOp::Le => {
                self.compare_order_for_branch(left, right, |ordering| !ordering.is_gt())
            }
            CompareOp::Ge => {
                self.compare_order_for_branch(left, right, |ordering| !ordering.is_lt())
            }
        }
    }

    fn compare_order_for_branch(
        &self,
        left: &Value,
        right: &Value,
        op: impl FnOnce(Ordering) -> bool,
    ) -> VmResult<bool> {
        let ordering = match (left, right) {
            (Value::Int(left), Value::Int(right)) => left.cmp(right),
            (Value::Nat(left), Value::Nat(right)) => left.cmp(right),
            (Value::Nat(left), Value::Int(right)) => {
                let Ok(right) = u64::try_from(*right) else {
                    return Ok(op(Ordering::Greater));
                };
                left.cmp(&right)
            }
            (Value::Int(left), Value::Nat(right)) => {
                let Ok(left) = u64::try_from(*left) else {
                    return Ok(op(Ordering::Less));
                };
                left.cmp(right)
            }
            (Value::Float(left), Value::Float(right)) => left.total_cmp(right),
            (Value::String(left), Value::String(right)) => {
                self.heap.string(*left)?.cmp(self.heap.string(*right)?)
            }
            _ => return Err(Self::invalid_value_kind(left.kind(), right)),
        };
        Ok(op(ordering))
    }
}
