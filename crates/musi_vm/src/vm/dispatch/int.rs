use super::*;

#[inline]
pub(super) fn int_from_value(value: &Value) -> VmResult<i64> {
    match value {
        Value::Int(value) => Ok(*value),
        Value::Nat(number) => i64::try_from(*number)
            .map_err(|_| Vm::invalid_value_kind(VmValueKind::Int, &Value::Nat(*number))),
        other => Err(Vm::invalid_value_kind(VmValueKind::Int, other)),
    }
}

#[inline]
pub(super) fn int_overflow_error() -> VmError {
    VmError::new(VmErrorKind::ArithmeticFailed {
        detail: "signed integer overflow".into(),
    })
}

#[inline]
pub(super) const fn compare_int_for_branch(compare: CompareOp, left: i64, right: i64) -> bool {
    match compare {
        CompareOp::Eq => left == right,
        CompareOp::Ne => left != right,
        CompareOp::Lt => left < right,
        CompareOp::Gt => left > right,
        CompareOp::Le => left <= right,
        CompareOp::Ge => left >= right,
    }
}

pub(super) fn pop_int_from_stack(frame: &mut CallFrame) -> VmResult<i64> {
    let value = frame.stack.pop().ok_or_else(|| {
        VmError::new(VmErrorKind::StackEmpty {
            stack: VmStackKind::Operand,
        })
    })?;
    match value {
        Value::Int(value) => Ok(value),
        Value::Nat(value) => i64::try_from(value)
            .map_err(|_| Vm::invalid_value_kind(VmValueKind::Int, &Value::Nat(value))),
        other => Err(Vm::invalid_value_kind(VmValueKind::Int, &other)),
    }
}

pub(super) fn local_int_from_frame(frame: &CallFrame, slot: u16) -> VmResult<i64> {
    let index = usize::from(slot);
    let Some(value) = frame.locals.get(index) else {
        return Err(VmError::new(VmErrorKind::IndexOutOfBounds {
            space: VmIndexSpace::Local,
            owner: None,
            index: i64::from(slot),
            len: frame.locals.len(),
        }));
    };
    match value {
        Value::Int(value) => Ok(*value),
        Value::Nat(number) => i64::try_from(*number)
            .map_err(|_| Vm::invalid_value_kind(VmValueKind::Int, &Value::Nat(*number))),
        other => Err(Vm::invalid_value_kind(VmValueKind::Int, other)),
    }
}
