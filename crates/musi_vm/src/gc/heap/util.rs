use crate::VmValueKind::{Int, Nat};
use crate::error::{VmError, VmErrorKind};
use crate::types::VmResult;
use crate::value::Value;

pub(super) fn fast_int(value: &Value) -> VmResult<i64> {
    match value {
        Value::Int(value) => Ok(*value),
        Value::Nat(value) => i64::try_from(*value).map_err(|_| {
            VmError::new(VmErrorKind::InvalidValueKind {
                expected: Int,
                found: Nat,
            })
        }),
        other => Err(VmError::new(VmErrorKind::InvalidValueKind {
            expected: Int,
            found: other.kind(),
        })),
    }
}

pub(super) fn arithmetic_overflow() -> VmError {
    VmError::new(VmErrorKind::ArithmeticFailed {
        detail: "signed integer overflow".into(),
    })
}
