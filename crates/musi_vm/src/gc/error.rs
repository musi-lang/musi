use crate::error::{VmError, VmErrorKind};
use crate::value::GcRef;

pub(super) fn stale_heap_ref(reference: GcRef) -> VmError {
    VmError::new(VmErrorKind::InvalidProgramShape {
        detail: format!(
            "stale heap reference `{}` generation `{}`",
            reference.slot(),
            reference.generation()
        )
        .into(),
    })
}

pub(super) fn invalid_heap_ref(reference: GcRef, expected: &'static str) -> VmError {
    VmError::new(VmErrorKind::InvalidProgramShape {
        detail: format!(
            "heap reference `{}` generation `{}` not `{expected}`",
            reference.slot(),
            reference.generation()
        )
        .into(),
    })
}
