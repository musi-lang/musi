use std::fmt::Display;

use musi_vm::{ForeignCall, VmError, VmErrorKind};

pub(super) fn invalid_runtime_args(
    foreign: &ForeignCall,
    expected: &str,
    found: impl Display,
) -> VmError {
    let detail = format!("{} expected {expected}, found {found}", foreign.name());
    VmError::new(VmErrorKind::ForeignCallRejected {
        foreign: detail.into_boxed_str(),
    })
}

pub(super) fn runtime_foreign_failed(foreign: &ForeignCall, source: impl Display) -> VmError {
    let detail = format!("{} failed ({source})", foreign.name());
    VmError::new(VmErrorKind::ForeignCallRejected {
        foreign: detail.into_boxed_str(),
    })
}

pub(super) fn runtime_host_unavailable(foreign: &ForeignCall, subject: &str) -> VmError {
    let detail = format!("{} host unavailable ({subject})", foreign.name());
    VmError::new(VmErrorKind::ForeignCallRejected {
        foreign: detail.into_boxed_str(),
    })
}

pub(super) fn runtime_foreign_unsupported(foreign: &ForeignCall) -> VmError {
    VmError::new(VmErrorKind::ForeignCallRejected {
        foreign: foreign.name().into(),
    })
}

pub(super) fn foreign_rejected(foreign: &ForeignCall) -> VmError {
    VmError::new(VmErrorKind::ForeignCallRejected {
        foreign: foreign.name().into(),
    })
}
