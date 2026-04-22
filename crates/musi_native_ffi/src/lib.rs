#![allow(unsafe_code)]

use std::fmt::Display;

use musi_vm::{ForeignCall, NativeFailureStage, Value, VmDiagKind, VmError, VmErrorKind, VmResult};
use music_base::diag::DiagContext;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct NativeFfi;

pub(crate) type NativeErrorText = Box<str>;

pub(crate) mod abi;
pub(crate) mod ffi;
pub(crate) mod loader;
pub(crate) mod marshal;
mod pointer_intrinsics;

#[cfg(test)]
mod tests;

pub use marshal::call_foreign;
pub use pointer_intrinsics::call_musi_pointer_intrinsic;

impl NativeFfi {
    #[must_use]
    pub const fn new() -> Self {
        Self
    }
}

pub(crate) const fn native_call_failed(
    foreign: NativeErrorText,
    stage: NativeFailureStage,
    subject: Option<NativeErrorText>,
    index: Option<usize>,
    detail: NativeErrorText,
) -> VmError {
    VmError::new(VmErrorKind::NativeCallFailed {
        foreign,
        stage,
        subject,
        index,
        detail,
    })
}

pub(crate) fn native_abi_detail(foreign: &ForeignCall, reason: NativeErrorText) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::AbiUnsupported,
        None::<NativeErrorText>,
        None,
        reason,
    )
}

pub(crate) fn native_result_detail(foreign: &ForeignCall, reason: NativeErrorText) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::ResultInvalid,
        None::<NativeErrorText>,
        None,
        reason,
    )
}

pub(crate) fn native_symbol_load_failed(
    foreign: &ForeignCall,
    symbol: NativeErrorText,
    detail: NativeErrorText,
) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::SymbolLoad,
        Some(symbol),
        None,
        detail,
    )
}

pub(crate) fn native_library_load_failed(
    foreign: &ForeignCall,
    library: NativeErrorText,
    detail: NativeErrorText,
) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::LibraryLoad,
        Some(library),
        None,
        detail,
    )
}

pub(crate) fn native_arg_detail(
    foreign: &ForeignCall,
    index: usize,
    reason: NativeErrorText,
) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::ArgInvalid,
        None::<NativeErrorText>,
        Some(index),
        reason,
    )
}

pub(crate) fn invalid_arg_type<T>(
    foreign: &ForeignCall,
    index: usize,
    expected: &str,
    found: &Value,
) -> VmResult<T> {
    Err(native_arg_detail(
        foreign,
        index,
        native_diag_text(
            VmDiagKind::NativeArgumentTypeMismatch,
            &DiagContext::new()
                .with("index", index)
                .with("expected", expected)
                .with("found", found.kind()),
        ),
    ))
}

pub(crate) fn native_diag_text(kind: VmDiagKind, context: &DiagContext) -> NativeErrorText {
    kind.message_with(context).into_boxed_str()
}

pub(crate) fn native_abi_issue(foreign: &ForeignCall, subject: impl Display) -> VmError {
    native_abi_detail(
        foreign,
        native_diag_text(
            VmDiagKind::NativeAbiUnsupported,
            &DiagContext::new().with("subject", subject),
        ),
    )
}

pub(crate) fn native_arg_issue(
    foreign: &ForeignCall,
    index: usize,
    subject: impl Display,
) -> VmError {
    native_arg_detail(
        foreign,
        index,
        native_diag_text(
            VmDiagKind::NativeArgumentInvalid,
            &DiagContext::new()
                .with("index", index)
                .with("subject", subject),
        ),
    )
}

pub(crate) fn native_result_issue(foreign: &ForeignCall, subject: impl Display) -> VmError {
    native_result_detail(
        foreign,
        native_diag_text(
            VmDiagKind::NativeResultInvalid,
            &DiagContext::new().with("subject", subject),
        ),
    )
}
