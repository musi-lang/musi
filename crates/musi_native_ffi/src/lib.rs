#![allow(unsafe_code)]

use musi_vm::{ForeignCall, NativeFailureStage, Value, VmError, VmErrorKind, VmResult};

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

pub(crate) fn native_abi_unsupported(foreign: &ForeignCall, reason: NativeErrorText) -> VmError {
    native_call_failed(
        foreign.name().into(),
        NativeFailureStage::AbiUnsupported,
        None::<NativeErrorText>,
        None,
        reason,
    )
}

pub(crate) fn native_result_invalid(foreign: &ForeignCall, reason: NativeErrorText) -> VmError {
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

pub(crate) fn native_arg_invalid(
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
    Err(native_arg_invalid(
        foreign,
        index,
        format!("expected `{expected}`, found `{:?}`", found.kind()).into(),
    ))
}
