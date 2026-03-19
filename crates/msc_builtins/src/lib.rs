//! `msc_builtins` - Musi standard library and FFI runtime.
//!
//! Provides the [`StdHost`] implementation of [`HostFunctions`] that
//! bridges `INV_FFI` opcodes to native C functions via `libffi` and
//! `libloading`.

#![allow(unsafe_code)]

mod core;
mod ffi;

pub use ffi::{StdHost, set_test_filter};
pub use msc_vm::HostFunctions;
