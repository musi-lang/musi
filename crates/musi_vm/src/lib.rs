//! Stack-based virtual machine for the Musi language.
//!
//! Executes `.mso` modules produced by `musi_codegen`.  The VM uses a
//! call-stack of [`vm::CallFrame`]s and a shared operand stack of [`Value`]s.
//! Built-in functions are dispatched via [`NativeRegistry`] (new system
//! modules) and the legacy `native::dispatch` match (VM primitives 0–23).

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

pub mod error;
pub mod ffi;
pub mod native;
pub mod native_registry;
pub mod value;
pub mod vm;

pub use error::VmError;
pub use native_registry::{NativeFn, NativeModuleEntry, NativeRegistry};
pub use value::Value;
pub use vm::{TestResult, Vm};
