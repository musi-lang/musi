//! `msc_vm` - Musi bytecode interpreter.
//!
//! Loads and executes `.muse` binaries produced by the `msc_emit` compiler
//! crate. Has no dependency on any `msc_*` compiler crate; operates solely
//! on raw bytes per §11 of the Musi bytecode spec.
//!
//! # Quick start
//!
//! ```ignore
//! use msc_vm::{load, verify, Vm};
//!
//! let bytes: &[u8] = /* read your .muse file */;
//! let module = load(bytes)?;
//! verify(&module)?;
//! let mut vm = Vm::new(module);
//! let result = vm.run()?;
//! ```

mod channel;
mod error;
mod heap;
mod host;
mod loader;
mod task;
mod value;
mod verifier;
mod vm;

pub use error::VmError;
pub use heap::Heap;
pub use host::HostFunctions;
pub use loader::{
    HandlerEntry, LoadedConst, LoadedEffect, LoadedFn, LoadedForeignFn, LoadedModule, load,
};
pub use value::Value;
pub use verifier::verify;
pub use vm::{Frame, StepResult, Vm};
