//! `musi_vm` — Musi bytecode interpreter.
//!
//! Loads and executes `.msbc` binaries produced by the `music_emit` compiler
//! crate. Has no dependency on any `music_*` compiler crate; operates solely
//! on raw bytes per §11 of the Musi bytecode spec.
//!
//! # Quick start
//!
//! ```ignore
//! use musi_vm::{load, verify, Vm};
//!
//! let bytes: &[u8] = /* read your .msbc file */;
//! let module = load(bytes)?;
//! verify(&module)?;
//! let mut vm = Vm::new(module);
//! let result = vm.run()?;
//! ```

mod channel;
mod error;
mod heap;
mod loader;
pub(crate) mod opcode;
mod task;
mod value;
mod verifier;
mod vm;

pub use error::VmError;
pub use heap::Heap;
pub use loader::{HandlerEntry, LoadedConst, LoadedEffect, LoadedFn, LoadedModule, load};
pub use value::Value;
pub use verifier::verify;
pub use vm::{Frame, StepResult, Vm};
