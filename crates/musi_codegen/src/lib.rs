//! Bytecode format and serialization for the Musi compiler.
//!
//! Defines the `.mso` binary container format, the [`Opcode`] instruction set,
//! and the [`Module`] type that holds a compiled Musi program.

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

pub mod emitter;
pub mod error;
pub mod module;
pub mod opcode;

pub use emitter::{emit, CodegenError};
pub use error::DeserError;
pub use module::{ConstEntry, FunctionEntry, Module, SymbolEntry, SymbolFlags};
pub use opcode::Opcode;
