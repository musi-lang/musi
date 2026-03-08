//! Bytecode format and serialization for the Musi compiler.
//!
//! Defines the `.mso` binary container format, the [`Opcode`] instruction set,
//! and the [`Module`] type that holds a compiled Musi program.

pub mod emitter;
pub mod error;
pub mod intrinsics;
pub mod module;
pub mod opcode;

pub use emitter::emit;
pub use error::{CodegenError, DeserError};
pub use module::{ConstEntry, FunctionEntry, Module, SymbolEntry, SymbolFlags};
pub use opcode::Opcode;
