//! SEAM bytecode emitter for checked HIR modules (clean-room).
//!
//! This crate turns `music_check` output into `music_il::SeamArtifact`.

mod emitter;
mod errors;
mod model;

pub use emitter::{emit_program, emit_single_program};
pub use errors::{EmitError, EmitErrorKind, EmitResult};
pub use model::{EmitModule, EmitProgram, ProgramArtifact};
