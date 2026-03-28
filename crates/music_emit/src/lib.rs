mod emitter;
pub mod error;
pub mod pool;
pub mod project;
pub mod writer;

pub use emitter::{GlobalEntry, MethodEntry, SeamModule, emit, emit_with_context};
pub use error::EmitError;
pub use pool::{ConstantEntry, ConstantPool};
pub use project::{ProjectEmitResult, emit_project};
pub use writer::write_seam;
