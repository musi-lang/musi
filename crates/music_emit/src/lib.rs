pub mod emitter;
pub mod error;
pub mod pool;
pub mod project;
pub mod writer;

pub use emitter::{emit, emit_with_context, GlobalEntry, MethodEntry, SeamModule};
pub use error::EmitError;
pub use pool::{ConstantEntry, ConstantPool};
pub use project::{emit_project, ProjectEmitResult};
pub use writer::write_seam;
