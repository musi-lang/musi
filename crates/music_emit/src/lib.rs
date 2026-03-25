pub mod emitter;
pub mod error;
pub mod pool;
pub mod writer;

pub use emitter::{emit, GlobalEntry, MethodEntry, SeamModule};
pub use error::EmitError;
pub use pool::{ConstantEntry, ConstantPool};
pub use writer::write_seam;
