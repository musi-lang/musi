pub mod errors;
pub mod frame;
pub mod heap;
pub mod loader;
pub mod module;
pub mod value;
pub mod vm;

pub use errors::{LoadError, VmError};
pub use heap::{Closure, Continuation, Heap};
pub use loader::load;
pub use module::Module;
pub use value::Value;
pub use vm::Vm;
