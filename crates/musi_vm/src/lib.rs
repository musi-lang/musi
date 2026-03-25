pub mod error;
pub mod frame;
pub mod heap;
pub mod loader;
pub mod module;
pub mod value;
pub mod vm;

pub use error::{LoadError, VmError};
pub use heap::{Closure, Heap};
pub use loader::load;
pub use module::Module;
pub use value::Value;
pub use vm::Vm;
