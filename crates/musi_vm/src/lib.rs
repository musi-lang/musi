pub(crate) mod effect;
pub mod errors;
pub(crate) mod frame;
pub mod heap;
pub mod loader;
pub mod module;
pub(crate) mod types;
pub mod value;
pub mod vm;

pub use errors::{LoadError, VmError};
pub use heap::{Heap, HeapObject};
pub use loader::load;
pub use module::{ConstantEntry, Module};
pub use value::Value;
pub use vm::Vm;
