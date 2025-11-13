pub mod bytecode;
pub mod engine;
pub mod errors;
pub mod executor;
pub mod frames;
pub mod memmgr;
pub mod opcode;
pub mod stack;
pub mod stdlib;
pub mod value;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub const MAX_STACK_SIZE: usize = 65536;
pub const MAX_CALL_DEPTH: usize = 1024;
