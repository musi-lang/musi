pub mod instr;

pub const VERSION: &str = env!("CARGO_PKG_VERSION");

pub const MAX_STACK_SIZE: usize = 65536;
pub const MAX_CALL_DEPTH: usize = 1024;
