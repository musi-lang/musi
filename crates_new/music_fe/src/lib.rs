//! Frontend orchestration over the clean-room compiler crates.

mod compile;
mod errors;
mod graph;

pub use compile::{CompiledProgram, compile_entry, compile_entry_binary};
pub use errors::{FrontendError, FrontendErrorKind, FrontendResult};
