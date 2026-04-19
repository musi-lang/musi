mod api;
mod diag;
mod error;
mod output;
mod runtime;
mod runtime_handlers;

pub use api::{RuntimeOptions, RuntimeOutputMode};
pub use diag::RuntimeDiagKind;
pub use error::{RuntimeError, RuntimeErrorKind, RuntimeSessionPhase};
pub use musi_vm::HeapCollectionStats;
pub use output::RuntimeOutput;
pub use runtime::Runtime;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
