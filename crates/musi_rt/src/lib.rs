mod api;
mod error;
mod runtime;
mod testing;

pub use api::RuntimeOptions;
pub use error::{RuntimeError, RuntimeErrorKind};
pub use runtime::Runtime;
pub use testing::{RuntimeTestCaseResult, RuntimeTestReport};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
