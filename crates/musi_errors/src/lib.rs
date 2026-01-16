//! Diagnostics and error handling.

mod diagnostic;
mod severity;

pub use diagnostic::{Diagnostic, DiagnosticBag};
pub use severity::Severity;
