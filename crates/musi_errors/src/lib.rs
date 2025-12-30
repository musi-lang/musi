//! Unified diagnostics for Musi compiler.
//!
//! # Structure
//!
//! * `code` - Numeric error codes
//! * `diagnostic` - Main diagnostic structures
//! * `helpers` - Constructor functions for common errors
//! * `types` - Core error types

pub mod code;
pub mod diagnostic;
pub mod helpers;
pub mod types;

pub use code::ErrorCode;
pub use diagnostic::{Diagnostic, DiagnosticBag, Level};
pub use types::{IntoMusiError, MusiError};
