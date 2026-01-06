pub mod code;
pub mod diagnostic;
pub mod helpers;
pub mod parse_errors;
pub mod types;

pub use code::ErrorCode;
pub use diagnostic::{Diagnostic, DiagnosticBag, Level};
pub use parse_errors::ParseErrorKind;
pub use types::{IntoMusiError, MusiError};

pub type MusiResult<T> = Result<T, MusiError>;
