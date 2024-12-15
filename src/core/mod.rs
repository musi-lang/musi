use diagnostics::Diagnostic;

pub mod diagnostics;
pub mod source;
pub mod span;
pub mod value;

pub type MusiResult<T> = Result<T, Diagnostic>;
