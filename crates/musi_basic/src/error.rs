use crate::span::Span;
use std::error;
use std::fmt;

pub type MusiResult<T> = Result<T, MusiError>;

#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct MusiError {
    pub message: String,
    pub hint: Option<&'static str>,
    pub level: Level,
    pub span: Span,
}

impl MusiError {
    #[must_use]
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            hint: None,
            level: Level::Error,
            span,
        }
    }

    #[must_use]
    pub const fn with_hint(mut self, hint: &'static str) -> Self {
        self.hint = Some(hint);
        self
    }

    #[must_use]
    pub const fn with_level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }
}

impl fmt::Display for MusiError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl error::Error for MusiError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Level {
    Error,
    Warning,
    Note,
}

/// Trait for converting phase-specific errors into `MusiError`.
pub trait IntoMusiError: fmt::Display {
    /// Optional hint for error recovery.
    fn hint(&self) -> Option<&'static str> {
        None
    }

    /// Severity level of this error.
    fn level(&self) -> Level {
        Level::Error
    }

    /// Convert into a `MusiError` at given span.
    fn into_musi_error(self, span: Span) -> MusiError
    where
        Self: Sized,
    {
        let mut err = MusiError::new(self.to_string(), span).with_level(self.level());
        if let Some(hint) = self.hint() {
            err = err.with_hint(hint);
        }
        err
    }
}
