use std::fmt;

use crate::diag::Level;
use crate::span::Span;
use thiserror::Error;

/// Unique identifier for compiler errors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ErrorCode {
    /// Unknown character in source.
    E1001,
    /// String literal was not closed.
    E1002,
    /// Template literal was not closed.
    E1003,
    /// Escaped identifier was not closed.
    E1004,
    /// Rune literal was not closed.
    E1005,
    /// Block comment was not closed.
    E1006,
    /// Identifier format is invalid.
    E1007,
    /// Rune literal is invalid.
    E1008,
    /// Numeric literal is malformed.
    E1009,
    /// Underscore placement is invalid.
    E1010,
    /// Escape sequence is unknown.
    E1011,

    /// Generic parser error.
    E2001,
}

impl ErrorCode {
    /// Returns numeric representation of error code.
    #[must_use]
    pub const fn as_u16(self) -> u16 {
        match self {
            Self::E1001 => 1001,
            Self::E1002 => 1002,
            Self::E1003 => 1003,
            Self::E1004 => 1004,
            Self::E1005 => 1005,
            Self::E1006 => 1006,
            Self::E1007 => 1007,
            Self::E1008 => 1008,
            Self::E1009 => 1009,
            Self::E1010 => 1010,
            Self::E1011 => 1011,

            Self::E2001 => 2001,
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MS{}", self.as_u16())
    }
}

#[derive(Debug, Clone, Error)]
#[error("{message}")]
#[non_exhaustive]
/// Core error type for Musi compiler.
pub struct MusiError {
    /// Optional error code.
    pub code: Option<ErrorCode>,
    /// Main error message.
    pub message: String,
    /// Optional helpful hint.
    pub hint: Option<&'static str>,
    /// Severity level.
    pub level: Level,
    /// Location of error.
    pub span: Span,
}

impl MusiError {
    #[must_use]
    /// Creates new error with message and span.
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            code: None,
            message: message.into(),
            hint: None,
            level: Level::Error,
            span,
        }
    }

    #[must_use]
    /// Sets error code.
    pub const fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = Some(code);
        self
    }

    #[must_use]
    /// Sets optional hint.
    pub const fn with_hint(mut self, hint: &'static str) -> Self {
        self.hint = Some(hint);
        self
    }

    #[must_use]
    /// Sets severity level.
    pub const fn with_level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }
}

/// Trait for types convertible to `MusiError`.
pub trait IntoMusiError: fmt::Display {
    /// Returns optional error code.
    fn code(&self) -> Option<ErrorCode> {
        None
    }

    /// Returns optional hint.
    fn hint(&self) -> Option<&'static str> {
        None
    }

    /// Returns severity level.
    fn level(&self) -> Level {
        Level::Error
    }

    /// Converts into `MusiError`.
    fn into_musi_error(self, span: Span) -> MusiError
    where
        Self: Sized,
    {
        let mut err = MusiError::new(self.to_string(), span).with_level(self.level());
        if let Some(code) = self.code() {
            err = err.with_code(code);
        }
        if let Some(hint) = self.hint() {
            err = err.with_hint(hint);
        }
        err
    }
}

/// Result type alias for Musi compiler operations.
pub type MusiResult<T> = Result<T, MusiError>;
