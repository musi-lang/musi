use crate::code::ErrorCode;
use crate::diagnostic::Level;
use musi_basic::span::Span;
use std::fmt;
use thiserror::Error;

/// Core error type for Musi compiler.
#[derive(Debug, Clone, Error)]
#[error("{message}")]
#[non_exhaustive]
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
    /// Creates new error with message and span.
    #[must_use]
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            code: None,
            message: message.into(),
            hint: None,
            level: Level::Error,
            span,
        }
    }

    /// Sets error code.
    #[must_use]
    pub fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = Some(code);
        self
    }

    /// Sets optional hint.
    #[must_use]
    pub fn with_hint(mut self, hint: &'static str) -> Self {
        self.hint = Some(hint);
        self
    }

    /// Sets severity level.
    #[must_use]
    pub fn with_level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }
}

/// Trait for types convertible to MusiError.
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
