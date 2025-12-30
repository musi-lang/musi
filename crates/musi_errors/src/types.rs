use crate::code::ErrorCode;
use crate::diagnostic::Level;
use musi_basic::span::Span;
use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[error("{message}")]
#[non_exhaustive]
pub struct MusiError {
    pub code: Option<ErrorCode>,
    pub message: String,
    pub hint: Option<&'static str>,
    pub level: Level,
    pub span: Span,
}

impl MusiError {
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

    #[must_use]
    pub fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = Some(code);
        self
    }

    #[must_use]
    pub fn with_hint(mut self, hint: &'static str) -> Self {
        self.hint = Some(hint);
        self
    }

    #[must_use]
    pub fn with_level(mut self, level: Level) -> Self {
        self.level = level;
        self
    }
}

pub trait IntoMusiError: fmt::Display {
    fn code(&self) -> Option<ErrorCode> {
        None
    }

    fn hint(&self) -> Option<&'static str> {
        None
    }

    fn level(&self) -> Level {
        Level::Error
    }

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
