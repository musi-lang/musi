use crate::span::Span;
use std::error;
use std::fmt;

pub type MusiResult<T> = Result<T, MusiError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[non_exhaustive]
pub enum ErrorCode {
    // Lex errors (1xxx)
    MS1001,
    MS1002,
    MS1003,

    // Parse/syntax errors (2xxx)
    MS2001,
    MS2002,
    MS2003,

    // Sema/type errors (3xxx)
    MS3001,
    MS3002,
    MS3003,
    MS3004,
    MS3005,
    MS3006,

    // Codegen errors (4xxx)
    MS4001,
    MS4002,

    // Runtime errors (5xxx)
    MS5001,
    MS5002,
}

impl ErrorCode {
    #[must_use]
    pub const fn as_u16(self) -> u16 {
        match self {
            Self::MS1001 => 1001,
            Self::MS1002 => 1002,
            Self::MS1003 => 1003,
            Self::MS2001 => 2001,
            Self::MS2002 => 2002,
            Self::MS2003 => 2003,
            Self::MS3001 => 3001,
            Self::MS3002 => 3002,
            Self::MS3003 => 3003,
            Self::MS3004 => 3004,
            Self::MS3005 => 3005,
            Self::MS3006 => 3006,
            Self::MS4001 => 4001,
            Self::MS4002 => 4002,
            Self::MS5001 => 5001,
            Self::MS5002 => 5002,
        }
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "MS{}", self.as_u16())
    }
}

#[derive(Debug, Clone)]
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
    pub const fn with_code(mut self, code: ErrorCode) -> Self {
        self.code = Some(code);
        self
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
