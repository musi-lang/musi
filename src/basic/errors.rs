use crate::basic::span::Span;
use crate::lex::errors::LexErrorKind;
use thiserror::Error;

pub type MusiResult<T> = Result<T, MusiError>;

#[derive(Debug, Clone, Error)]
#[error("{kind}")]
#[non_exhaustive]
pub struct MusiError {
    pub kind: ErrorKind,
    pub span: Span,
}

impl MusiError {
    #[must_use]
    pub const fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[must_use]
    pub const fn hint(&self) -> Option<&'static str> {
        self.kind.hint()
    }

    #[must_use]
    pub const fn level(&self) -> Level {
        self.kind.level()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[non_exhaustive]
pub enum ErrorKind {
    #[error(transparent)]
    Lex(#[from] LexErrorKind),
}

impl ErrorKind {
    #[must_use]
    pub const fn hint(&self) -> Option<&'static str> {
        match self {
            Self::Lex(err) => err.hint(),
        }
    }

    #[must_use]
    pub const fn level(&self) -> Level {
        match self {
            Self::Lex(err) => err.level(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Level {
    Error,
    Warning,
    Note,
}
