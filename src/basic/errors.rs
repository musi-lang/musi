use crate::basic::span::Span;
use thiserror::Error;

pub type MusiResult<T> = Result<T, MusiError>;

#[derive(Debug, Clone, Error)]
#[error("{kind}")]
pub struct MusiError {
    pub kind: ErrorKind,
    pub span: Span,
}

impl MusiError {
    pub const fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn hint(&self) -> Option<&'static str> {
        self.kind.hint()
    }

    pub const fn level(&self) -> Level {
        self.kind.level()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ErrorKind {
    #[error(transparent)]
    Lex(#[from] crate::lex::errors::LexErrorKind),
}

impl ErrorKind {
    pub fn hint(&self) -> Option<&'static str> {
        match self {
            Self::Lex(err) => err.hint(),
        }
    }

    pub const fn level(&self) -> Level {
        match self {
            Self::Lex(err) => err.level(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Note,
}
