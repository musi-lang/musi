use crate::basic::span::Span;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Error)]
#[error("{kind}")]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl Error {
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
