use musi_basic::error::{IntoMusiError, Level};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[non_exhaustive]
pub enum ParseErrorKind {
    #[error("expected {0}")]
    Expected(&'static str),

    #[error("expected {0} after {1}")]
    ExpectedAfter(&'static str, &'static str),

    #[error("unexpected {0}")]
    Unexpected(String),

    #[error("unexpected end of file")]
    UnexpectedEof,

    #[error("unclosed {0}")]
    Unclosed(&'static str),

    #[error("invalid {0}")]
    Invalid(&'static str),
}

impl IntoMusiError for ParseErrorKind {
    fn hint(&self) -> Option<&'static str> {
        match self {
            Self::Unclosed("block") => Some("add '}'"),
            Self::Unclosed("parenthese(s)") => Some("add ')'"),
            Self::Unclosed("bracket(s)") => Some("add ']'"),
            Self::Unclosed("attribute") => Some("add '>]'"),
            Self::ExpectedAfter("';'", _) => Some("add ';'"),
            _ => None,
        }
    }

    fn level(&self) -> Level {
        Level::Error
    }
}
