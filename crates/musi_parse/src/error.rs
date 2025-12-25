use musi_basic::error::{IntoMusiError, Level};
use std::borrow::Cow;
use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[non_exhaustive]
pub enum ParseErrorKind {
    #[error("expected {0}")]
    Expected(Cow<'static, str>),

    #[error("expected {0} after {1}")]
    ExpectedAfter(Cow<'static, str>, Cow<'static, str>),

    #[error("unexpected {0}")]
    Unexpected(String),

    #[error("unexpected end of file")]
    UnexpectedEof,

    #[error("unclosed {0}")]
    Unclosed(Cow<'static, str>),

    #[error("invalid {0}")]
    Invalid(Cow<'static, str>),
}

impl IntoMusiError for ParseErrorKind {
    fn hint(&self) -> Option<&'static str> {
        match self {
            Self::Unclosed(s) if s == "block" => Some("add '}'"),
            Self::Unclosed(s) if s == "parenthese(s)" => Some("add ')'"),
            Self::Unclosed(s) if s == "bracket(s)" => Some("add ']'"),
            Self::Unclosed(s) if s == "attribute" => Some("add '>]'"),
            Self::ExpectedAfter(s, _) if s == "';'" => Some("add ';'"),
            _ => None,
        }
    }

    fn level(&self) -> Level {
        Level::Error
    }
}
