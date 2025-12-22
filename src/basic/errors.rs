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
    Lex(#[from] LexErrorKind),
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

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LexErrorKind {
    #[error("unknown character '{0}'")]
    UnknownChar(char),
    #[error("unknown escape sequence '{0}'")]
    UnknownEscape(String),

    #[error("unclosed string literal")]
    UnclosedString,
    #[error("unclosed template literal")]
    UnclosedTemplate,
    #[error("unclosed escaped identifier")]
    UnclosedEscapedIdent,
    #[error("unclosed rune literal")]
    UnclosedRune,
    #[error("unclosed block comment")]
    UnclosedComment,

    #[error("invalid identifier")]
    InvalidIdent,
    #[error("invalid rune literal")]
    InvalidRune,

    #[error("malformed numeric literal")]
    MalformedNumber,
    #[error("malformed '_' in {0} literal")]
    MalformedUnderscore(String),
}

impl LexErrorKind {
    pub fn hint(&self) -> Option<&'static str> {
        match self {
            Self::UnclosedString | Self::UnclosedTemplate => Some("missing '\"'"),
            Self::UnclosedEscapedIdent => Some("missing '`'"),
            Self::UnclosedRune => Some("missing '\''"),
            Self::UnclosedComment => Some("missing '*/'"),
            Self::UnknownChar(_) => Some("remove this character"),
            Self::UnknownEscape(_) => {
                Some("use '\\n', '\\r', '\\t', '\\\\', '\\'', '\\\"', '\\xHH', or '\\u{...}'")
            }
            Self::MalformedUnderscore(_) => Some("underscores must separate digits"),
            Self::MalformedNumber => Some("add missing digits"),
            Self::InvalidIdent | Self::InvalidRune => None,
        }
    }

    pub const fn level(&self) -> Level {
        Level::Error
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Level {
    Error,
    Warning,
    Note,
}
