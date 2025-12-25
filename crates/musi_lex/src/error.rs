use musi_basic::error::{IntoMusiError, Level};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[non_exhaustive]
pub enum LexErrorKind {
    #[error("unknown character '{0}'")]
    UnknownChar(char),
    #[error("unknown escape sequence '\\{0}'")]
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

impl IntoMusiError for LexErrorKind {
    fn hint(&self) -> Option<&'static str> {
        match self {
            Self::UnclosedString | Self::UnclosedTemplate => Some("missing '\"'"),
            Self::UnclosedEscapedIdent => Some("missing '`'"),
            Self::UnclosedRune => Some("missing '\\''"),
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

    fn level(&self) -> Level {
        Level::Error
    }
}
