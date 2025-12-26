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
    UnclosedStringLit,
    #[error("unclosed template literal")]
    UnclosedTemplateLit,
    #[error("unclosed escaped identifier")]
    UnclosedEscapedIdent,
    #[error("unclosed rune literal")]
    UnclosedRuneLit,
    #[error("unclosed block comment")]
    UnclosedBlockComment,

    #[error("invalid identifier")]
    InvalidIdent,
    #[error("invalid rune literal")]
    InvalidRuneLit,

    #[error("malformed numeric literal")]
    MalformedNumericLit,
    #[error("malformed '_' in {0} literal")]
    MalformedUnderscore(String),
}

impl IntoMusiError for LexErrorKind {
    fn hint(&self) -> Option<&'static str> {
        match self {
            Self::UnclosedStringLit | Self::UnclosedTemplateLit => Some("add '\"'"),
            Self::UnclosedEscapedIdent => Some("add '`'"),
            Self::UnclosedRuneLit => Some("add '\\''"),
            Self::UnclosedBlockComment => Some("add '*/'"),
            Self::MalformedUnderscore(_) => Some("underscores must separate digits"),
            _ => None,
        }
    }

    fn level(&self) -> Level {
        Level::Error
    }
}
