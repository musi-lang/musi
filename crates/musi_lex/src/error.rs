use musi_basic::error::Level;
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

impl LexErrorKind {
    #[must_use]
    pub const fn hint(&self) -> Option<&'static str> {
        match self {
            Self::MalformedUnderscore(_) => Some("underscores must separate digits"),
            _ => None,
        }
    }

    #[must_use]
    pub const fn level(&self) -> Level {
        Level::Error
    }
}
