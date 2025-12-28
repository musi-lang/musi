use musi_basic::error::Level;
use musi_macros::MusiError;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Error, MusiError)]
#[non_exhaustive]
pub enum LexErrorKind {
    #[ms(1001)]
    #[error("unknown character '{0}'")]
    UnknownChar(char),
    #[ms(1002)]
    #[error("unknown escape sequence '\\{0}'")]
    UnknownEscape(String),

    #[ms(1003)]
    #[error("unclosed string literal")]
    UnclosedStringLit,
    #[ms(1004)]
    #[error("unclosed template literal")]
    UnclosedTemplateLit,
    #[ms(1005)]
    #[error("unclosed escaped identifier")]
    UnclosedEscapedIdent,
    #[ms(1006)]
    #[error("unclosed rune literal")]
    UnclosedRuneLit,
    #[ms(1007)]
    #[error("unclosed block comment")]
    UnclosedBlockComment,

    #[ms(1008)]
    #[error("invalid identifier")]
    InvalidIdent,
    #[ms(1009)]
    #[error("invalid rune literal")]
    InvalidRuneLit,

    #[ms(1010)]
    #[error("malformed numeric literal")]
    MalformedNumericLit,
    #[ms(1011)]
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
