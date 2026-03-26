use core::error;
use core::fmt;

use music_shared::Span;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum LexErrorKind {
    #[error("unexpected character '{0}'")]
    UnexpectedChar(char),

    #[error("unterminated string literal")]
    UnterminatedString,

    #[error("unterminated f-string literal")]
    UnterminatedFString,

    #[error("unterminated f-string interpolation")]
    UnterminatedFStringExpr,

    #[error("unterminated rune literal")]
    UnterminatedRune,

    #[error("unterminated escaped identifier")]
    UnterminatedEscapedIdent,

    #[error("unterminated block comment")]
    UnterminatedBlockComment,

    #[error("empty rune literal")]
    EmptyRune,

    #[error("rune literal contains more than one character")]
    MultiCharRune,

    #[error("invalid escape sequence '\\{0}'")]
    InvalidEscape(char),

    #[error("invalid hex escape: expected {expected} hex digits")]
    InvalidHexEscape { expected: u8 },

    #[error("invalid unicode escape")]
    InvalidUnicodeEscape,

    #[error("invalid number literal: expected digits after base prefix")]
    InvalidNumberPrefix,

    #[error("number literal overflow")]
    NumberOverflow,
}

impl LexError {
    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl error::Error for LexError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(&self.kind)
    }
}

pub type LexResult<T> = Result<T, LexError>;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
