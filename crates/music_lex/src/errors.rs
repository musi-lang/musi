use core::error;
use core::fmt;

use music_shared::diag::{Diag, DiagCode};
use music_shared::SourceId;
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

    #[error("invalid hex escape; expected {expected} hex digit(s)")]
    InvalidHexEscape { expected: u8 },

    #[error("invalid unicode escape")]
    InvalidUnicodeEscape,

    #[error("invalid number literal; expected digit(s) after base prefix")]
    InvalidNumberPrefix,

    #[error("number literal overflow")]
    NumberOverflow,
}

impl LexError {
    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }

    #[must_use]
    pub fn diagnostic(&self, source_id: SourceId) -> Diag {
        let (code, message) = match self.kind {
            LexErrorKind::UnexpectedChar(ch) => (
                DiagCode::new(1001),
                format!("unexpected character '{ch}'"),
            ),
            LexErrorKind::UnterminatedString => {
                (DiagCode::new(1002), String::from("unterminated string literal"))
            }
            LexErrorKind::UnterminatedFString => {
                (DiagCode::new(1003), String::from("unterminated f-string literal"))
            }
            LexErrorKind::UnterminatedFStringExpr => (
                DiagCode::new(1004),
                String::from("unterminated f-string interpolation"),
            ),
            LexErrorKind::UnterminatedRune => {
                (DiagCode::new(1005), String::from("unterminated rune literal"))
            }
            LexErrorKind::UnterminatedEscapedIdent => (
                DiagCode::new(1006),
                String::from("unterminated escaped identifier"),
            ),
            LexErrorKind::UnterminatedBlockComment => (
                DiagCode::new(1007),
                String::from("unterminated block comment"),
            ),
            LexErrorKind::EmptyRune => (DiagCode::new(1008), String::from("empty rune literal")),
            LexErrorKind::MultiCharRune => (
                DiagCode::new(1009),
                String::from("rune literal contains more than one character"),
            ),
            LexErrorKind::InvalidEscape(ch) => (
                DiagCode::new(1010),
                format!("invalid escape sequence '\\{ch}'"),
            ),
            LexErrorKind::InvalidHexEscape { expected } => (
                DiagCode::new(1011),
                format!("invalid hex escape; expected {expected} hex digit(s)"),
            ),
            LexErrorKind::InvalidUnicodeEscape => {
                (DiagCode::new(1012), String::from("invalid unicode escape"))
            }
            LexErrorKind::InvalidNumberPrefix => (
                DiagCode::new(1013),
                String::from("invalid number literal; expected digit(s) after base prefix"),
            ),
            LexErrorKind::NumberOverflow => {
                (DiagCode::new(1014), String::from("number literal overflow"))
            }
        };

        Diag::error(message)
            .with_code(code)
            .with_label(self.span, source_id, "")
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
