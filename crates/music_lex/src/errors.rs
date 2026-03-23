use music_found::Span;
use thiserror::Error;

#[derive(Debug, Clone, Error, PartialEq, Eq)]
pub enum LexError {
    #[error("unexpected character '{ch}'")]
    UnexpectedChar { ch: char, span: Span },

    #[error("unterminated string literal")]
    UnterminatedString { span: Span },

    #[error("unterminated f-string literal")]
    UnterminatedFString { span: Span },

    #[error("unterminated f-string interpolation")]
    UnterminatedFStringExpr { span: Span },

    #[error("unterminated rune literal")]
    UnterminatedRune { span: Span },

    #[error("unterminated escaped identifier")]
    UnterminatedEscapedIdent { span: Span },

    #[error("unterminated block comment")]
    UnterminatedBlockComment { span: Span },

    #[error("empty rune literal")]
    EmptyRune { span: Span },

    #[error("rune literal contains more than one character")]
    MultiCharRune { span: Span },

    #[error("invalid escape sequence '\\{ch}'")]
    InvalidEscape { ch: char, span: Span },

    #[error("invalid hex escape: expected {expected} hex digits")]
    InvalidHexEscape { expected: u8, span: Span },

    #[error("invalid unicode escape")]
    InvalidUnicodeEscape { span: Span },

    #[error("invalid number literal: expected digits after base prefix")]
    InvalidNumberPrefix { span: Span },

    #[error("number literal overflow")]
    NumberOverflow { span: Span },
}

impl LexError {
    #[must_use]
    pub const fn span(&self) -> Span {
        match *self {
            Self::UnexpectedChar { span, .. }
            | Self::UnterminatedString { span }
            | Self::UnterminatedFString { span }
            | Self::UnterminatedFStringExpr { span }
            | Self::UnterminatedRune { span }
            | Self::UnterminatedEscapedIdent { span }
            | Self::UnterminatedBlockComment { span }
            | Self::EmptyRune { span }
            | Self::MultiCharRune { span }
            | Self::InvalidEscape { span, .. }
            | Self::InvalidHexEscape { span, .. }
            | Self::InvalidUnicodeEscape { span }
            | Self::InvalidNumberPrefix { span }
            | Self::NumberOverflow { span } => span,
        }
    }
}

pub type LexResult<T> = Result<T, LexError>;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
