use music_base::Span;
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum LexErrorKind {
    #[error("invalid character '{ch}'")]
    InvalidChar { ch: char },

    #[error("unterminated string literal")]
    UnterminatedStringLiteral,

    #[error("unterminated rune literal")]
    UnterminatedRuneLiteral,

    #[error("empty rune literal")]
    EmptyRuneLiteral,

    #[error("rune literal contains more than one character")]
    RuneLiteralTooLong,

    #[error("unterminated block comment")]
    UnterminatedBlockComment,

    #[error("unterminated escaped identifier")]
    UnterminatedEscapedIdent,

    #[error("missing digits after base prefix {base}")]
    MissingDigitsAfterBasePrefix { base: u32 },

    #[error("invalid digit '{ch}' in base {base} literal")]
    InvalidDigitForBase { base: u32, ch: char },

    #[error("unexpected '_' in number literal")]
    UnexpectedUnderscoreInNumberLiteral,

    #[error("missing digit after '_' in number literal")]
    MissingDigitAfterUnderscoreInNumberLiteral,

    #[error("missing digits in exponent")]
    MissingExponentDigits,

    #[error("missing escape code after '\\\\'")]
    MissingEscapeCode,

    #[error("unexpected escape '\\\\{ch}'")]
    UnexpectedEscape { ch: char },

    #[error("missing hex digits in '\\\\x' escape")]
    MissingHexDigitsInByteEscape,

    #[error("invalid hex digit '{ch}' in '\\\\x' escape")]
    InvalidHexDigitInByteEscape { ch: char },

    #[error("missing hex digits in '\\\\u' escape")]
    MissingHexDigitsInUnicodeEscape,

    #[error("invalid hex digit '{ch}' in '\\\\u' escape")]
    InvalidHexDigitInUnicodeEscape { ch: char },

    #[error("expected 4 or 6 hex digits in '\\\\u' escape")]
    ExpectedFourOrSixHexDigitsInUnicodeEscape,

    #[error("invalid unicode scalar U+{value:06X}")]
    InvalidUnicodeScalar { value: u32 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

pub type LexErrorList = Vec<LexError>;
