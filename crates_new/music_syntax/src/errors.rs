use music_base::Span;
use thiserror::Error;

use crate::TokenKind;

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

    #[error("unterminated template literal")]
    UnterminatedTemplateLiteral,

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

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum ParseErrorKind {
    #[error("expected {expected}, found {found}")]
    ExpectedToken {
        expected: TokenKind,
        found: TokenKind,
    },

    #[error("expected expression, found {found}")]
    ExpectedExpression { found: TokenKind },

    #[error("expected pattern, found {found}")]
    ExpectedPattern { found: TokenKind },

    #[error("expected member, found {found}")]
    ExpectedMember { found: TokenKind },

    #[error("expected identifier, found {found}")]
    ExpectedIdentifier { found: TokenKind },

    #[error("expected splice target, found {found}")]
    ExpectedSpliceTarget { found: TokenKind },

    #[error("expected operator member name, found {found}")]
    ExpectedOperatorMemberName { found: TokenKind },

    #[error("expected array dimension, found {found}")]
    ExpectedArrayDimension { found: TokenKind },

    #[error("expected field name or tuple index, found {found}")]
    ExpectedFieldTarget { found: TokenKind },

    #[error("expected constraint operator '<:' or ':', found {found}")]
    ExpectedConstraintOperator { found: TokenKind },

    #[error("expected attribute value, found {found}")]
    ExpectedAttrValue { found: TokenKind },

    #[error("splice outside quote")]
    SpliceOutsideQuote,

    #[error("non-associative comparison chain")]
    NonAssociativeChain,
}
