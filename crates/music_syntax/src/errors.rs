use music_base::diag::{Diag, DiagCode};
use music_base::{SourceId, Span};
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
pub type ParseErrorList = Vec<ParseError>;

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

impl LexError {
    #[must_use]
    pub fn to_diag(self, source_id: SourceId, source_text: &str) -> Diag {
        self.kind.to_diag(self.span, source_id, source_text)
    }
}

impl ParseError {
    #[must_use]
    pub fn to_diag(self, source_id: SourceId, source_text: &str) -> Diag {
        self.kind.to_diag(self.span, source_id, source_text)
    }
}

impl LexErrorKind {
    #[must_use]
    pub const fn code(self) -> DiagCode {
        DiagCode::new(match self {
            Self::InvalidChar { .. } => 1200,
            Self::UnterminatedStringLiteral => 1201,
            Self::UnterminatedRuneLiteral => 1202,
            Self::EmptyRuneLiteral => 1203,
            Self::RuneLiteralTooLong => 1204,
            Self::UnterminatedBlockComment => 1205,
            Self::UnterminatedTemplateLiteral => 1206,
            Self::MissingDigitsAfterBasePrefix { .. } => 1207,
            Self::InvalidDigitForBase { .. } => 1208,
            Self::UnexpectedUnderscoreInNumberLiteral => 1209,
            Self::MissingDigitAfterUnderscoreInNumberLiteral => 1210,
            Self::MissingExponentDigits => 1211,
            Self::MissingEscapeCode => 1212,
            Self::UnexpectedEscape { .. } => 1213,
            Self::MissingHexDigitsInByteEscape => 1214,
            Self::InvalidHexDigitInByteEscape { .. } => 1215,
            Self::MissingHexDigitsInUnicodeEscape => 1216,
            Self::InvalidHexDigitInUnicodeEscape { .. } => 1217,
            Self::ExpectedFourOrSixHexDigitsInUnicodeEscape => 1218,
            Self::InvalidUnicodeScalar { .. } => 1219,
        })
    }

    #[must_use]
    pub fn headline(self) -> String {
        match self {
            Self::InvalidChar { .. } => "invalid character".into(),
            Self::UnterminatedStringLiteral => "unterminated string literal".into(),
            Self::UnterminatedRuneLiteral => "unterminated rune literal".into(),
            Self::EmptyRuneLiteral => "empty rune literal".into(),
            Self::RuneLiteralTooLong => "rune literal contains more than one character".into(),
            Self::UnterminatedBlockComment => "unterminated block comment".into(),
            Self::UnterminatedTemplateLiteral => "unterminated template literal".into(),
            Self::MissingDigitsAfterBasePrefix { base } => {
                format!("base {base} literal requires digits")
            }
            Self::InvalidDigitForBase { base, .. } => {
                format!("invalid digit in base {base} literal")
            }
            Self::UnexpectedUnderscoreInNumberLiteral => "underscore invalid".into(),
            Self::MissingDigitAfterUnderscoreInNumberLiteral => {
                "digit after `_` is required".into()
            }
            Self::MissingExponentDigits => "exponent requires digits".into(),
            Self::MissingEscapeCode => "escape requires code".into(),
            Self::UnexpectedEscape { .. } => "invalid escape".into(),
            Self::MissingHexDigitsInByteEscape => "`\\x` escape requires hex digits".into(),
            Self::InvalidHexDigitInByteEscape { .. } => {
                "`\\x` escape contains invalid hex digit".into()
            }
            Self::MissingHexDigitsInUnicodeEscape => "`\\u` escape requires hex digits".into(),
            Self::InvalidHexDigitInUnicodeEscape { .. } => {
                "`\\u` escape contains invalid hex digit".into()
            }
            Self::ExpectedFourOrSixHexDigitsInUnicodeEscape => {
                "`\\u` escape requires 4 or 6 hex digits".into()
            }
            Self::InvalidUnicodeScalar { value } => {
                format!("invalid unicode scalar `U+{value:06X}`")
            }
        }
    }

    #[must_use]
    pub fn to_diag(self, span: Span, source_id: SourceId, source_text: &str) -> Diag {
        let label = match self {
            Self::InvalidChar { ch } | Self::InvalidDigitForBase { ch, .. } => {
                format!("found `{}`", escape_char(ch))
            }
            Self::UnterminatedStringLiteral => "string starts here".into(),
            Self::UnterminatedRuneLiteral => "rune starts here".into(),
            Self::EmptyRuneLiteral
            | Self::RuneLiteralTooLong
            | Self::MissingDigitsAfterBasePrefix { .. }
            | Self::UnexpectedUnderscoreInNumberLiteral
            | Self::MissingDigitAfterUnderscoreInNumberLiteral
            | Self::MissingExponentDigits
            | Self::MissingEscapeCode
            | Self::UnexpectedEscape { .. }
            | Self::MissingHexDigitsInByteEscape
            | Self::InvalidHexDigitInByteEscape { .. }
            | Self::MissingHexDigitsInUnicodeEscape
            | Self::InvalidHexDigitInUnicodeEscape { .. }
            | Self::ExpectedFourOrSixHexDigitsInUnicodeEscape
            | Self::InvalidUnicodeScalar { .. } => {
                format!("found {}", describe_span(source_text, span, "invalid text"))
            }
            Self::UnterminatedBlockComment => "comment starts here".into(),
            Self::UnterminatedTemplateLiteral => "template starts here".into(),
        };
        Diag::error(self.headline())
            .with_code(self.code())
            .with_label(span, source_id, label)
    }
}

impl ParseErrorKind {
    #[must_use]
    pub const fn code(self) -> DiagCode {
        DiagCode::new(match self {
            Self::ExpectedToken { .. } => 1300,
            Self::ExpectedExpression { .. } => 1301,
            Self::ExpectedPattern { .. } => 1302,
            Self::ExpectedMember { .. } => 1303,
            Self::ExpectedIdentifier { .. } => 1304,
            Self::ExpectedSpliceTarget { .. } => 1305,
            Self::ExpectedOperatorMemberName { .. } => 1306,
            Self::ExpectedFieldTarget { .. } => 1307,
            Self::ExpectedConstraintOperator { .. } => 1308,
            Self::ExpectedAttrValue { .. } => 1309,
            Self::SpliceOutsideQuote => 1310,
            Self::NonAssociativeChain => 1311,
        })
    }

    #[must_use]
    pub fn headline(self) -> String {
        match self {
            Self::ExpectedToken { expected, .. } => format!("expected {expected}"),
            Self::ExpectedExpression { .. } => "expected expression".into(),
            Self::ExpectedPattern { .. } => "expected pattern".into(),
            Self::ExpectedMember { .. } => "expected member".into(),
            Self::ExpectedIdentifier { .. } => "expected identifier".into(),
            Self::ExpectedSpliceTarget { .. } => "expected splice target".into(),
            Self::ExpectedOperatorMemberName { .. } => "expected operator member name".into(),
            Self::ExpectedFieldTarget { .. } => "expected field name or tuple index".into(),
            Self::ExpectedConstraintOperator { .. } => {
                "expected constraint operator `<:` or `:`".into()
            }
            Self::ExpectedAttrValue { .. } => "expected attr value".into(),
            Self::SpliceOutsideQuote => "splice is only valid inside quote".into(),
            Self::NonAssociativeChain => "comparison chain requires grouping".into(),
        }
    }

    #[must_use]
    pub fn to_diag(self, span: Span, source_id: SourceId, source_text: &str) -> Diag {
        let mut diag = Diag::error(self.headline()).with_code(self.code());
        let label = match self {
            Self::ExpectedToken { found, .. }
            | Self::ExpectedExpression { found }
            | Self::ExpectedPattern { found }
            | Self::ExpectedMember { found }
            | Self::ExpectedIdentifier { found }
            | Self::ExpectedSpliceTarget { found }
            | Self::ExpectedOperatorMemberName { found }
            | Self::ExpectedFieldTarget { found }
            | Self::ExpectedConstraintOperator { found }
            | Self::ExpectedAttrValue { found } => {
                format!("found {}", describe_found(source_text, span, found))
            }
            Self::SpliceOutsideQuote => {
                format!("found {}", describe_span(source_text, span, "`#`"))
            }
            Self::NonAssociativeChain => "chain continues here".into(),
        };
        diag = diag.with_label(span, source_id, label);
        if matches!(self, Self::NonAssociativeChain) {
            diag = diag.with_hint("parenthesize comparison");
        }
        diag
    }
}

fn describe_found(source_text: &str, span: Span, found: TokenKind) -> String {
    if matches!(found, TokenKind::Eof) {
        return "end of input".into();
    }
    describe_span(source_text, span, found.to_string().as_str())
}

fn describe_span(source_text: &str, span: Span, fallback: &str) -> String {
    snippet_text(source_text, span).unwrap_or_else(|| fallback.into())
}

fn snippet_text(source_text: &str, span: Span) -> Option<String> {
    let start = usize::try_from(span.start).ok()?;
    let end = usize::try_from(span.end).ok()?;
    let raw = source_text.get(start..end)?;
    if raw.is_empty() {
        return None;
    }
    let mut escaped = String::new();
    for ch in raw.chars().take(24) {
        match ch {
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    if raw.chars().count() > 24 {
        escaped.push_str("...");
    }
    Some(format!("`{escaped}`"))
}

fn escape_char(ch: char) -> String {
    match ch {
        '\n' => "\\n".into(),
        '\r' => "\\r".into(),
        '\t' => "\\t".into(),
        _ => ch.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use music_base::SourceMap;

    use super::*;

    fn source_id(text: &str) -> SourceId {
        let mut map = SourceMap::default();
        map.add("test.ms", text).expect("source add succeeds")
    }

    #[test]
    fn lex_diag_points_at_invalid_character() {
        let text = "€";
        let diag = LexError {
            kind: LexErrorKind::InvalidChar { ch: '€' },
            span: Span::new(0, 3),
        }
        .to_diag(source_id(text), text);

        assert_eq!(diag.message(), "invalid character");
        assert_eq!(diag.labels()[0].message(), "found `€`");
    }

    #[test]
    fn parse_diag_uses_end_of_input() {
        let text = "let x := 1";
        let diag = ParseError {
            kind: ParseErrorKind::ExpectedToken {
                expected: TokenKind::Semicolon,
                found: TokenKind::Eof,
            },
            span: Span::new(10, 10),
        }
        .to_diag(source_id(text), text);

        assert_eq!(diag.message(), "expected `;`");
        assert_eq!(diag.labels()[0].message(), "found end of input");
    }

    #[test]
    fn parse_diag_uses_fixit_hint_only_for_grouping_error() {
        let text = "a < b < c";
        let diag = ParseError {
            kind: ParseErrorKind::NonAssociativeChain,
            span: Span::new(6, 7),
        }
        .to_diag(source_id(text), text);

        assert_eq!(diag.message(), "comparison chain requires grouping");
        assert_eq!(diag.hint(), Some("parenthesize comparison"));
    }
}
