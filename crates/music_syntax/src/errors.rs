use music_base::diag::{Diag, DiagCode, DiagContext, DiagLevel, DiagnosticKind};
use music_base::{SourceId, Span};
use thiserror::Error;

use crate::TokenKind;

#[path = "diag_catalog_gen.rs"]
mod diag_catalog_gen;

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxDiagKind {
    InvalidChar,
    UnterminatedStringLiteral,
    UnterminatedRuneLiteral,
    EmptyRuneLiteral,
    RuneLiteralTooLong,
    UnterminatedBlockComment,
    UnterminatedTemplateLiteral,
    MissingDigitsAfterBasePrefix,
    InvalidDigitForBase,
    UnexpectedUnderscoreInNumberLiteral,
    MissingDigitAfterUnderscoreInNumberLiteral,
    MissingExponentDigits,
    MissingEscapeCode,
    UnexpectedEscape,
    MissingHexDigitsInByteEscape,
    InvalidHexDigitInByteEscape,
    MissingHexDigitsInUnicodeEscape,
    InvalidHexDigitInUnicodeEscape,
    ExpectedFourOrSixHexDigitsInUnicodeEscape,
    InvalidUnicodeScalar,
    ExpectedToken,
    ExpectedExpression,
    ExpectedPattern,
    ExpectedMember,
    ExpectedIdentifier,
    ReservedKeywordIdentifier,
    ExpectedSpliceTarget,
    ExpectedOperatorMemberName,
    ExpectedFieldTarget,
    ExpectedConstraintOperator,
    ExpectedAttrValue,
    SpliceOutsideQuote,
    NonAssociativeChain,
}

impl SyntaxDiagKind {
    #[must_use]
    pub fn code(self) -> DiagCode {
        DiagCode::new(diag_catalog_gen::code(self))
    }

    #[must_use]
    pub fn message(self) -> &'static str {
        diag_catalog_gen::message(self)
    }

    #[must_use]
    pub fn label(self) -> &'static str {
        diag_catalog_gen::primary(self)
    }

    #[must_use]
    pub fn hint(self) -> Option<&'static str> {
        diag_catalog_gen::help(self)
    }

    #[must_use]
    pub fn message_with(self, context: &DiagContext) -> String {
        diag_catalog_gen::render_message(self, context)
    }

    #[must_use]
    pub fn label_with(self, context: &DiagContext) -> String {
        diag_catalog_gen::render_primary(self, context)
    }

    #[must_use]
    pub fn from_code(code: DiagCode) -> Option<Self> {
        diag_catalog_gen::from_code(code.raw())
    }

    #[must_use]
    pub fn from_diag(diag: &Diag) -> Option<Self> {
        diag.code().and_then(Self::from_code)
    }
}

impl DiagnosticKind for SyntaxDiagKind {
    fn code(self) -> DiagCode {
        self.code()
    }

    fn phase(self) -> &'static str {
        "syntax"
    }

    fn level(self) -> DiagLevel {
        DiagLevel::Error
    }

    fn message(self) -> &'static str {
        self.message()
    }

    fn primary(self) -> &'static str {
        self.label()
    }

    fn help(self) -> Option<&'static str> {
        self.hint()
    }
}

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

    #[error("reserved keyword {keyword} cannot name identifier")]
    ReservedKeywordIdentifier { keyword: TokenKind },

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
    pub const fn new(kind: LexErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[must_use]
    pub fn to_diag(self, source_id: SourceId, source_text: &str) -> Diag {
        self.kind.to_diag(self.span, source_id, source_text)
    }
}

impl ParseError {
    #[must_use]
    pub const fn new(kind: ParseErrorKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[must_use]
    pub fn to_diag(self, source_id: SourceId, source_text: &str) -> Diag {
        self.kind.to_diag(self.span, source_id, source_text)
    }
}

impl LexErrorKind {
    #[must_use]
    pub const fn diag_kind(self) -> SyntaxDiagKind {
        diag_catalog_gen::lex_error_kind(self)
    }

    #[must_use]
    pub fn code(self) -> DiagCode {
        self.diag_kind().code()
    }

    #[must_use]
    pub fn headline(self) -> String {
        self.context().render(self.diag_kind().message())
    }

    #[must_use]
    pub fn context(self) -> DiagContext {
        match self {
            Self::InvalidChar { ch }
            | Self::UnexpectedEscape { ch }
            | Self::InvalidHexDigitInByteEscape { ch }
            | Self::InvalidHexDigitInUnicodeEscape { ch } => {
                DiagContext::new().with("ch", escape_char(ch))
            }
            Self::MissingDigitsAfterBasePrefix { base } => DiagContext::new().with("base", base),
            Self::InvalidDigitForBase { base, ch } => DiagContext::new()
                .with("base", base)
                .with("ch", escape_char(ch)),
            Self::InvalidUnicodeScalar { value } => {
                DiagContext::new().with("value", format!("{value:06X}"))
            }
            Self::UnterminatedStringLiteral
            | Self::UnterminatedRuneLiteral
            | Self::EmptyRuneLiteral
            | Self::RuneLiteralTooLong
            | Self::UnterminatedBlockComment
            | Self::UnterminatedTemplateLiteral
            | Self::UnexpectedUnderscoreInNumberLiteral
            | Self::MissingDigitAfterUnderscoreInNumberLiteral
            | Self::MissingExponentDigits
            | Self::MissingEscapeCode
            | Self::MissingHexDigitsInByteEscape
            | Self::MissingHexDigitsInUnicodeEscape
            | Self::ExpectedFourOrSixHexDigitsInUnicodeEscape => DiagContext::new(),
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
    pub const fn diag_kind(self) -> SyntaxDiagKind {
        diag_catalog_gen::parse_error_kind(self)
    }

    #[must_use]
    pub fn code(self) -> DiagCode {
        self.diag_kind().code()
    }

    #[must_use]
    pub fn headline(self) -> String {
        self.context().render(self.diag_kind().message())
    }

    #[must_use]
    pub fn context(self) -> DiagContext {
        match self {
            Self::ExpectedToken { expected, found } => DiagContext::new()
                .with("expected", expected)
                .with("found", found),
            Self::ExpectedExpression { found }
            | Self::ExpectedPattern { found }
            | Self::ExpectedMember { found }
            | Self::ExpectedIdentifier { found }
            | Self::ExpectedSpliceTarget { found }
            | Self::ExpectedOperatorMemberName { found }
            | Self::ExpectedFieldTarget { found }
            | Self::ExpectedConstraintOperator { found }
            | Self::ExpectedAttrValue { found } => DiagContext::new().with("found", found),
            Self::ReservedKeywordIdentifier { keyword } => {
                DiagContext::new().with("keyword", keyword)
            }
            Self::SpliceOutsideQuote | Self::NonAssociativeChain => DiagContext::new(),
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
            Self::ReservedKeywordIdentifier { keyword } => {
                format!("{keyword} found where identifier required")
            }
            Self::SpliceOutsideQuote => {
                format!("found {}", describe_span(source_text, span, "`#`"))
            }
            Self::NonAssociativeChain => "chain continues here".into(),
        };
        diag = diag.with_label(span, source_id, label);
        if let Some(hint) = self.diag_kind().hint() {
            diag = diag.with_hint(hint);
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
mod tests;
