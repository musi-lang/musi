use music_basic::{Diag, DiagCode, SourceId, Span};
use music_lex::{LexErrorKind, TokenKind, display_token_kind};
use thiserror::Error;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ParseErrorKind {
    #[error(
        "expected {}, found {}",
        display_token_kind(.expected.as_ref()),
        display_token_kind(.found.as_ref())
    )]
    ExpectedToken {
        expected: Box<TokenKind>,
        found: Box<TokenKind>,
    },
    #[error("import does not support 'as' aliasing; use let binding")]
    ImportAliasNotSupported,
    #[error("expected expression, found {}", display_token_kind(.found.as_ref()))]
    ExpectedExpression { found: Box<TokenKind> },
    #[error("expected pattern, found {}", display_token_kind(.found.as_ref()))]
    ExpectedPattern { found: Box<TokenKind> },
    #[error("expected type, found {}", display_token_kind(.found.as_ref()))]
    ExpectedType { found: Box<TokenKind> },
    #[error("expected member, found {}", display_token_kind(.found.as_ref()))]
    ExpectedMember { found: Box<TokenKind> },
    #[error("expected identifier, found {}", display_token_kind(.found.as_ref()))]
    ExpectedIdentifier { found: Box<TokenKind> },
    #[error("expected string literal, found {}", display_token_kind(.found.as_ref()))]
    ExpectedStringLiteral { found: Box<TokenKind> },
    #[error("expected splice target, found {}", display_token_kind(.found.as_ref()))]
    ExpectedSpliceTarget { found: Box<TokenKind> },
    #[error("splice outside quote")]
    SpliceOutsideQuote,
    #[error("expected operator member name, found {}", display_token_kind(.found.as_ref()))]
    ExpectedOperatorMemberName { found: Box<TokenKind> },
    #[error("expected effect item, found {}", display_token_kind(.found.as_ref()))]
    ExpectedEffectItem { found: Box<TokenKind> },
    #[error("expected effect row remainder name after '...', found {}", display_token_kind(.found.as_ref()))]
    ExpectedEffectRemainderName { found: Box<TokenKind> },
    #[error("effect row remainder must be final entry in effect set")]
    EffectRemainderMustBeLast,
    #[error("expected foreign binding, found {}", display_token_kind(.found.as_ref()))]
    ExpectedForeignBinding { found: Box<TokenKind> },
    #[error("expected data member, found {}", display_token_kind(.found.as_ref()))]
    ExpectedDataMember { found: Box<TokenKind> },
    #[error("expected array dimension, found {}", display_token_kind(.found.as_ref()))]
    ExpectedArrayDimension { found: Box<TokenKind> },
    #[error("expected field name or tuple index, found {}", display_token_kind(.found.as_ref()))]
    ExpectedFieldTarget { found: Box<TokenKind> },
    #[error("expected constraint operator '<:' or ':', found {}", display_token_kind(.found.as_ref()))]
    ExpectedConstraintOperator { found: Box<TokenKind> },
    #[error(
        "expected attribute value, found {}",
        display_token_kind(.found.as_ref())
    )]
    ExpectedAttrValue { found: Box<TokenKind> },
    #[error("invalid attribute target before {}", display_token_kind(.found.as_ref()))]
    InvalidAttributeTarget { found: Box<TokenKind> },
    #[error("non-associative comparison chain")]
    NonAssociativeChain,
    #[error(
        "record literal starts with '{{' ('.{{' is record update syntax and requires receiver)"
    )]
    RecordLiteralUsesDotBrace,
    #[error("record pattern starts with '{{'")]
    RecordPatternUsesDotBrace,
    #[error("invalid f-string interpolation; {kind}")]
    InvalidFStringInterpolation { kind: Box<LexErrorKind> },
}

impl ParseError {
    #[must_use]
    pub fn to_diag(&self, source_id: SourceId) -> Diag {
        let code = match &self.kind {
            ParseErrorKind::ExpectedToken { .. } => 2001,
            ParseErrorKind::ImportAliasNotSupported => 2002,
            ParseErrorKind::ExpectedExpression { .. } => 2003,
            ParseErrorKind::ExpectedPattern { .. } => 2004,
            ParseErrorKind::ExpectedType { .. } => 2005,
            ParseErrorKind::ExpectedMember { .. } => 2006,
            ParseErrorKind::ExpectedIdentifier { .. } => 2007,
            ParseErrorKind::ExpectedStringLiteral { .. } => 2008,
            ParseErrorKind::ExpectedSpliceTarget { .. } => 2009,
            ParseErrorKind::SpliceOutsideQuote => 2025,
            ParseErrorKind::ExpectedOperatorMemberName { .. } => 2010,
            ParseErrorKind::ExpectedEffectItem { .. } => 2011,
            ParseErrorKind::ExpectedEffectRemainderName { .. } => 2012,
            ParseErrorKind::EffectRemainderMustBeLast => 2013,
            ParseErrorKind::ExpectedForeignBinding { .. } => 2014,
            ParseErrorKind::ExpectedDataMember { .. } => 2015,
            ParseErrorKind::ExpectedArrayDimension { .. } => 2016,
            ParseErrorKind::ExpectedFieldTarget { .. } => 2017,
            ParseErrorKind::ExpectedConstraintOperator { .. } => 2018,
            ParseErrorKind::InvalidAttributeTarget { .. } => 2019,
            ParseErrorKind::NonAssociativeChain => 2020,
            ParseErrorKind::RecordLiteralUsesDotBrace => 2021,
            ParseErrorKind::RecordPatternUsesDotBrace => 2022,
            ParseErrorKind::InvalidFStringInterpolation { .. } => 2023,
            ParseErrorKind::ExpectedAttrValue { .. } => 2024,
        };

        Diag::error(self.kind.to_string())
            .with_code(DiagCode::new(code))
            .with_label(self.span, source_id, "")
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
