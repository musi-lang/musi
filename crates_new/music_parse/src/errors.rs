use music_basic::{Diag, DiagCode, SourceId, Span};
use music_lex::{TokenKind, display_token_kind};
use thiserror::Error;

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ParseErrorKind {
    #[error("expected token {}, found {}", display_token_kind(.expected.as_ref()), display_token_kind(.found.as_ref()))]
    ExpectedToken {
        expected: Box<TokenKind>,
        found: Box<TokenKind>,
    },
    #[error("import does not support 'as' aliasing; use a let binding")]
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
    #[error("expected operator member name, found {}", display_token_kind(.found.as_ref()))]
    ExpectedOperatorMemberName { found: Box<TokenKind> },
    #[error("expected effect item, found {}", display_token_kind(.found.as_ref()))]
    ExpectedEffectItem { found: Box<TokenKind> },
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
    #[error("invalid attribute target before {}", display_token_kind(.found.as_ref()))]
    InvalidAttributeTarget { found: Box<TokenKind> },
    #[error("non-associative comparison chain")]
    NonAssociativeChain,
    #[error(
        "record literal starts with '{{' ('.{{' is record update syntax and requires a receiver)"
    )]
    RecordLiteralUsesDotBrace,
    #[error("record pattern starts with '{{'")]
    RecordPatternUsesDotBrace,
}

impl ParseError {
    #[must_use]
    pub fn to_diag(&self, source_id: SourceId) -> Diag {
        Diag::error(self.kind.to_string())
            .with_code(DiagCode::new(2001))
            .with_label(self.span, source_id, "")
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
