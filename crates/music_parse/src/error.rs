//! Structured parse errors.

use music_lex::token::TokenKind;
use music_shared::{IntoDiagnostic, Severity};

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("expected {expected}, found {found}")]
    ExpectedToken {
        expected: TokenKind,
        found: TokenKind,
    },
    #[error("expected identifier")]
    ExpectedIdent,
    #[error("expected literal")]
    ExpectedLiteral,
    #[error("expected type")]
    ExpectedType,
    #[error("expected pattern")]
    ExpectedPattern,
    #[error("expected parameter name")]
    ExpectedParamName,
    #[error("expected string literal after 'import'")]
    ExpectedImportPath,
    #[error("expected 'let', 'var', 'effect', or '{{' after 'export'")]
    ExpectedAfterExport,
    #[error("expected declaration after attributes")]
    ExpectedDeclAfterAttrs,
    #[error("expected record fields or refinement type")]
    ExpectedRecordOrRefinement,
    #[error("unexpected {kind}")]
    UnexpectedKind { kind: TokenKind },
    #[error("unterminated interpolated string literal")]
    UnterminatedFString,
}

impl ParseError {
    pub const fn expected_token(expected: TokenKind, found: TokenKind) -> Self {
        Self::ExpectedToken { expected, found }
    }

    pub const fn unexpected_kind(kind: TokenKind) -> Self {
        Self::UnexpectedKind { kind }
    }
}

impl IntoDiagnostic for ParseError {
    fn severity(&self) -> Severity {
        Severity::Error
    }
}
