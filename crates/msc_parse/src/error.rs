//! Structured parse errors.

use msc_lex::token::TokenKind;
use msc_shared::{IntoDiagnostic, Severity};

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
    #[error("expected 'let', 'var', 'class', 'given', 'effect', 'foreign', or '{{' after 'export'")]
    ExpectedAfterExport,
    #[error("expected declaration after attributes")]
    ExpectedDeclAfterAttrs,
    #[error("unexpected {kind} `{text}`")]
    UnexpectedKind { kind: TokenKind, text: Box<str> },
    #[error("unterminated interpolated string literal")]
    UnterminatedFString,
    #[error("expected type variable (e.g. 'T)")]
    ExpectedTypeVariable,
    #[error("invalid integer literal")]
    InvalidIntLiteral,
    #[error("invalid float literal")]
    InvalidFloatLiteral,
    #[error("invalid rune escape sequence")]
    InvalidRuneEscape,
}

impl ParseError {
    pub const fn expected_token(expected: TokenKind, found: TokenKind) -> Self {
        Self::ExpectedToken { expected, found }
    }

    pub const fn unexpected_kind(kind: TokenKind, text: Box<str>) -> Self {
        Self::UnexpectedKind { kind, text }
    }
}

impl IntoDiagnostic for ParseError {
    fn severity(&self) -> Severity {
        Severity::Error
    }
}
