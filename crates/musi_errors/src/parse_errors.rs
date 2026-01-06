use musi_basic::token::TokenKind;
use thiserror::Error;

use crate::IntoMusiError;

#[derive(Debug, Clone, Error)]
#[non_exhaustive]
pub enum ParseErrorKind {
    #[error("expected identifier")]
    ExpectedIdent,
    #[error("expected expression")]
    ExpectedExpr,
    #[error("expected type")]
    ExpectedTyp,
    #[error("expected pattern")]
    ExpectedPat,
    #[error("expected literal")]
    ExpectedLit,
    #[error("invalid literal")]
    InvalidLiteral,
    #[error("expected string literal")]
    ExpectedStringLit,
    #[error("expected '{0}'")]
    ExpectedToken(TokenKind),
    #[error("expected '{0}' separator")]
    ExpectedSeparator(TokenKind),

    #[error("unexpected '{0}'")]
    UnexpectedToken(TokenKind),
    #[error("unexpected end of file")]
    UnexpectedEof,

    #[error("unclosed '{0}' delimiter")]
    UnclosedDelimiter(TokenKind),
    #[error("unclosed attribute")]
    UnclosedAttr,
    #[error("unclosed template expression")]
    UnclosedTemplateExpr,
}

impl IntoMusiError for ParseErrorKind {
    fn hint(&self) -> Option<&'static str> {
        match self {
            Self::UnclosedAttr => Some("add ']'"),
            Self::UnclosedTemplateExpr => Some("add '}'"),
            _ => None,
        }
    }
}
