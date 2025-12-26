use musi_basic::error::{IntoMusiError, Level};
use musi_lex::token::TokenKind;
use thiserror::Error;

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

    fn level(&self) -> Level {
        Level::Error
    }
}
