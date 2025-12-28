use musi_basic::error::Level;
use musi_lex::token::TokenKind;
use musi_macros::MusiError;
use thiserror::Error;

#[derive(Debug, Clone, Error, MusiError)]
#[non_exhaustive]
pub enum ParseErrorKind {
    #[ms(2001)]
    #[error("expected identifier")]
    ExpectedIdent,
    #[ms(2002)]
    #[error("expected expression")]
    ExpectedExpr,
    #[ms(2003)]
    #[error("expected type")]
    ExpectedTyp,
    #[ms(2004)]
    #[error("expected pattern")]
    ExpectedPat,
    #[ms(2005)]
    #[error("expected literal")]
    ExpectedLit,
    #[ms(2006)]
    #[error("expected string literal")]
    ExpectedStringLit,
    #[ms(2007)]
    #[error("expected '{0}'")]
    ExpectedToken(TokenKind),
    #[ms(2008)]
    #[error("expected '{0}' separator")]
    ExpectedSeparator(TokenKind),

    #[ms(2009)]
    #[error("unexpected '{0}'")]
    UnexpectedToken(TokenKind),
    #[ms(2010)]
    #[error("unexpected end of file")]
    UnexpectedEof,

    #[ms(2011)]
    #[error("unclosed '{0}' delimiter")]
    UnclosedDelimiter(TokenKind),
    #[ms(2012)]
    #[error("unclosed attribute")]
    UnclosedAttr,
    #[ms(2013)]
    #[error("unclosed template expression")]
    UnclosedTemplateExpr,
}

impl ParseErrorKind {
    #[must_use]
    pub const fn hint(&self) -> Option<&'static str> {
        match self {
            Self::UnclosedAttr => Some("add ']'"),
            Self::UnclosedTemplateExpr => Some("add '}'"),
            _ => None,
        }
    }

    #[must_use]
    pub const fn level(&self) -> Level {
        Level::Error
    }
}
