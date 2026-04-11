use music_syntax::{Lexer, parse};
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub type SyntaxTermResult<T = ()> = Result<T, SyntaxTermError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SyntaxShape {
    Expr,
    Module,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SyntaxTerm {
    shape: SyntaxShape,
    text: Box<str>,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum SyntaxTermError {
    #[error("syntax fragment is empty")]
    Empty,
    #[error("syntax fragment parse failed")]
    Parse,
}

impl SyntaxTerm {
    pub fn parse(shape: SyntaxShape, text: &str) -> SyntaxTermResult<Self> {
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return Err(SyntaxTermError::Empty);
        }
        match shape {
            SyntaxShape::Expr => validate_expr_fragment(trimmed)?,
            SyntaxShape::Module => validate_module_fragment(trimmed)?,
        }
        Ok(Self {
            shape,
            text: trimmed.into(),
        })
    }

    pub fn from_quote_source(raw: &str) -> SyntaxTermResult<Self> {
        let trimmed = raw.trim();
        let Some(rest) = trimmed.strip_prefix("quote") else {
            return Err(SyntaxTermError::Parse);
        };
        let rest = rest.trim_start();
        if rest.len() < 2 {
            return Err(SyntaxTermError::Parse);
        }
        match (rest.chars().next(), rest.chars().last()) {
            (Some('('), Some(')')) => Ok(Self {
                shape: SyntaxShape::Expr,
                text: rest[1..rest.len() - 1].trim().into(),
            }),
            (Some('{'), Some('}')) => Ok(Self {
                shape: SyntaxShape::Module,
                text: rest[1..rest.len() - 1].trim().into(),
            }),
            _ => Err(SyntaxTermError::Parse),
        }
    }

    #[must_use]
    pub const fn shape(&self) -> SyntaxShape {
        self.shape
    }

    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }
}

fn validate_expr_fragment(text: &str) -> SyntaxTermResult {
    let wrapped = format!("{text};");
    let parsed = parse(Lexer::new(&wrapped).lex());
    if parsed.errors().is_empty() {
        Ok(())
    } else {
        Err(SyntaxTermError::Parse)
    }
}

fn validate_module_fragment(text: &str) -> SyntaxTermResult {
    let parsed = parse(Lexer::new(text).lex());
    if parsed.errors().is_empty() {
        Ok(())
    } else {
        Err(SyntaxTermError::Parse)
    }
}
