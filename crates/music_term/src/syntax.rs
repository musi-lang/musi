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
    FragmentEmpty,
    #[error("syntax fragment parse failed")]
    FragmentParseFailed,
}

impl SyntaxTerm {
    /// # Errors
    ///
    /// Returns [`SyntaxTermError`] when the fragment is empty or does not parse as the requested
    /// syntax shape.
    pub fn parse(shape: SyntaxShape, text: &str) -> SyntaxTermResult<Self> {
        let trimmed = text.trim();
        if trimmed.is_empty() {
            return Err(SyntaxTermError::FragmentEmpty);
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

    /// # Errors
    ///
    /// Returns [`SyntaxTermError`] when `raw` is not a `quote(...)` or `quote { ... }` form.
    pub fn from_quote_source(raw: &str) -> SyntaxTermResult<Self> {
        let trimmed = raw.trim();
        let Some(rest) = trimmed.strip_prefix("quote") else {
            return Err(SyntaxTermError::FragmentParseFailed);
        };
        let rest = rest.trim_start();
        if let Some(inner) = strip_wrapped_fragment(rest, '(', ')') {
            return Ok(Self {
                shape: SyntaxShape::Expr,
                text: inner.trim().into(),
            });
        }
        if let Some(inner) = strip_wrapped_fragment(rest, '{', '}') {
            return Ok(Self {
                shape: SyntaxShape::Module,
                text: inner.trim().into(),
            });
        }
        Err(SyntaxTermError::FragmentParseFailed)
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
        Err(SyntaxTermError::FragmentParseFailed)
    }
}

fn validate_module_fragment(text: &str) -> SyntaxTermResult {
    let parsed = parse(Lexer::new(text).lex());
    if parsed.errors().is_empty() {
        Ok(())
    } else {
        Err(SyntaxTermError::FragmentParseFailed)
    }
}

fn strip_wrapped_fragment(text: &str, open: char, close: char) -> Option<&str> {
    let inner = text.strip_prefix(open)?;
    inner.strip_suffix(close)
}
