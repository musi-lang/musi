//! Literal values and f-string parts.

#[cfg(test)]
mod tests;

use msc_shared::{Span, Symbol};

use crate::ExprIdx;

/// A literal value in the source.
#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int { value: i64, span: Span },
    Float { value: f64, span: Span },
    Str { value: Symbol, span: Span },
    FStr { parts: Vec<FStrPart>, span: Span },
    Rune { codepoint: u32, span: Span },
    Unit { span: Span },
}

impl Lit {
    /// Returns the span of this literal.
    #[must_use]
    pub const fn span(&self) -> Span {
        match *self {
            Self::Int { span, .. }
            | Self::Float { span, .. }
            | Self::Str { span, .. }
            | Self::FStr { span, .. }
            | Self::Rune { span, .. }
            | Self::Unit { span } => span,
        }
    }
}

/// A segment of an f-string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FStrPart {
    Text { raw: Symbol, span: Span },
    Interpolated { expr: ExprIdx, span: Span },
}
