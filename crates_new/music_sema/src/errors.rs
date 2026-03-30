use music_basic::{Diag, DiagCode, SourceId, Span};
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaError {
    pub kind: SemaErrorKind,
    pub source_id: SourceId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum SemaErrorKind {
    #[error("undefined binding '{name}'")]
    UndefinedBinding { name: String },

    #[error("duplicate binding '{name}'")]
    DuplicateBinding { name: String, first: Span },

    #[error("or-pattern bindings must match across alternatives")]
    OrPatternBindingsMismatch,

    #[error("unresolved import '{path}'")]
    UnresolvedImport { path: String },

    #[error("malformed syntax: {what}")]
    MalformedSyntax { what: &'static str },
}

impl SemaError {
    #[must_use]
    pub fn to_diag(&self) -> Diag {
        match &self.kind {
            SemaErrorKind::UndefinedBinding { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3001))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::DuplicateBinding { first, .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3002))
                .with_label(self.span, self.source_id, "redefined here")
                .with_label(*first, self.source_id, "previously defined here"),
            SemaErrorKind::MalformedSyntax { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3003))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::OrPatternBindingsMismatch => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3004))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::UnresolvedImport { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3005))
                .with_label(self.span, self.source_id, ""),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
