use music_basic::{Diag, DiagCode, SourceId, Span};
use thiserror::Error;

pub type SemaErrorKinds = Vec<SemaErrorKind>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaError {
    pub kind: SemaErrorKind,
    pub source_id: SourceId,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum SemaErrorKind {
    #[error("expected type '{expected}', found '{found}'")]
    TypeMismatch { expected: String, found: String },

    #[error("effectful expression requires explicit 'with {{ ... }}' signature")]
    MissingWithClause,

    #[error("invalid perform target")]
    InvalidPerformTarget,

    #[error("unknown effect '{name}'")]
    UnknownEffect { name: String },

    #[error("unknown effect operation '{op}' for effect '{effect}'")]
    UnknownEffectOp { effect: String, op: String },

    #[error("handle requires exactly one value clause")]
    HandleValueClauseRequired,

    #[error("resume may only appear inside operation clause body")]
    ResumeOutsideOpClause,

    #[error("effect '{name}' not declared in this signature's effect row")]
    EffectNotDeclared { name: String },

    #[error("effect row remainder not declared in this signature's effect row")]
    EffectRemainderNotDeclared,
}

impl SemaError {
    #[must_use]
    pub fn to_diag(&self) -> Diag {
        match &self.kind {
            SemaErrorKind::TypeMismatch { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3006))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::MissingWithClause => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3007))
                .with_label(self.span, self.source_id, "")
                .with_hint("add 'with { ... }' clause to declaration signature"),
            SemaErrorKind::InvalidPerformTarget => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3008))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::UnknownEffect { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3009))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::UnknownEffectOp { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3010))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::HandleValueClauseRequired => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3011))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::ResumeOutsideOpClause => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3012))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::EffectNotDeclared { .. } => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3013))
                .with_label(self.span, self.source_id, ""),
            SemaErrorKind::EffectRemainderNotDeclared => Diag::error(self.kind.to_string())
                .with_code(DiagCode::new(3014))
                .with_label(self.span, self.source_id, "")
                .with_hint("make signature effect row open by adding remainder like '...r'"),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
