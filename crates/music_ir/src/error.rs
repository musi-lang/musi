//! Structured IR lowering errors.

use music_shared::{FileId, IntoDiagnostic, Severity, Span};

/// A spanned IR error, pairing a diagnostic with its source location.
pub struct SpannedIrError {
    pub error: IrError,
    pub span: Span,
    /// When set, overrides the default file used for diagnostic rendering.
    /// Populated by multi-module lowering when the error originates from a
    /// dependency module rather than the entry module.
    pub file_id: Option<FileId>,
}

#[derive(Debug, thiserror::Error)]
pub enum IrError {
    #[error("unresolved type variable during lowering")]
    UnresolvedTypeVariable,
    #[error("monomorphization depth exceeded for `{name}`")]
    MonoDepthExceeded { name: Box<str> },
    #[error("{kind} not yet supported")]
    UnsupportedExpr { kind: &'static str },
    #[error("operator `{op}` not yet supported")]
    UnsupportedOp { op: &'static str },
    #[error("non-primitive type not yet supported in IR")]
    UnsupportedType,
    #[error("pattern {kind} not yet supported")]
    UnsupportedPattern { kind: &'static str },
    #[error("missing type information for expression")]
    MissingExprType,
    #[error("unresolved name reference")]
    UnresolvedName,
    #[error("index overflow during lowering")]
    IndexOverflow,
    #[error("unknown effect `{name}`")]
    UnknownEffect { name: Box<str> },
    #[error("duplicate #[entrypoint] attribute")]
    DuplicateEntryPoint,
}

impl IrError {
    #[must_use]
    pub const fn at(self, span: Span) -> SpannedIrError {
        SpannedIrError {
            error: self,
            span,
            file_id: None,
        }
    }
}

impl IntoDiagnostic for IrError {
    fn severity(&self) -> Severity {
        Severity::Error
    }
}
