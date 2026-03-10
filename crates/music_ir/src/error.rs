//! Structured IR lowering errors.

use music_shared::{IntoDiagnostic, Severity};

#[derive(Debug, thiserror::Error)]
pub enum IrError {
    #[error("unresolved type variable during lowering")]
    UnresolvedTypeVariable,
    #[error("monomorphization depth exceeded for `{name}`")]
    MonoDepthExceeded { name: Box<str> },
    #[error("unsupported expression in IR lowering")]
    UnsupportedExpr,
    #[error("effect `{name}` not found in well-known registry")]
    UnknownEffect { name: Box<str> },
    #[error("duplicate #[entrypoint] attribute")]
    DuplicateEntryPoint,
}

impl IntoDiagnostic for IrError {
    fn severity(&self) -> Severity {
        Severity::Error
    }
}
