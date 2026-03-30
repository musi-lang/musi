use thiserror::Error;

pub type EmitResult<T> = Result<T, EmitError>;

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct EmitError {
    pub kind: EmitErrorKind,
}

#[derive(Debug, Error)]
pub enum EmitErrorKind {
    #[error("emit failed")]
    EmitFailed,
    #[error("syntax value has no runtime lowering")]
    SyntaxNotLowerable,
    #[error("import target missing")]
    ImportTargetMissing,
    #[error("unsupported expression form")]
    UnsupportedExpr,
    #[error("unsupported pattern form")]
    UnsupportedPat,
}
