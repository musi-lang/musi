use thiserror::Error;

pub type FrontendResult<T> = Result<T, FrontendError>;

#[derive(Debug, Error)]
#[error("{kind}")]
pub struct FrontendError {
    pub kind: FrontendErrorKind,
}

#[derive(Debug, Error)]
pub enum FrontendErrorKind {
    #[error("entry source missing")]
    EntrySourceMissing,
    #[error("import cycle")]
    ImportCycle,
    #[error("import target missing")]
    ImportTargetMissing,
    #[error("parse failed")]
    ParseFailed,
    #[error("analysis failed")]
    AnalyzeFailed,
    #[error("emit failed")]
    EmitFailed,
    #[error("encode failed")]
    EncodeFailed,
}
