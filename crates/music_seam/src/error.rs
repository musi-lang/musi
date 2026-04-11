use crate::ArtifactError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum AssemblyError {
    #[error("{0}")]
    ArtifactValidationFailed(#[from] ArtifactError),
    #[error("invalid binary header")]
    InvalidBinaryHeader,
    #[error("binary version `{0}` unsupported")]
    BinaryVersionUnsupported(u16),
    #[error("section tag `{0}` unknown")]
    SectionTagUnknown(u8),
    #[error("binary payload truncated")]
    BinaryPayloadTruncated,
    #[error("opcode `{0}` unknown")]
    OpcodeUnknown(u16),
    #[error("text parse failed (`{0}`)")]
    TextParseFailed(String),
}
