use music_bc::ArtifactError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum AssemblyError {
    #[error("{0}")]
    Artifact(#[from] ArtifactError),
    #[error("invalid binary header")]
    InvalidBinaryHeader,
    #[error("unsupported binary version {0}")]
    UnsupportedVersion(u16),
    #[error("unknown section tag {0}")]
    UnknownSection(u8),
    #[error("truncated binary payload")]
    TruncatedBinary,
    #[error("unknown opcode {0}")]
    UnknownOpcode(u16),
    #[error("text parse error: {0}")]
    Text(String),
}
