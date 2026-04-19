use std::error::Error;
use std::fmt::{self, Display, Formatter};

use music_base::diag::{CatalogDiagnostic, DiagContext};

use crate::{ArtifactError, SeamDiagKind, diag::assembly_error_kind};

#[derive(Debug)]
pub enum AssemblyError {
    ArtifactValidationFailed(ArtifactError),
    InvalidBinaryHeader,
    UnsupportedBinaryVersion(u16),
    UnknownSectionTag(u8),
    BinaryPayloadTruncated,
    UnknownOpcode(u16),
    TextParseFailed(String),
}

impl AssemblyError {
    #[must_use]
    pub const fn diag_kind(&self) -> SeamDiagKind {
        assembly_error_kind(self)
    }

    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<SeamDiagKind> {
        CatalogDiagnostic::new(self.diag_kind(), self.diag_context())
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::ArtifactValidationFailed(source) => DiagContext::new().with("source", source),
            Self::InvalidBinaryHeader | Self::BinaryPayloadTruncated => DiagContext::new(),
            Self::UnsupportedBinaryVersion(version) => DiagContext::new().with("version", *version),
            Self::UnknownSectionTag(tag) => DiagContext::new().with("tag", *tag),
            Self::UnknownOpcode(opcode) => DiagContext::new().with("opcode", *opcode),
            Self::TextParseFailed(source) => DiagContext::new().with("source", source),
        }
    }
}

impl Display for AssemblyError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl Error for AssemblyError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ArtifactValidationFailed(source) => Some(source),
            _ => None,
        }
    }
}

impl From<ArtifactError> for AssemblyError {
    fn from(value: ArtifactError) -> Self {
        Self::ArtifactValidationFailed(value)
    }
}
