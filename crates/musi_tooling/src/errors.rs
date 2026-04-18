use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::io::Error as IoError;
use std::path::PathBuf;

use music_base::diag::{CatalogDiagnostic, DiagCode, DiagContext};
use music_session::SessionError;

use crate::{ToolingDiagKind, diag::tooling_error_kind};

#[derive(Debug)]
pub enum ToolingError {
    MissingEntrySource { path: PathBuf },
    PackageImportRequiresMusi { spec: String },
    MissingImport { path: PathBuf, spec: String },
    ToolingIoFailed { path: PathBuf, source: IoError },
    SessionCompilationFailed(SessionError),
}

pub type ToolingResult<T = ()> = Result<T, ToolingError>;

impl ToolingError {
    #[must_use]
    pub const fn diag_kind(&self) -> ToolingDiagKind {
        tooling_error_kind(self)
    }

    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<ToolingDiagKind> {
        CatalogDiagnostic::new(self.diag_kind(), self.diag_context())
    }

    #[must_use]
    pub fn diag_code(&self) -> Option<DiagCode> {
        Some(self.diag_kind().code())
    }

    #[must_use]
    pub fn diag_message(&self) -> Option<Cow<'static, str>> {
        Some(Cow::Owned(self.diagnostic().message()))
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::MissingEntrySource { path } | Self::ToolingIoFailed { path, .. } => {
                let ctx = DiagContext::new().with("path", path.display());
                match self {
                    Self::ToolingIoFailed { source, .. } => ctx.with("source", source),
                    _ => ctx,
                }
            }
            Self::PackageImportRequiresMusi { spec } => DiagContext::new().with("spec", spec),
            Self::MissingImport { path, spec } => DiagContext::new()
                .with("path", path.display())
                .with("spec", spec),
            Self::SessionCompilationFailed(source) => DiagContext::new().with("source", source),
        }
    }
}

impl Display for ToolingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl Error for ToolingError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ToolingIoFailed { source, .. } => Some(source),
            Self::SessionCompilationFailed(source) => Some(source),
            _ => None,
        }
    }
}

impl From<SessionError> for ToolingError {
    fn from(value: SessionError) -> Self {
        Self::SessionCompilationFailed(value)
    }
}
