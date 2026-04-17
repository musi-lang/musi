use std::borrow::Cow;
use std::io::Error as IoError;
use std::path::PathBuf;

use music_base::diag::DiagCode;
use music_session::SessionError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ToolingError {
    #[error("entry source not found at `{path}`")]
    MissingEntrySource { path: PathBuf },
    #[error("import specifier `{spec}` uses package context and must run through `musi`")]
    PackageImportRequiresMusi { spec: String },
    #[error("import specifier `{spec}` not found from `{path}`")]
    MissingImport { path: PathBuf, spec: String },
    #[error("tooling I/O failed at `{path}`")]
    ToolingIoFailed {
        path: PathBuf,
        #[source]
        source: IoError,
    },
    #[error("session compilation failed")]
    SessionCompilationFailed(#[from] SessionError),
}

pub type ToolingResult<T = ()> = Result<T, ToolingError>;

impl ToolingError {
    #[must_use]
    pub const fn diag_code(&self) -> Option<DiagCode> {
        Some(DiagCode::new(match self {
            Self::MissingEntrySource { .. } => 3630,
            Self::PackageImportRequiresMusi { .. } => 3631,
            Self::MissingImport { .. } => 3632,
            Self::ToolingIoFailed { .. } => 3633,
            Self::SessionCompilationFailed(_) => return None,
        }))
    }

    #[must_use]
    pub fn diag_message(&self) -> Option<Cow<'static, str>> {
        Some(match self {
            Self::MissingEntrySource { .. } => Cow::Borrowed("entry source not found"),
            Self::PackageImportRequiresMusi { spec } => Cow::Owned(format!(
                "import specifier `{spec}` requires `musi` package context"
            )),
            Self::MissingImport { spec, .. } => {
                Cow::Owned(format!("import specifier `{spec}` not found"))
            }
            Self::ToolingIoFailed { .. } => Cow::Borrowed("tooling I/O failed"),
            Self::SessionCompilationFailed(_) => return None,
        })
    }
}
