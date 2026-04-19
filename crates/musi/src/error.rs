use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::path::PathBuf;

use musi_fmt::FormatError;
use musi_project::ProjectError;
use musi_rt::RuntimeError;
use musi_tooling::ToolingError;
use music_base::diag::{CatalogDiagnostic, DiagContext, display_catalog_or_source};
use music_session::SessionError;

use crate::diag::{CliDiagKind, cli_error_kind};

pub type MusiResult<T = ()> = Result<T, MusiError>;
#[derive(Debug)]
pub enum MusiError {
    ProjectModelFailed(ProjectError),
    SessionCompilationFailed(SessionError),
    RuntimeExecutionFailed(RuntimeError),
    ToolingIoFailed(ToolingError),
    FormattingFailed(FormatError),
    JsonSerializationFailed(serde_json::Error),
    MissingCurrentDirectory,
    TaskFailed {
        name: String,
    },
    UnsupportedRunArgs {
        argument: String,
    },
    PackageAlreadyInitialized {
        path: PathBuf,
    },
    MissingPackageName {
        path: PathBuf,
    },
    UnknownTarget {
        target: PathBuf,
    },
    CheckCommandFailed,
    CommandUnavailable {
        command: &'static str,
        feature: &'static str,
    },
    LspServerFailed {
        message: String,
    },
    IncompatibleCommandArgs {
        left: String,
        right: String,
    },
}

impl MusiError {
    fn diagnostic(&self) -> Option<CatalogDiagnostic<CliDiagKind>> {
        Some(CatalogDiagnostic::new(
            self.diag_kind()?,
            self.diag_context(),
        ))
    }

    const fn diag_kind(&self) -> Option<CliDiagKind> {
        cli_error_kind(self)
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::TaskFailed { name } => DiagContext::new().with("name", name),
            Self::UnsupportedRunArgs { argument } => DiagContext::new().with("argument", argument),
            Self::PackageAlreadyInitialized { path } | Self::MissingPackageName { path } => {
                DiagContext::new().with("path", path.display())
            }
            Self::UnknownTarget { target } => DiagContext::new().with("target", target.display()),
            Self::CommandUnavailable { command, feature } => DiagContext::new()
                .with("command", command)
                .with("feature", feature),
            Self::LspServerFailed { message } => DiagContext::new().with("message", message),
            Self::IncompatibleCommandArgs { left, right } => {
                DiagContext::new().with("left", left).with("right", right)
            }
            _ => DiagContext::new(),
        }
    }
}

impl From<ProjectError> for MusiError {
    fn from(source: ProjectError) -> Self {
        Self::ProjectModelFailed(source)
    }
}

impl From<SessionError> for MusiError {
    fn from(source: SessionError) -> Self {
        Self::SessionCompilationFailed(source)
    }
}

impl From<RuntimeError> for MusiError {
    fn from(source: RuntimeError) -> Self {
        Self::RuntimeExecutionFailed(source)
    }
}

impl From<ToolingError> for MusiError {
    fn from(source: ToolingError) -> Self {
        Self::ToolingIoFailed(source)
    }
}

impl From<FormatError> for MusiError {
    fn from(source: FormatError) -> Self {
        Self::FormattingFailed(source)
    }
}

impl From<serde_json::Error> for MusiError {
    fn from(source: serde_json::Error) -> Self {
        Self::JsonSerializationFailed(source)
    }
}

impl Display for MusiError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        display_catalog_or_source(
            self.diagnostic(),
            Error::source(self),
            "CLI diagnostic missing",
            f,
        )
    }
}

impl Error for MusiError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ProjectModelFailed(source) => Some(source),
            Self::SessionCompilationFailed(source) => Some(source),
            Self::RuntimeExecutionFailed(source) => Some(source),
            Self::ToolingIoFailed(source) => Some(source),
            Self::FormattingFailed(source) => Some(source),
            Self::JsonSerializationFailed(source) => Some(source),
            _ => None,
        }
    }
}
