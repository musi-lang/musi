use std::borrow::Cow;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::io::Error as IoError;
use std::path::PathBuf;

use music_base::diag::{CatalogDiagnostic, Diag, DiagCode, DiagContext, OwnedSourceDiag};
use music_session::SessionError;

use crate::{ProjectDiagKind, diag::project_error_kind};

#[derive(Debug)]
pub enum ProjectError {
    MissingManifest {
        path: PathBuf,
    },
    MissingManifestAncestor {
        path: PathBuf,
    },
    MissingPackageRoot {
        path: PathBuf,
    },
    MissingModule {
        path: PathBuf,
    },
    InvalidManifestJson {
        path: PathBuf,
        source: serde_json::Error,
    },
    SourceDiagnostic(Box<OwnedSourceDiag>),
    ProjectIoFailed {
        path: PathBuf,
        source: IoError,
    },
    ManifestValidationFailed {
        message: String,
    },
    DuplicateWorkspaceMember {
        member: String,
    },
    DuplicatePackageName {
        name: String,
    },
    MissingPackageVersion {
        name: String,
    },
    NoRootPackage,
    MissingPackageEntryModule {
        package: String,
    },
    UnknownTask {
        name: String,
    },
    TaskDependencyCycle {
        name: String,
    },
    PackageDependencyCycle {
        name: String,
    },
    UnresolvedImport {
        spec: String,
    },
    UnknownPackage {
        name: String,
    },
    PackageGraphEntryMissing {
        name: String,
    },
    MissingRegistryRoot {
        name: String,
    },
    MissingGlobalCacheRoot,
    RegistryVersionNotFound {
        name: String,
        requirement: String,
    },
    InvalidVersionRequirement {
        name: String,
        requirement: String,
    },
    InvalidGitDependency {
        name: String,
        requirement: String,
    },
    GitCommandFailed {
        command: String,
        path: PathBuf,
    },
    GitReferenceNotFound {
        name: String,
        reference: String,
    },
    MissingFrozenLockfile {
        path: PathBuf,
    },
    FrozenLockfileOutOfDate {
        path: PathBuf,
    },
    SessionCompilationFailed(SessionError),
}

impl ProjectError {
    #[must_use]
    pub const fn source_diag(&self) -> Option<&OwnedSourceDiag> {
        match self {
            Self::SourceDiagnostic(diag) => Some(diag),
            _ => None,
        }
    }

    #[must_use]
    pub fn source_diag_rendered(&self) -> Option<&Diag> {
        self.source_diag().map(OwnedSourceDiag::diag)
    }

    #[must_use]
    pub const fn diag_kind(&self) -> Option<ProjectDiagKind> {
        project_error_kind(self)
    }

    #[must_use]
    pub fn diagnostic(&self) -> Option<CatalogDiagnostic<ProjectDiagKind>> {
        self.diag_kind()
            .map(|kind| CatalogDiagnostic::new(kind, self.diag_context()))
    }

    #[must_use]
    pub fn diag_code(&self) -> Option<DiagCode> {
        match self {
            Self::SourceDiagnostic(diag) => diag.diag().code(),
            _ => self.diag_kind().map(ProjectDiagKind::code),
        }
    }

    #[must_use]
    pub fn diag_message(&self) -> Option<Cow<'static, str>> {
        match self {
            Self::SourceDiagnostic(diag) => Some(Cow::Owned(diag.diag().message().to_owned())),
            _ => self.diagnostic().map(|diag| Cow::Owned(diag.message())),
        }
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::MissingManifest { path }
            | Self::MissingManifestAncestor { path }
            | Self::MissingPackageRoot { path }
            | Self::MissingModule { path }
            | Self::MissingFrozenLockfile { path }
            | Self::FrozenLockfileOutOfDate { path } => {
                DiagContext::new().with("path", path.display())
            }
            Self::InvalidManifestJson { path, source } => DiagContext::new()
                .with("path", path.display())
                .with("source", source),
            Self::ProjectIoFailed { path, source } => DiagContext::new()
                .with("path", path.display())
                .with("source", source),
            Self::SourceDiagnostic(diag) => {
                DiagContext::new().with("message", diag.diag().message())
            }
            Self::ManifestValidationFailed { message } => {
                DiagContext::new().with("message", message)
            }
            Self::DuplicateWorkspaceMember { member } => DiagContext::new().with("member", member),
            Self::DuplicatePackageName { name }
            | Self::MissingPackageVersion { name }
            | Self::UnknownTask { name }
            | Self::TaskDependencyCycle { name }
            | Self::PackageDependencyCycle { name }
            | Self::UnknownPackage { name }
            | Self::PackageGraphEntryMissing { name }
            | Self::MissingRegistryRoot { name } => DiagContext::new().with("name", name),
            Self::NoRootPackage | Self::MissingGlobalCacheRoot => DiagContext::new(),
            Self::MissingPackageEntryModule { package } => {
                DiagContext::new().with("package", package)
            }
            Self::UnresolvedImport { spec } => DiagContext::new().with("spec", spec),
            Self::RegistryVersionNotFound { name, requirement }
            | Self::InvalidVersionRequirement { name, requirement }
            | Self::InvalidGitDependency { name, requirement } => DiagContext::new()
                .with("name", name)
                .with("requirement", requirement),
            Self::GitCommandFailed { command, path } => DiagContext::new()
                .with("command", command)
                .with("path", path.display()),
            Self::GitReferenceNotFound { name, reference } => DiagContext::new()
                .with("name", name)
                .with("reference", reference),
            Self::SessionCompilationFailed(source) => DiagContext::new().with("source", source),
        }
    }
}

impl Display for ProjectError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(diag) = self.source_diag() {
            return Display::fmt(diag, f);
        }
        match self.diagnostic() {
            Some(diag) => Display::fmt(&diag, f),
            None => f.write_str("project diagnostic missing"),
        }
    }
}

impl Error for ProjectError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::InvalidManifestJson { source, .. } => Some(source),
            Self::ProjectIoFailed { source, .. } => Some(source),
            Self::SessionCompilationFailed(source) => Some(source),
            _ => None,
        }
    }
}

impl From<SessionError> for ProjectError {
    fn from(value: SessionError) -> Self {
        Self::SessionCompilationFailed(value)
    }
}
