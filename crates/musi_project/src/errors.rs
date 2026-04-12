use std::borrow::Cow;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::io::Error as IoError;
use std::path::{Path, PathBuf};

use music_base::diag::{Diag, DiagCode};
use music_base::{SourceId, Span};
use music_session::SessionError;
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct ProjectSourceLabel {
    span: Span,
    message: String,
}

impl ProjectSourceLabel {
    #[must_use]
    pub fn new(span: Span, message: impl Into<String>) -> Self {
        Self {
            span,
            message: message.into(),
        }
    }

    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }

    #[must_use]
    pub const fn message(&self) -> &str {
        self.message.as_str()
    }
}

#[derive(Debug, Clone)]
pub struct ProjectSourceDiagnostic {
    path: PathBuf,
    text: String,
    code: DiagCode,
    message: String,
    primary_label: ProjectSourceLabel,
    secondary_labels: Vec<ProjectSourceLabel>,
    notes: Vec<String>,
    hint: Option<String>,
}

impl ProjectSourceDiagnostic {
    #[must_use]
    pub fn new(
        path: PathBuf,
        text: String,
        code: DiagCode,
        message: impl Into<String>,
        primary_label: ProjectSourceLabel,
    ) -> Self {
        Self {
            path,
            text,
            code,
            message: message.into(),
            primary_label,
            secondary_labels: Vec::new(),
            notes: Vec::new(),
            hint: None,
        }
    }

    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

    #[must_use]
    pub const fn code(&self) -> DiagCode {
        self.code
    }

    #[must_use]
    pub const fn message(&self) -> &str {
        self.message.as_str()
    }

    #[must_use]
    pub const fn primary_label(&self) -> &ProjectSourceLabel {
        &self.primary_label
    }

    #[must_use]
    pub const fn secondary_labels(&self) -> &[ProjectSourceLabel] {
        self.secondary_labels.as_slice()
    }

    #[must_use]
    pub const fn notes(&self) -> &[String] {
        self.notes.as_slice()
    }

    #[must_use]
    pub fn hint(&self) -> Option<&str> {
        self.hint.as_deref()
    }

    pub fn set_hint(&mut self, hint: impl Into<String>) {
        self.hint = Some(hint.into());
    }

    #[must_use]
    pub fn to_diag(&self, source_id: SourceId) -> Diag {
        let mut diag = Diag::error(self.message.clone())
            .with_code(self.code)
            .with_label(
                self.primary_label.span(),
                source_id,
                self.primary_label.message(),
            );
        for label in &self.secondary_labels {
            diag = diag.with_label(label.span(), source_id, label.message());
        }
        for note in &self.notes {
            diag = diag.with_note(note.clone());
        }
        if let Some(hint) = &self.hint {
            diag = diag.with_hint(hint.clone());
        }
        diag
    }
}

impl Display for ProjectSourceDiagnostic {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.write_str(self.message())
    }
}

#[derive(Debug, Error)]
pub enum ProjectError {
    #[error("manifest not found at `{path}`")]
    MissingManifest { path: PathBuf },
    #[error("ancestor manifest not found from `{path}`")]
    MissingManifestAncestor { path: PathBuf },
    #[error("package root not found at `{path}`")]
    MissingPackageRoot { path: PathBuf },
    #[error("module path `{path}` does not exist")]
    MissingModule { path: PathBuf },
    #[error("invalid manifest at `{path}`")]
    ManifestJsonInvalid {
        path: PathBuf,
        #[source]
        source: serde_json::Error,
    },
    #[error("{0}")]
    ManifestSourceDiagnostic(Box<ProjectSourceDiagnostic>),
    #[error("project I/O failed at `{path}`")]
    ProjectIoFailed {
        path: PathBuf,
        #[source]
        source: IoError,
    },
    #[error("manifest validation failed (`{message}`)")]
    ManifestValidationFailed { message: String },
    #[error("duplicate workspace member `{member}`")]
    DuplicateWorkspaceMember { member: String },
    #[error("duplicate package `{name}` in project graph")]
    DuplicatePackageName { name: String },
    #[error("missing version for package `{name}`")]
    MissingPackageVersion { name: String },
    #[error("no package entry for root package")]
    NoRootPackage,
    #[error("package `{package}` entry module not found")]
    PackageEntryModuleMissing { package: String },
    #[error("task `{name}` not found")]
    UnknownTask { name: String },
    #[error("task dependency cycle reaches `{name}`")]
    TaskDependencyCycle { name: String },
    #[error("package dependency cycle reaches `{name}`")]
    PackageDependencyCycle { name: String },
    #[error("dependency `{name}` could not be resolved")]
    UnresolvedDependency { name: String },
    #[error("registry root is required to resolve `{name}`")]
    MissingRegistryRoot { name: String },
    #[error("package cache root is required to resolve `{name}`")]
    MissingCacheRoot { name: String },
    #[error("no registry versions found for `{name}` matching `{requirement}`")]
    RegistryVersionNotFound { name: String, requirement: String },
    #[error("invalid semver requirement `{requirement}` for `{name}`")]
    InvalidVersionRequirement { name: String, requirement: String },
    #[error("lockfile at `{path}` is required because frozen mode is enabled")]
    MissingFrozenLockfile { path: PathBuf },
    #[error("lockfile at `{path}` is out of date")]
    FrozenLockfileOutOfDate { path: PathBuf },
    #[error("session compilation failed")]
    SessionCompilationFailed(#[from] SessionError),
}

impl ProjectError {
    #[must_use]
    pub const fn source_diag(&self) -> Option<&ProjectSourceDiagnostic> {
        match self {
            Self::ManifestSourceDiagnostic(diag) => Some(diag),
            _ => None,
        }
    }

    #[must_use]
    pub const fn diag_code(&self) -> Option<DiagCode> {
        Some(DiagCode::new(match self {
            Self::MissingManifest { .. } => 3600,
            Self::MissingManifestAncestor { .. } => 3601,
            Self::MissingPackageRoot { .. } => 3602,
            Self::MissingModule { .. } => 3603,
            Self::ManifestJsonInvalid { .. } => 3604,
            Self::ManifestSourceDiagnostic(diag) => return Some(diag.code()),
            Self::ProjectIoFailed { .. } => 3605,
            Self::ManifestValidationFailed { .. } => 3606,
            Self::DuplicateWorkspaceMember { .. } => 3607,
            Self::DuplicatePackageName { .. } => 3608,
            Self::MissingPackageVersion { .. } => 3609,
            Self::NoRootPackage => 3610,
            Self::PackageEntryModuleMissing { .. } => 3611,
            Self::UnknownTask { .. } => 3612,
            Self::TaskDependencyCycle { .. } => 3613,
            Self::PackageDependencyCycle { .. } => 3614,
            Self::UnresolvedDependency { .. } => 3615,
            Self::MissingRegistryRoot { .. } => 3616,
            Self::MissingCacheRoot { .. } => 3617,
            Self::RegistryVersionNotFound { .. } => 3618,
            Self::InvalidVersionRequirement { .. } => 3619,
            Self::MissingFrozenLockfile { .. } => 3620,
            Self::FrozenLockfileOutOfDate { .. } => 3621,
            Self::SessionCompilationFailed(_) => return None,
        }))
    }

    #[must_use]
    pub fn diag_message(&self) -> Option<Cow<'static, str>> {
        Some(match self {
            Self::MissingManifest { .. } => Cow::Borrowed("manifest not found"),
            Self::MissingManifestAncestor { .. } => Cow::Borrowed("ancestor manifest not found"),
            Self::MissingPackageRoot { .. } => Cow::Borrowed("package root not found"),
            Self::MissingModule { .. } => Cow::Borrowed("module path not found"),
            Self::ManifestJsonInvalid { .. } => Cow::Borrowed("manifest JSON invalid"),
            Self::ManifestSourceDiagnostic(diag) => Cow::Owned(diag.message().to_owned()),
            Self::ProjectIoFailed { .. } => Cow::Borrowed("project I/O failed"),
            Self::ManifestValidationFailed { message } => {
                Cow::Owned(format!("manifest validation failed (`{message}`)"))
            }
            Self::DuplicateWorkspaceMember { member } => {
                Cow::Owned(format!("duplicate workspace member `{member}`"))
            }
            Self::DuplicatePackageName { name } => {
                Cow::Owned(format!("duplicate package `{name}`"))
            }
            Self::MissingPackageVersion { name } => {
                Cow::Owned(format!("package `{name}` is missing version"))
            }
            Self::NoRootPackage => Cow::Borrowed("root package entry is missing"),
            Self::PackageEntryModuleMissing { package } => {
                Cow::Owned(format!("package `{package}` entry module not found"))
            }
            Self::UnknownTask { name } => Cow::Owned(format!("task `{name}` not found")),
            Self::TaskDependencyCycle { name } => {
                Cow::Owned(format!("task dependency cycle reaches `{name}`"))
            }
            Self::PackageDependencyCycle { name } => {
                Cow::Owned(format!("package dependency cycle reaches `{name}`"))
            }
            Self::UnresolvedDependency { name } => {
                Cow::Owned(format!("dependency `{name}` could not be resolved"))
            }
            Self::MissingRegistryRoot { name } => {
                Cow::Owned(format!("registry root is required for `{name}`"))
            }
            Self::MissingCacheRoot { name } => {
                Cow::Owned(format!("package cache root is required for `{name}`"))
            }
            Self::RegistryVersionNotFound { name, requirement } => Cow::Owned(format!(
                "registry version for `{name}` matching `{requirement}` not found"
            )),
            Self::InvalidVersionRequirement { name, requirement } => Cow::Owned(format!(
                "semver requirement `{requirement}` for `{name}` is invalid"
            )),
            Self::MissingFrozenLockfile { .. } => Cow::Borrowed("frozen lockfile is required"),
            Self::FrozenLockfileOutOfDate { .. } => Cow::Borrowed("frozen lockfile is out of date"),
            Self::SessionCompilationFailed(_) => return None,
        })
    }
}
