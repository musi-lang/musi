use std::borrow::Cow;
use std::io::Error as IoError;
use std::path::PathBuf;

use music_base::diag::{Diag, DiagCode, OwnedSourceDiag};
use music_session::SessionError;
use thiserror::Error;

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
    SourceDiagnostic(Box<OwnedSourceDiag>),
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
    #[error("unresolved import `{spec}`")]
    UnresolvedImport { spec: String },
    #[error("unknown package `{name}`")]
    UnknownPackage { name: String },
    #[error("package graph entry missing `{name}`")]
    PackageGraphEntryMissing { name: String },
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
    const fn manifest_diag_code(&self) -> Option<DiagCode> {
        let code = match self {
            Self::MissingManifest { .. } => 3600,
            Self::MissingManifestAncestor { .. } => 3601,
            Self::MissingPackageRoot { .. } => 3602,
            Self::MissingModule { .. } => 3603,
            Self::ManifestJsonInvalid { .. } => 3604,
            Self::ProjectIoFailed { .. } => 3605,
            Self::ManifestValidationFailed { .. } => 3606,
            Self::DuplicateWorkspaceMember { .. } => 3607,
            Self::DuplicatePackageName { .. } => 3608,
            Self::MissingPackageVersion { .. } => 3609,
            Self::NoRootPackage => 3610,
            Self::PackageEntryModuleMissing { .. } => 3611,
            _ => return None,
        };
        Some(DiagCode::new(code))
    }

    const fn graph_diag_code(&self) -> Option<DiagCode> {
        let code = match self {
            Self::PackageDependencyCycle { .. } => 3614,
            Self::UnresolvedImport { .. } => 3615,
            Self::UnknownPackage { .. } => 3622,
            Self::PackageGraphEntryMissing { .. } => 3623,
            _ => return None,
        };
        Some(DiagCode::new(code))
    }

    const fn task_diag_code(&self) -> Option<DiagCode> {
        let code = match self {
            Self::UnknownTask { .. } => 3612,
            Self::TaskDependencyCycle { .. } => 3613,
            _ => return None,
        };
        Some(DiagCode::new(code))
    }

    const fn registry_diag_code(&self) -> Option<DiagCode> {
        let code = match self {
            Self::MissingRegistryRoot { .. } => 3616,
            Self::MissingCacheRoot { .. } => 3617,
            Self::RegistryVersionNotFound { .. } => 3618,
            Self::InvalidVersionRequirement { .. } => 3619,
            _ => return None,
        };
        Some(DiagCode::new(code))
    }

    const fn lockfile_diag_code(&self) -> Option<DiagCode> {
        let code = match self {
            Self::MissingFrozenLockfile { .. } => 3620,
            Self::FrozenLockfileOutOfDate { .. } => 3621,
            _ => return None,
        };
        Some(DiagCode::new(code))
    }

    const fn builtin_diag_code(&self) -> Option<DiagCode> {
        if let Some(code) = self.manifest_diag_code() {
            return Some(code);
        }
        if let Some(code) = self.graph_diag_code() {
            return Some(code);
        }
        if let Some(code) = self.task_diag_code() {
            return Some(code);
        }
        if let Some(code) = self.registry_diag_code() {
            return Some(code);
        }
        if let Some(code) = self.lockfile_diag_code() {
            return Some(code);
        }
        None
    }

    fn manifest_diag_message(&self) -> Option<Cow<'static, str>> {
        Some(match self {
            Self::MissingManifest { .. } => Cow::Borrowed("manifest not found"),
            Self::MissingManifestAncestor { .. } => Cow::Borrowed("ancestor manifest not found"),
            Self::MissingPackageRoot { .. } => Cow::Borrowed("package root not found"),
            Self::MissingModule { .. } => Cow::Borrowed("module path not found"),
            Self::ManifestJsonInvalid { .. } => Cow::Borrowed("manifest JSON invalid"),
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
            _ => return None,
        })
    }

    fn graph_diag_message(&self) -> Option<Cow<'static, str>> {
        Some(match self {
            Self::PackageDependencyCycle { name } => {
                Cow::Owned(format!("package dependency cycle reaches `{name}`"))
            }
            Self::UnresolvedImport { spec } => Cow::Owned(format!("unresolved import `{spec}`")),
            Self::UnknownPackage { name } => Cow::Owned(format!("unknown package `{name}`")),
            Self::PackageGraphEntryMissing { name } => {
                Cow::Owned(format!("package graph entry missing `{name}`"))
            }
            _ => return None,
        })
    }

    fn task_diag_message(&self) -> Option<Cow<'static, str>> {
        Some(match self {
            Self::UnknownTask { name } => Cow::Owned(format!("task `{name}` not found")),
            Self::TaskDependencyCycle { name } => {
                Cow::Owned(format!("task dependency cycle reaches `{name}`"))
            }
            _ => return None,
        })
    }

    fn registry_diag_message(&self) -> Option<Cow<'static, str>> {
        Some(match self {
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
            _ => return None,
        })
    }

    const fn lockfile_diag_message(&self) -> Option<Cow<'static, str>> {
        Some(match self {
            Self::MissingFrozenLockfile { .. } => Cow::Borrowed("frozen lockfile is required"),
            Self::FrozenLockfileOutOfDate { .. } => Cow::Borrowed("frozen lockfile is out of date"),
            _ => return None,
        })
    }

    fn builtin_diag_message(&self) -> Option<Cow<'static, str>> {
        self.manifest_diag_message()
            .or_else(|| self.graph_diag_message())
            .or_else(|| self.task_diag_message())
            .or_else(|| self.registry_diag_message())
            .or_else(|| self.lockfile_diag_message())
    }

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
    pub fn diag_code(&self) -> Option<DiagCode> {
        match self {
            Self::SourceDiagnostic(diag) => diag.diag().code(),
            _ => self.builtin_diag_code(),
        }
    }

    #[must_use]
    pub fn diag_message(&self) -> Option<Cow<'static, str>> {
        match self {
            Self::SourceDiagnostic(diag) => Some(Cow::Owned(diag.diag().message().to_owned())),
            _ => self.builtin_diag_message(),
        }
    }
}
