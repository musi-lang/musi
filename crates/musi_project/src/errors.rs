use std::borrow::Cow;
use std::io::Error as IoError;
use std::path::PathBuf;

use music_base::diag::DiagCode;
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
    ManifestJson {
        path: PathBuf,
        #[source]
        source: serde_json::Error,
    },
    #[error("io error at `{path}`")]
    Io {
        path: PathBuf,
        #[source]
        source: IoError,
    },
    #[error("manifest validation failed with `{message}`")]
    Validation { message: String },
    #[error("duplicate workspace member `{member}`")]
    DuplicateWorkspaceMember { member: String },
    #[error("duplicate package `{name}` in project graph")]
    DuplicatePackageName { name: String },
    #[error("missing version for package `{name}`")]
    MissingPackageVersion { name: String },
    #[error("no package entry for root package")]
    NoRootPackage,
    #[error("entry module not found for package `{package}`")]
    MissingEntry { package: String },
    #[error("task `{name}` not found")]
    UnknownTask { name: String },
    #[error("task dependency cycle detected at `{name}`")]
    TaskCycle { name: String },
    #[error("dependency cycle detected at `{name}`")]
    DependencyCycle { name: String },
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
    #[error("session error")]
    Session(#[from] SessionError),
}

impl ProjectError {
    #[must_use]
    pub const fn diag_code(&self) -> Option<DiagCode> {
        Some(DiagCode::new(match self {
            Self::MissingManifest { .. } => 3600,
            Self::MissingManifestAncestor { .. } => 3601,
            Self::MissingPackageRoot { .. } => 3602,
            Self::MissingModule { .. } => 3603,
            Self::ManifestJson { .. } => 3604,
            Self::Io { .. } => 3605,
            Self::Validation { .. } => 3606,
            Self::DuplicateWorkspaceMember { .. } => 3607,
            Self::DuplicatePackageName { .. } => 3608,
            Self::MissingPackageVersion { .. } => 3609,
            Self::NoRootPackage => 3610,
            Self::MissingEntry { .. } => 3611,
            Self::UnknownTask { .. } => 3612,
            Self::TaskCycle { .. } => 3613,
            Self::DependencyCycle { .. } => 3614,
            Self::UnresolvedDependency { .. } => 3615,
            Self::MissingRegistryRoot { .. } => 3616,
            Self::MissingCacheRoot { .. } => 3617,
            Self::RegistryVersionNotFound { .. } => 3618,
            Self::InvalidVersionRequirement { .. } => 3619,
            Self::MissingFrozenLockfile { .. } => 3620,
            Self::FrozenLockfileOutOfDate { .. } => 3621,
            Self::Session(_) => return None,
        }))
    }

    #[must_use]
    pub fn diag_message(&self) -> Option<Cow<'static, str>> {
        Some(match self {
            Self::MissingManifest { .. } => Cow::Borrowed("manifest not found"),
            Self::MissingManifestAncestor { .. } => Cow::Borrowed("ancestor manifest not found"),
            Self::MissingPackageRoot { .. } => Cow::Borrowed("package root not found"),
            Self::MissingModule { .. } => Cow::Borrowed("module path not found"),
            Self::ManifestJson { .. } => Cow::Borrowed("manifest json is invalid"),
            Self::Io { .. } => Cow::Borrowed("project io failed"),
            Self::Validation { message } => {
                Cow::Owned(format!("manifest validation failed: `{message}`"))
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
            Self::MissingEntry { package } => {
                Cow::Owned(format!("package `{package}` entry module not found"))
            }
            Self::UnknownTask { name } => Cow::Owned(format!("task `{name}` not found")),
            Self::TaskCycle { name } => Cow::Owned(format!("task cycle detected at `{name}`")),
            Self::DependencyCycle { name } => {
                Cow::Owned(format!("dependency cycle detected at `{name}`"))
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
            Self::Session(_) => return None,
        })
    }
}
