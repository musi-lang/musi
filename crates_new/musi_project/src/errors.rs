use std::io::Error as IoError;
use std::path::PathBuf;

use music_session::SessionError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ProjectError {
    #[error("manifest not found at `{path}`")]
    MissingManifest { path: PathBuf },
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
    #[error("manifest validation failed: {message}")]
    Validation { message: String },
    #[error("workspace member `{member}` is duplicated")]
    DuplicateWorkspaceMember { member: String },
    #[error("package `{name}` is duplicated in the project graph")]
    DuplicatePackageName { name: String },
    #[error("package `{name}` is missing a version")]
    MissingPackageVersion { name: String },
    #[error("root project has no package entry")]
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
