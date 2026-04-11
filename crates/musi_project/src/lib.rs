mod errors;
mod lock;
pub mod manifest;
mod manifest_source;
mod project;
mod registry;
mod testing;
mod types;

pub use errors::{ProjectError, ProjectSourceDiagnostic, ProjectSourceLabel};
pub use lock::{LockedPackage, LockedPackageSource, Lockfile};
pub use manifest::PackageManifest;
pub use project::{
    PackageId, PackageSource, Project, ProjectEntry, ProjectOptions, ResolvedPackage, TaskSpec,
    WorkspaceGraph, load_project, load_project_ancestor,
};
pub use testing::PackageTestModule;
pub use types::ProjectResult;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
