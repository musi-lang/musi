mod errors;
mod intrinsics;
mod lock;
pub mod manifest;
mod project;
mod registry;
mod testing;
mod types;

pub use errors::ProjectError;
pub use lock::{LockedPackage, LockedPackageSource, Lockfile};
pub use manifest::PackageManifest;
pub use project::{
    PackageId, PackageSource, Project, ProjectEntry, ProjectOptions, ResolvedPackage, TaskSpec,
    WorkspaceGraph, load_project,
};
pub use testing::PackageTestModule;
pub use types::ProjectResult;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
