mod errors;
mod lock;
mod manifest;
mod project;
mod registry;

pub use errors::ProjectError;
pub use lock::{LockedPackage, LockedPackageSource, Lockfile};
pub use manifest::{
    Author, AuthorObject, BenchConfig, Bugs, BugsObject, CompileConfig, CompilerOptions, Exports,
    FmtConfig, LintConfig, LintRules, LockConfig, LockConfigObject, PackageManifest, PublishConfig,
    PublishSettings, Repository, RepositoryObject, TaskConfig, TaskDefinition,
    TaskDefinitionObject, TestConfig, WorkspaceConfig, WorkspaceMembersObject,
};
pub use project::{
    PackageId, PackageSource, Project, ProjectEntry, ProjectOptions, ResolvedPackage, TaskSpec,
    WorkspaceGraph, load_project,
};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
