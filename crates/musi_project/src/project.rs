use std::path::Path;

use crate::ProjectResult;
use crate::errors::ProjectError;

mod access;
mod dependencies;
mod git;
mod manifest;
mod model;
mod module_graph;
mod session;
mod storage;
mod tasks;
mod workspace;

use manifest::{manifest_ancestor_path_for, manifest_path_for, read_manifest, validate_manifest};
use model::{LoadedManifest, ResolvedWorkspaceState};
pub use model::{
    PackageId, PackageSource, Project, ProjectEntry, ProjectOptions, ResolvedPackage, TaskSpec,
    WorkspaceGraph,
};
use storage::ProjectStorage;
use workspace::{load_lockfile, resolve_workspace_state};

/// # Errors
///
/// Returns [`ProjectError`] when the project manifest, workspace, or dependency graph cannot be
/// loaded from `path`.
pub fn load_project(path: impl AsRef<Path>, options: ProjectOptions) -> ProjectResult<Project> {
    Project::load(path, options)
}

/// # Errors
///
/// Returns [`ProjectError`] when no ancestor `musi.json` can be found from `path`, or the
/// discovered project cannot be loaded.
pub fn load_project_ancestor(
    path: impl AsRef<Path>,
    options: ProjectOptions,
) -> ProjectResult<Project> {
    Project::load_ancestor(path, options)
}

impl Project {
    /// # Errors
    ///
    /// Returns [`ProjectError`] when the manifest, workspace, dependency graph, or registry/cache
    /// state cannot be loaded into a project model.
    pub fn load(path: impl AsRef<Path>, options: ProjectOptions) -> ProjectResult<Self> {
        let root_manifest_path = manifest_path_for(path.as_ref())?;
        let root_dir = root_manifest_path
            .parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| ProjectError::MissingPackageRoot {
                path: root_manifest_path.clone(),
            })?;
        let LoadedManifest {
            manifest,
            source: root_manifest_source,
        } = read_manifest(&root_manifest_path)?;
        validate_manifest(&manifest, &root_manifest_source)?;
        let storage = ProjectStorage::new(&root_dir, &manifest, &options)?;

        let lockfile_path = root_dir.join(manifest.lock_path());
        if manifest.is_lock_frozen() && !lockfile_path.exists() {
            return Err(ProjectError::MissingFrozenLockfile {
                path: lockfile_path,
            });
        }
        let loaded_lockfile = load_lockfile(&lockfile_path)?;
        let ResolvedWorkspaceState {
            workspace,
            package_name_index,
            import_map,
            module_texts,
            resolved_lockfile,
        } = resolve_workspace_state(
            &root_dir,
            &root_manifest_path,
            &root_manifest_source,
            &manifest,
            &options,
            &storage,
            &loaded_lockfile,
        )?;
        if manifest.is_lock_frozen()
            && loaded_lockfile.clone().normalized() != resolved_lockfile.clone().normalized()
        {
            return Err(ProjectError::FrozenLockfileOutOfDate {
                path: lockfile_path,
            });
        }

        Ok(Self {
            options,
            root_dir,
            root_manifest_path,
            root_manifest_source,
            manifest,
            workspace,
            import_map,
            module_texts,
            package_name_index,
            lockfile_path,
            global_cache_dir: storage.global_cache_dir,
            modules_dir: storage.modules_dir,
            loaded_lockfile,
            resolved_lockfile,
        })
    }

    /// # Errors
    ///
    /// Returns [`ProjectError`] when no ancestor `musi.json` can be found from `path`, or the
    /// discovered project cannot be loaded.
    pub fn load_ancestor(path: impl AsRef<Path>, options: ProjectOptions) -> ProjectResult<Self> {
        let manifest_path = manifest_ancestor_path_for(path.as_ref())?;
        Self::load(manifest_path, options)
    }
}
