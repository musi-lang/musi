use std::path::{Path, PathBuf};

use musi_project::{PackageId, Project, ProjectEntry};

pub fn selected_package_entries(project: &Project) -> Vec<ProjectEntry> {
    selected_package_ids(project)
        .into_iter()
        .filter_map(|id| project.workspace().packages.get(&id))
        .map(|package| package.entry.clone())
        .collect()
}

pub fn selected_package_roots(project: &Project) -> Vec<PathBuf> {
    selected_package_ids(project)
        .into_iter()
        .filter_map(|id| project.workspace().packages.get(&id))
        .map(|package| package.root_dir.clone())
        .collect()
}

pub const fn uses_workspace_scope(project: &Project, target: Option<&Path>, workspace: u8) -> bool {
    workspace > 0 || (target.is_none() && project.manifest().workspace.is_some())
}

fn selected_package_ids(project: &Project) -> Vec<PackageId> {
    let mut ids = Vec::new();
    if let Some(root) = &project.workspace().root_package {
        ids.push(root.clone());
    }
    ids.extend(project.workspace().members.iter().cloned());
    ids.sort();
    ids.dedup();
    ids
}
