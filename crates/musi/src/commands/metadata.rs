use std::path::Path;

use crate::error::MusiResult;
use musi_project::{ProjectOptions, load_project_ancestor};

use super::project_target::{project_anchor, resolve_project_entry};

pub(super) fn print_project_metadata(target: Option<&Path>) -> MusiResult {
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    let entry = resolve_project_entry(&project, target)?;
    let workspace = project.workspace();
    println!("package: {}", entry.package.name);
    println!("manifest: {}", project.root_manifest_path().display());
    println!("entry: {}", entry.path.display());
    println!("module: {}", entry.module_key.as_str());
    println!("workspacePackages: {}", workspace.packages.len());
    println!("workspaceMembers: {}", workspace.members.len());
    println!("modules: {}", project.module_texts().count());
    match project.modules_dir() {
        Some(path) => println!("musiModules: {}", path.display()),
        None => println!("musiModules: disabled"),
    }
    println!("globalCache: {}", project.global_cache_dir().display());
    println!("lockfile: {}", project.lockfile_path().display());
    Ok(())
}
