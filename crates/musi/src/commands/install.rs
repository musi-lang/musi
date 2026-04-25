use std::path::Path;

use crate::error::MusiResult;
use musi_project::{ProjectOptions, load_project_ancestor};

use super::project_target::project_anchor;

pub(super) fn install_project(target: Option<&Path>) -> MusiResult {
    let anchor = project_anchor(target)?;
    let project = load_project_ancestor(anchor, ProjectOptions::default())?;
    if project.lockfile_needs_write() {
        project.write_lockfile()?;
    }
    match project.modules_dir() {
        Some(path) => println!("musiModules: {}", path.display()),
        None => println!("musiModules: disabled"),
    }
    println!("globalCache: {}", project.global_cache_dir().display());
    println!("lockfile: {}", project.lockfile_path().display());
    Ok(())
}
