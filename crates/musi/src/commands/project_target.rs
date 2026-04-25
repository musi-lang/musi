use std::env::current_dir;
use std::path::{Path, PathBuf};

use crate::error::{MusiError, MusiResult};
use crate::workspace::{selected_package_entries, uses_workspace_scope};
use musi_project::{Project, ProjectEntry};

pub(super) fn project_anchor(target: Option<&Path>) -> MusiResult<PathBuf> {
    let current = current_dir().map_err(|_| MusiError::MissingCurrentDirectory)?;
    Ok(target.map_or_else(
        || current.clone(),
        |path| {
            if path.is_absolute() {
                path.to_path_buf()
            } else {
                current.join(path)
            }
        },
    ))
}

pub(super) fn resolve_project_entry(
    project: &Project,
    target: Option<&Path>,
) -> MusiResult<ProjectEntry> {
    let Some(target) = target else {
        return project.root_entry().cloned().map_err(Into::into);
    };
    if target.file_name().is_some_and(|name| name == "musi.json") || target.is_dir() {
        return project.root_entry().cloned().map_err(Into::into);
    }
    if target.extension().is_some_and(|ext| ext == "ms") || target.components().count() > 1 {
        return resolve_project_path_entry(project, target);
    }
    let package_name = target.to_string_lossy();
    if let Ok(entry) = project.package_entry(package_name.as_ref()) {
        return Ok(entry.clone());
    }
    resolve_project_path_entry(project, target)
}

pub(super) fn resolve_project_entries(
    project: &Project,
    target: Option<&Path>,
    workspace: u8,
) -> MusiResult<Vec<ProjectEntry>> {
    reject_workspace_target(workspace, target)?;
    if uses_workspace_scope(project, target, workspace) {
        return Ok(selected_package_entries(project));
    }
    Ok(vec![resolve_project_entry(project, target)?])
}

pub(super) fn reject_workspace_target(workspace: u8, target: Option<&Path>) -> MusiResult {
    if workspace > 0 && target.is_some() {
        return Err(MusiError::IncompatibleCommandArgs {
            left: "--workspace".to_owned(),
            right: "target".to_owned(),
        });
    }
    Ok(())
}

pub(super) fn resolve_project_path_entry(
    project: &Project,
    target: &Path,
) -> MusiResult<ProjectEntry> {
    let resolved = if target.is_absolute() {
        target.to_path_buf()
    } else {
        project.root_dir().join(target)
    };
    let resolved = resolved
        .canonicalize()
        .map_err(|_| MusiError::UnknownTarget {
            target: resolved.clone(),
        })?;
    let matched_entry = project.workspace().packages.values().find_map(|package| {
        package.module_keys.iter().find_map(|(module_key, path)| {
            (path == &resolved).then_some((package, module_key, path))
        })
    });
    if let Some((package, module_key, path)) = matched_entry {
        return Ok(ProjectEntry::new(
            package.id.clone(),
            module_key.clone(),
            path.clone(),
        ));
    }
    Err(MusiError::UnknownTarget { target: resolved })
}

pub(super) fn normalized_project_target(
    project: &Project,
    target: Option<&Path>,
) -> MusiResult<Option<PathBuf>> {
    let Some(target) = target else {
        return Ok(None);
    };
    if !target.exists() && target.components().count() == 1 && target.extension().is_none() {
        return Ok(Some(target.to_path_buf()));
    }
    let resolved = if target.is_absolute() {
        target.to_path_buf()
    } else {
        current_dir()
            .map_err(|_| MusiError::MissingCurrentDirectory)?
            .join(target)
    };
    let resolved = resolved
        .canonicalize()
        .map_err(|_| MusiError::UnknownTarget {
            target: resolved.clone(),
        })?;
    if resolved == project.root_dir() || resolved == project.root_manifest_path() {
        return Ok(None);
    }
    Ok(Some(
        resolved
            .strip_prefix(project.root_dir())
            .map_or_else(|_| resolved.clone(), Path::to_path_buf),
    ))
}
pub(super) fn manifest_output_path(project: &Project, entry: &ProjectEntry) -> Option<PathBuf> {
    project
        .workspace()
        .packages
        .get(&entry.package)
        .and_then(|package| {
            package
                .manifest
                .compile
                .as_ref()
                .and_then(|compile| compile.output.as_deref())
                .map(|output| package.root_dir.join(output))
        })
}
