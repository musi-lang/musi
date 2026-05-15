use std::fs;
use std::path::{Component, Path, PathBuf};

use async_lsp::lsp_types::InitializeParams;
use musi_project::{PackageSource, ProjectOptions, load_project, load_project_ancestor};

use super::MusiLanguageServer;

#[allow(deprecated)]
pub(super) fn workspace_roots(params: &InitializeParams) -> Vec<PathBuf> {
    let mut roots = Vec::new();
    if let Some(folders) = &params.workspace_folders {
        roots.extend(
            folders
                .iter()
                .filter_map(|folder| folder.uri.to_file_path().ok()),
        );
    }
    if roots.is_empty()
        && let Some(root_uri) = &params.root_uri
        && let Ok(path) = root_uri.to_file_path()
    {
        roots.push(path);
    }
    roots
}

pub(super) fn workspace_module_paths(root: &Path) -> Vec<PathBuf> {
    let Ok(project) = load_project(root, ProjectOptions::default()) else {
        return Vec::new();
    };
    sort_dedup_paths(
        project
            .workspace()
            .packages
            .values()
            .filter(|package| matches!(package.source, PackageSource::Workspace))
            .flat_map(|package| package.module_keys.values().cloned())
            .collect(),
    )
}

impl MusiLanguageServer {
    pub(super) fn workspace_query_roots(&self) -> Vec<PathBuf> {
        let mut roots = self.workspace_roots.clone();
        roots.extend(inferred_workspace_roots(
            self.open_documents
                .keys()
                .filter_map(|uri| uri.to_file_path().ok()),
        ));
        sort_dedup_paths(roots)
    }
}

fn inferred_workspace_roots(paths: impl IntoIterator<Item = PathBuf>) -> Vec<PathBuf> {
    sort_dedup_paths(
        paths
            .into_iter()
            .filter_map(|path| {
                load_project_ancestor(&path, ProjectOptions::default())
                    .ok()
                    .map(|project| project.root_dir().to_path_buf())
            })
            .collect(),
    )
}

pub(super) fn collect_workspace_source_paths(root: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(root) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.components().any(is_ignored_workspace_component) {
            continue;
        }
        if path.is_dir() {
            collect_workspace_source_paths(&path, out);
        } else if path.extension().and_then(|extension| extension.to_str()) == Some("ms") {
            out.push(path);
        }
    }
}

pub(super) fn sort_dedup_paths(mut paths: Vec<PathBuf>) -> Vec<PathBuf> {
    paths.sort_by_key(|path| canonical_path(path));
    paths.dedup_by(|left, right| paths_match(left, right));
    paths
}

pub(super) fn paths_match(left: &Path, right: &Path) -> bool {
    canonical_path(left) == canonical_path(right)
}

pub(super) fn renamed_target_path(
    renames: &[(PathBuf, PathBuf)],
    target: &Path,
) -> Option<PathBuf> {
    renames.iter().find_map(|(old_path, new_path)| {
        let target_key = canonical_path(target);
        let old_key = canonical_path(old_path);
        if target_key == old_key {
            return Some(new_path.clone());
        }
        target_key
            .strip_prefix(old_key)
            .ok()
            .map(|relative| new_path.join(relative))
    })
}

pub(super) fn import_specifier_for_target(
    importer_path: &Path,
    target_path: &Path,
) -> Option<String> {
    let importer_dir = canonical_path(importer_path.parent()?);
    let target_path = canonical_target_path(target_path);
    let relative = relative_path(&importer_dir, &target_path)?;
    let relative = strip_musi_extension(relative);
    let mut specifier = relative.to_string_lossy().replace('\\', "/");
    if !specifier.starts_with('.') {
        specifier = format!("./{specifier}");
    }
    Some(specifier)
}

fn canonical_path(path: &Path) -> PathBuf {
    path.canonicalize().unwrap_or_else(|_| path.to_path_buf())
}

fn is_ignored_workspace_component(component: Component<'_>) -> bool {
    let text = component.as_os_str().to_string_lossy();
    matches!(
        text.as_ref(),
        ".git" | ".cache" | ".musi" | "musi_modules" | "node_modules" | "target"
    )
}

fn canonical_target_path(path: &Path) -> PathBuf {
    if let Ok(path) = path.canonicalize() {
        return path;
    }
    let mut missing = Vec::new();
    let mut current = path;
    while let Some(parent) = current.parent() {
        if let Some(file_name) = current.file_name() {
            missing.push(file_name.to_owned());
        }
        if let Ok(mut base) = parent.canonicalize() {
            for component in missing.iter().rev() {
                base.push(component);
            }
            return base;
        }
        current = parent;
    }
    path.to_path_buf()
}

fn relative_path(from_dir: &Path, target_path: &Path) -> Option<PathBuf> {
    let from_components = normal_components(from_dir);
    let target_components = normal_components(target_path);
    if from_components.first() != target_components.first() {
        return None;
    }
    let mut common = 0usize;
    while from_components.get(common) == target_components.get(common)
        && common < from_components.len()
        && common < target_components.len()
    {
        common = common.saturating_add(1);
    }
    let mut relative = PathBuf::new();
    for _ in common..from_components.len() {
        relative.push("..");
    }
    for component in &target_components[common..] {
        relative.push(component);
    }
    Some(relative)
}

fn normal_components(path: &Path) -> Vec<String> {
    path.components()
        .filter_map(|component| match component {
            Component::CurDir => None,
            Component::Normal(part) => Some(part.to_string_lossy().into_owned()),
            Component::ParentDir | Component::RootDir | Component::Prefix(_) => {
                Some(component.as_os_str().to_string_lossy().into_owned())
            }
        })
        .collect()
}

fn strip_musi_extension(mut path: PathBuf) -> PathBuf {
    if path.extension().and_then(|extension| extension.to_str()) == Some("ms") {
        let _ = path.set_extension("");
    }
    path
}
