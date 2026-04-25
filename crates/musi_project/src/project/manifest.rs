use std::collections::BTreeSet;
use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};

use music_base::diag::DiagContext;

use crate::errors::ProjectError;
use crate::manifest::PackageManifest;
use crate::manifest_source::ManifestSource;
use crate::project::model::{LoadedManifest, TaskNameSet};
use crate::project::module_graph::normalize_lookup_path;
use crate::{ProjectDiagKind, ProjectResult};

use crate::manifest::{MusiModulesDir, PublishConfig};

pub(super) fn manifest_path_for(path: &Path) -> ProjectResult<PathBuf> {
    let manifest_path = if path.file_name() == Some(OsStr::new("musi.json")) {
        path.to_path_buf()
    } else {
        path.join("musi.json")
    };
    if manifest_path.is_file() {
        Ok(normalize_lookup_path(&manifest_path))
    } else {
        Err(ProjectError::MissingManifest {
            path: manifest_path,
        })
    }
}

pub(super) fn manifest_ancestor_path_for(path: &Path) -> ProjectResult<PathBuf> {
    let start_dir = if path.file_name() == Some(OsStr::new("musi.json")) {
        path.parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| ProjectError::MissingPackageRoot {
                path: path.to_path_buf(),
            })?
    } else if path.is_dir() {
        path.to_path_buf()
    } else {
        path.parent()
            .map(Path::to_path_buf)
            .ok_or_else(|| ProjectError::MissingPackageRoot {
                path: path.to_path_buf(),
            })?
    };

    for ancestor in start_dir.ancestors() {
        let manifest_path = ancestor.join("musi.json");
        if manifest_path.is_file() {
            return Ok(normalize_lookup_path(&manifest_path));
        }
    }

    Err(ProjectError::MissingManifestAncestor {
        path: path.to_path_buf(),
    })
}

pub(super) fn read_manifest(path: &Path) -> ProjectResult<LoadedManifest> {
    let text = fs::read_to_string(path).map_err(|source| ProjectError::ProjectIoFailed {
        path: path.to_path_buf(),
        source,
    })?;
    let source = ManifestSource::from_text(path.to_path_buf(), text.clone());
    let manifest = serde_json::from_str(&text).map_err(|error| source.parse_error(&error))?;
    Ok(LoadedManifest { manifest, source })
}

pub(super) fn validate_manifest(
    manifest: &PackageManifest,
    source: &ManifestSource,
) -> ProjectResult {
    if let Some(name) = &manifest.name {
        if name.trim().is_empty() {
            let span = source
                .value_span(&json_pointer(&["name"]))
                .unwrap_or_else(|| source.insertion_span());
            return Err(source.catalog_error(
                ProjectDiagKind::ManifestPackageNameEmpty,
                DiagContext::new().with("path", source.path().display()),
                span,
            ));
        }
    }
    for (index, lib) in manifest.enabled_libs().into_iter().enumerate() {
        if lib != "std" {
            let pointer = format!("/lib/{index}");
            let span = source
                .value_span(&pointer)
                .or_else(|| source.value_span(&json_pointer(&["lib"])))
                .unwrap_or_else(|| source.insertion_span());
            return Err(source.catalog_error(
                ProjectDiagKind::ManifestLibUnknown,
                DiagContext::new().with("lib", lib),
                span,
            ));
        }
    }
    if matches!(manifest.publish, Some(PublishConfig::Disabled(true))) {
        let span = source
            .value_span(&json_pointer(&["publish"]))
            .unwrap_or_else(|| source.insertion_span());
        return Err(source.catalog_error(
            ProjectDiagKind::ManifestPublishUnsupported,
            DiagContext::new().with("value", true),
            span,
        ));
    }
    if matches!(
        manifest.musi_modules_dir,
        Some(MusiModulesDir::Disabled(true))
    ) {
        let span = source
            .value_span(&json_pointer(&["musiModulesDir"]))
            .unwrap_or_else(|| source.insertion_span());
        return Err(source.catalog_error(
            ProjectDiagKind::ManifestModulesDirUnsupported,
            DiagContext::new().with("value", true),
            span,
        ));
    }
    validate_fmt_config(manifest, source)?;
    for (export_name, export_path) in manifest.export_map() {
        if export_name != "." && !export_name.starts_with("./") {
            let pointer = format!("/exports/{}", escape_pointer_segment(&export_name));
            let span = source
                .key_span(&pointer)
                .or_else(|| source.value_span(&json_pointer(&["exports"])))
                .unwrap_or_else(|| source.insertion_span());
            return Err(source.catalog_error(
                ProjectDiagKind::ManifestExportKeyInvalid,
                DiagContext::new().with("key", &export_name),
                span,
            ));
        }
        if !export_path.starts_with("./") {
            let pointer = format!("/exports/{}", escape_pointer_segment(&export_name));
            let span = source
                .value_span(&pointer)
                .or_else(|| source.value_span(&json_pointer(&["exports"])))
                .unwrap_or_else(|| source.insertion_span());
            return Err(source.catalog_error(
                ProjectDiagKind::ManifestExportTargetInvalid,
                DiagContext::new().with("target", &export_path),
                span,
            ));
        }
    }
    validate_task_graph(manifest, source)?;
    Ok(())
}

fn validate_fmt_config(manifest: &PackageManifest, source: &ManifestSource) -> ProjectResult {
    let Some(config) = manifest.fmt.as_ref() else {
        return Ok(());
    };
    if matches!(config.line_width, Some(0)) {
        let span = source
            .value_span(&json_pointer(&["fmt", "lineWidth"]))
            .unwrap_or_else(|| source.insertion_span());
        return Err(source.catalog_error(
            ProjectDiagKind::ManifestFmtLineWidthInvalid,
            DiagContext::new().with("value", 0),
            span,
        ));
    }
    if matches!(config.indent_width, Some(0)) {
        let span = source
            .value_span(&json_pointer(&["fmt", "indentWidth"]))
            .unwrap_or_else(|| source.insertion_span());
        return Err(source.catalog_error(
            ProjectDiagKind::ManifestFmtIndentWidthInvalid,
            DiagContext::new().with("value", 0),
            span,
        ));
    }
    if let Some(pattern) = first_duplicate(&config.include) {
        let span = source
            .value_span(&json_pointer(&["fmt", "include"]))
            .unwrap_or_else(|| source.insertion_span());
        return Err(source.catalog_error(
            ProjectDiagKind::ManifestFmtIncludeDuplicate,
            DiagContext::new().with("pattern", pattern),
            span,
        ));
    }
    if let Some(pattern) = first_duplicate(&config.exclude) {
        let span = source
            .value_span(&json_pointer(&["fmt", "exclude"]))
            .unwrap_or_else(|| source.insertion_span());
        return Err(source.catalog_error(
            ProjectDiagKind::ManifestFmtExcludeDuplicate,
            DiagContext::new().with("pattern", pattern),
            span,
        ));
    }
    Ok(())
}

fn first_duplicate(items: &[String]) -> Option<&str> {
    let mut seen = BTreeSet::new();
    items
        .iter()
        .find(|item| !seen.insert(item.as_str()))
        .map(String::as_str)
}

fn validate_task_graph(manifest: &PackageManifest, source: &ManifestSource) -> ProjectResult {
    let mut seen = BTreeSet::new();
    let mut active = BTreeSet::new();
    for name in manifest.tasks.keys() {
        validate_task_node(name, manifest, source, &mut seen, &mut active)?;
    }
    Ok(())
}

fn validate_task_node(
    name: &str,
    manifest: &PackageManifest,
    source: &ManifestSource,
    seen: &mut TaskNameSet,
    active: &mut TaskNameSet,
) -> ProjectResult {
    if !seen.insert(name.into()) {
        return Ok(());
    }
    if !active.insert(name.into()) {
        return Err(ProjectError::TaskDependencyCycle { name: name.into() });
    }
    if let Some(task) = manifest.task_config(name) {
        for (index, dependency) in task.dependencies.into_iter().enumerate() {
            if manifest.task_config(&dependency).is_none() {
                let pointer = format!(
                    "/tasks/{}/dependencies/{}",
                    escape_pointer_segment(name),
                    index
                );
                let span = source
                    .value_span(&pointer)
                    .unwrap_or_else(|| source.insertion_span());
                return Err(source.catalog_error(
                    ProjectDiagKind::ManifestTaskDependencyUnknown,
                    DiagContext::new()
                        .with("task", name)
                        .with("dependency", &dependency),
                    span,
                ));
            }
            validate_task_node(&dependency, manifest, source, seen, active)?;
        }
    }
    let _ = active.remove(name);
    Ok(())
}

fn json_pointer(segments: &[&str]) -> String {
    let mut pointer = String::new();
    for segment in segments {
        pointer.push('/');
        pointer.push_str(segment);
    }
    pointer
}

fn escape_pointer_segment(segment: &str) -> String {
    segment.replace('~', "~0").replace('/', "~1")
}
