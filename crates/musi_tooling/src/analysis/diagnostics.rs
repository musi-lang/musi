use std::path::Path;

use musi_project::{ProjectOptions, load_project_ancestor};

use crate::{
    CliDiagnostic,
    analysis_support::{
        collect_direct_file_diagnostics, collect_foundation_diagnostics,
        collect_loaded_project_diagnostics,
    },
};

#[must_use]
pub fn collect_project_diagnostics(path: &Path) -> Vec<CliDiagnostic> {
    collect_project_diagnostics_with_overlay(path, None)
}

#[must_use]
pub fn collect_project_diagnostics_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> Vec<CliDiagnostic> {
    if let Some(diagnostics) = collect_foundation_diagnostics(path, overlay_text) {
        return diagnostics;
    }
    if let Ok(project) = load_project_ancestor(path, ProjectOptions::default())
        && let Some(module_key) = project.module_key_for_path(path)
    {
        return collect_loaded_project_diagnostics(&project, &module_key, overlay_text);
    }
    collect_direct_file_diagnostics(path, overlay_text)
}
