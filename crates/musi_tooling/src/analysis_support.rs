use std::collections::BTreeMap;
use std::path::Path;

use musi_project::{Project, ProjectOptions, ProjectResult, load_project_ancestor};
use music_module::ModuleKey;
use music_session::{Session, SessionOptions};

use crate::{
    CliDiagnostic, ToolingError, load_direct_graph, project_error_report, session_error_report,
    tooling_error_report,
};

pub fn build_overlay_session(
    project: &Project,
    module_key: &ModuleKey,
    overlay_text: Option<&str>,
) -> ProjectResult<Session> {
    let mut session = project.build_session()?;
    if let Some(text) = overlay_text {
        session.set_module_text(module_key, text.to_owned())?;
    }
    Ok(session)
}

pub fn analysis_session(path: &Path, overlay_text: Option<&str>) -> Option<(Session, ModuleKey)> {
    let (mut session, module_key) = analysis_session_unchecked(path, overlay_text)?;
    drop(session.check_module(&module_key));
    Some((session, module_key))
}

pub fn analysis_session_unchecked(
    path: &Path,
    overlay_text: Option<&str>,
) -> Option<(Session, ModuleKey)> {
    if let Ok(project) = load_project_ancestor(path, ProjectOptions::default())
        && let Some(module_key) = project.module_key_for_path(path)
    {
        let session = build_overlay_session(&project, &module_key, overlay_text).ok()?;
        return Some((session, module_key));
    }
    let graph = load_direct_graph(path).ok()?;
    let module_key = graph.entry_key().clone();
    let mut session = graph.build_session(SessionOptions::default()).ok()?;
    if let Some(text) = overlay_text {
        session.set_module_text(&module_key, text.to_owned()).ok()?;
    }
    Some((session, module_key))
}

pub fn collect_loaded_project_diagnostics(
    project: &Project,
    module_key: &ModuleKey,
    overlay_text: Option<&str>,
) -> Vec<CliDiagnostic> {
    let mut session = match build_overlay_session(project, module_key, overlay_text) {
        Ok(session) => session,
        Err(error) => {
            return project_error_report("musi_lsp", "diagnostics", None, None, &error).diagnostics;
        }
    };
    match session.check_module(module_key) {
        Ok(_) => Vec::new(),
        Err(error) => {
            let mut diagnostics =
                session_error_report("musi_lsp", "diagnostics", None, None, &session, &error)
                    .diagnostics;
            remap_project_diagnostic_paths(&mut diagnostics, project);
            diagnostics
        }
    }
}

pub fn collect_direct_file_diagnostics(
    path: &Path,
    overlay_text: Option<&str>,
) -> Vec<CliDiagnostic> {
    let graph = match load_direct_graph(path) {
        Ok(graph) => graph,
        Err(error) => {
            return tooling_error_report("musi_lsp", "diagnostics", None, None, &error).diagnostics;
        }
    };
    let module_key = graph.entry_key().clone();
    let mut session = match graph.build_session(SessionOptions::default()) {
        Ok(session) => session,
        Err(error) => {
            return tooling_error_report("musi_lsp", "diagnostics", None, None, &error).diagnostics;
        }
    };
    if let Some(text) = overlay_text
        && let Err(error) = session.set_module_text(&module_key, text.to_owned())
    {
        let error = ToolingError::from(error);
        return tooling_error_report("musi_lsp", "diagnostics", None, None, &error).diagnostics;
    }
    match session.check_module(&module_key) {
        Ok(_) => Vec::new(),
        Err(error) => {
            session_error_report("musi_lsp", "diagnostics", None, None, &session, &error)
                .diagnostics
        }
    }
}

fn remap_project_diagnostic_paths(diagnostics: &mut [CliDiagnostic], project: &Project) {
    let path_map = project_module_path_map(project);
    for diagnostic in diagnostics {
        diagnostic.file = diagnostic
            .file
            .as_deref()
            .map(|file| remap_project_path(file, &path_map));
        for label in &mut diagnostic.labels {
            label.file = label
                .file
                .as_deref()
                .map(|file| remap_project_path(file, &path_map));
        }
    }
}

fn project_module_path_map(project: &Project) -> BTreeMap<String, String> {
    project
        .workspace()
        .packages
        .values()
        .flat_map(|package| &package.module_keys)
        .map(|(key, path)| (key.as_str().to_owned(), path.display().to_string()))
        .collect()
}

fn remap_project_path(file: &str, path_map: &BTreeMap<String, String>) -> String {
    if let Some(path) = path_map.get(file) {
        return path.clone();
    }
    if let Some(module_key) = file.strip_suffix("#expanded")
        && let Some(path) = path_map.get(module_key)
    {
        return path.clone();
    }
    file.to_owned()
}
