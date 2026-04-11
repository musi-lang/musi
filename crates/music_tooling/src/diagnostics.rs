use std::path::Path;

use musi_project::ProjectError;
use music_base::diag::{emit, supports_color};
use music_base::{Diag, SourceMap};
use music_base::{DiagLabel, Source};
use music_session::{Session, SessionError};
use serde::Serialize;

use crate::{ToolingError, ToolingResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticsFormat {
    Text,
    Json,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct CliDiagnosticsReport {
    pub schema: &'static str,
    pub tool: String,
    pub command: String,
    pub status: &'static str,
    pub package_root: Option<String>,
    pub manifest: Option<String>,
    pub diagnostics: Vec<CliDiagnostic>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct CliDiagnostic {
    pub phase: &'static str,
    pub severity: &'static str,
    pub code: Option<String>,
    pub message: String,
    pub file: Option<String>,
    pub range: Option<CliDiagnosticRange>,
    pub labels: Vec<CliDiagnosticLabel>,
    pub notes: Vec<String>,
    pub hint: Option<String>,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct CliDiagnosticLabel {
    pub file: Option<String>,
    pub range: Option<CliDiagnosticRange>,
    pub message: String,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
pub struct CliDiagnosticRange {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

pub fn session_error_report(
    tool: &str,
    command: &str,
    package_root: Option<&Path>,
    manifest: Option<&Path>,
    session: &Session,
    error: &SessionError,
) -> CliDiagnosticsReport {
    let diags = session_error_diags(error);
    CliDiagnosticsReport {
        schema: "musi.diagnostics.v1",
        tool: tool.to_owned(),
        command: command.to_owned(),
        status: "error",
        package_root: package_root.map(path_string),
        manifest: manifest.map(path_string),
        diagnostics: diags
            .iter()
            .map(|diag| cli_diag(session.source_map(), session_phase(error), diag))
            .collect(),
    }
}

pub fn project_error_report(
    tool: &str,
    command: &str,
    package_root: Option<&Path>,
    manifest: Option<&Path>,
    error: &ProjectError,
) -> CliDiagnosticsReport {
    CliDiagnosticsReport {
        schema: "musi.diagnostics.v1",
        tool: tool.to_owned(),
        command: command.to_owned(),
        status: "error",
        package_root: package_root.map(path_string),
        manifest: manifest.map(path_string),
        diagnostics: vec![project_diag(error)],
    }
}

pub fn tooling_error_report(
    tool: &str,
    command: &str,
    package_root: Option<&Path>,
    manifest: Option<&Path>,
    error: &ToolingError,
) -> CliDiagnosticsReport {
    CliDiagnosticsReport {
        schema: "musi.diagnostics.v1",
        tool: tool.to_owned(),
        command: command.to_owned(),
        status: "error",
        package_root: package_root.map(path_string),
        manifest: manifest.map(path_string),
        diagnostics: vec![tooling_diag(error)],
    }
}

pub fn render_session_error(session: &Session, error: &SessionError) -> ToolingResult<String> {
    let mut output = Vec::new();
    let diags = session_error_diags(error);
    if diags.is_empty() {
        output.extend_from_slice(error.to_string().as_bytes());
        output.push(b'\n');
        return Ok(String::from_utf8_lossy(&output).into_owned());
    }
    for diag in diags {
        if emit(&mut output, diag, session.source_map(), supports_color()).is_err() {
            output.extend_from_slice(error.to_string().as_bytes());
            output.push(b'\n');
            break;
        }
    }
    Ok(String::from_utf8_lossy(&output).into_owned())
}

pub fn render_project_error(error: &ProjectError) -> String {
    render_simple_error_line(
        error.diag_code().map(|code| code.to_string()),
        error
            .diag_message()
            .unwrap_or_else(|| error.to_string().into())
            .as_ref(),
    )
}

pub fn render_tooling_error(error: &ToolingError) -> String {
    render_simple_error_line(
        error.diag_code().map(|code| code.to_string()),
        error
            .diag_message()
            .unwrap_or_else(|| error.to_string().into())
            .as_ref(),
    )
}

fn session_error_diags(error: &SessionError) -> &[Diag] {
    match error {
        SessionError::Parse { syntax, .. } => syntax.diags(),
        SessionError::Resolve { diags, .. }
        | SessionError::Sema { diags, .. }
        | SessionError::Ir { diags, .. }
        | SessionError::Emit { diags, .. } => diags,
        SessionError::UnknownModule { .. }
        | SessionError::SourceMap { .. }
        | SessionError::Assembly(_) => &[],
    }
}

fn session_phase(error: &SessionError) -> &'static str {
    match error {
        SessionError::Parse { .. } => "parse",
        SessionError::Resolve { .. } => "resolve",
        SessionError::Sema { .. } => "sema",
        SessionError::Ir { .. } => "ir",
        SessionError::Emit { .. } => "emit",
        SessionError::UnknownModule { .. } => "session",
        SessionError::SourceMap { .. } => "source",
        SessionError::Assembly(_) => "assembly",
    }
}

fn cli_diag(sources: &SourceMap, phase: &'static str, diag: &Diag) -> CliDiagnostic {
    let primary = diag.labels().first().and_then(|label| {
        sources.get(label.source_id()).map(|source| {
            (
                source.path().display().to_string(),
                range_for_label(source, label),
            )
        })
    });
    CliDiagnostic {
        phase,
        severity: diag.level().label(),
        code: diag.code().map(|code| code.to_string()),
        message: diag.message().to_owned(),
        file: primary.as_ref().map(|(file, _)| file.clone()),
        range: primary.and_then(|(_, range)| range),
        labels: diag
            .labels()
            .iter()
            .map(|label| {
                let located = sources.get(label.source_id()).map(|source| {
                    (
                        source.path().display().to_string(),
                        range_for_label(source, label),
                    )
                });
                CliDiagnosticLabel {
                    file: located.as_ref().map(|(file, _)| file.clone()),
                    range: located.and_then(|(_, range)| range),
                    message: label.message().to_owned(),
                }
            })
            .collect(),
        notes: diag.notes().to_vec(),
        hint: diag.hint().map(str::to_owned),
    }
}

fn range_for_label(source: &Source, label: &DiagLabel) -> Option<CliDiagnosticRange> {
    let span = label.span();
    let (start_line, start_col) = source.line_col(span.start);
    let end_offset = span.end.saturating_sub(u32::from(span.len() > 0));
    let (end_line, end_col) = source.line_col(end_offset);
    Some(CliDiagnosticRange {
        start_line,
        start_col,
        end_line,
        end_col,
    })
}

fn tooling_diag(error: &ToolingError) -> CliDiagnostic {
    let file = match error {
        ToolingError::MissingEntrySource { path }
        | ToolingError::Io { path, .. }
        | ToolingError::MissingImport { path, .. } => Some(path.display().to_string()),
        ToolingError::PackageImportRequiresMusi { .. } | ToolingError::Session(_) => None,
    };
    CliDiagnostic {
        phase: "tooling",
        severity: "error",
        code: error.diag_code().map(|code| code.to_string()),
        message: error
            .diag_message()
            .unwrap_or_else(|| error.to_string().into())
            .into_owned(),
        file,
        range: None,
        labels: Vec::new(),
        notes: Vec::new(),
        hint: None,
    }
}

fn project_diag(error: &ProjectError) -> CliDiagnostic {
    let file = match error {
        ProjectError::MissingManifest { path }
        | ProjectError::MissingManifestAncestor { path }
        | ProjectError::MissingPackageRoot { path }
        | ProjectError::MissingModule { path }
        | ProjectError::ManifestJson { path, .. }
        | ProjectError::Io { path, .. }
        | ProjectError::MissingFrozenLockfile { path }
        | ProjectError::FrozenLockfileOutOfDate { path } => Some(path.display().to_string()),
        ProjectError::Validation { .. }
        | ProjectError::DuplicateWorkspaceMember { .. }
        | ProjectError::DuplicatePackageName { .. }
        | ProjectError::MissingPackageVersion { .. }
        | ProjectError::NoRootPackage
        | ProjectError::MissingEntry { .. }
        | ProjectError::UnknownTask { .. }
        | ProjectError::TaskCycle { .. }
        | ProjectError::DependencyCycle { .. }
        | ProjectError::UnresolvedDependency { .. }
        | ProjectError::MissingRegistryRoot { .. }
        | ProjectError::MissingCacheRoot { .. }
        | ProjectError::RegistryVersionNotFound { .. }
        | ProjectError::InvalidVersionRequirement { .. }
        | ProjectError::Session(_) => None,
    };
    CliDiagnostic {
        phase: "project",
        severity: "error",
        code: error.diag_code().map(|code| code.to_string()),
        message: error
            .diag_message()
            .unwrap_or_else(|| error.to_string().into())
            .into_owned(),
        file,
        range: None,
        labels: Vec::new(),
        notes: Vec::new(),
        hint: None,
    }
}

fn path_string(path: &Path) -> String {
    path.display().to_string()
}

fn render_simple_error_line(code: Option<String>, message: &str) -> String {
    match code {
        Some(code) => format!("error[{code}]: {message}\n"),
        None => format!("error: {message}\n"),
    }
}
