use std::path::Path;

use musi_project::ProjectError;
use music_base::diag::{DiagLevel, emit, supports_color};
use music_base::{Diag, OwnedSourceDiag, SourceMap};
use music_base::{DiagLabel, Source, SourceId};
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

impl CliDiagnosticsReport {
    pub const SCHEMA: &'static str = "musi.diagnostics.v1";

    #[must_use]
    pub fn ok(
        tool: &str,
        command: &str,
        package_root: Option<&Path>,
        manifest: Option<&Path>,
    ) -> Self {
        Self {
            schema: Self::SCHEMA,
            tool: tool.to_owned(),
            command: command.to_owned(),
            status: "ok",
            package_root: package_root.map(path_string),
            manifest: manifest.map(path_string),
            diagnostics: Vec::new(),
        }
    }

    #[must_use]
    pub fn error(
        tool: &str,
        command: &str,
        package_root: Option<&Path>,
        manifest: Option<&Path>,
        diagnostics: Vec<CliDiagnostic>,
    ) -> Self {
        Self {
            schema: Self::SCHEMA,
            tool: tool.to_owned(),
            command: command.to_owned(),
            status: "error",
            package_root: package_root.map(path_string),
            manifest: manifest.map(path_string),
            diagnostics,
        }
    }
}

impl CliDiagnostic {
    #[must_use]
    pub fn new(phase: &'static str, severity: &'static str, message: impl Into<String>) -> Self {
        Self {
            phase,
            severity,
            code: None,
            message: message.into(),
            file: None,
            range: None,
            labels: Vec::new(),
            notes: Vec::new(),
            hint: None,
        }
    }

    #[must_use]
    pub const fn error_with_file(
        phase: &'static str,
        code: Option<String>,
        message: String,
        file: Option<String>,
    ) -> Self {
        Self::error_inner(phase, code, message, file, None)
    }

    #[must_use]
    pub const fn error_with_file_and_hint(
        phase: &'static str,
        code: Option<String>,
        message: String,
        file: Option<String>,
        hint: Option<String>,
    ) -> Self {
        Self::error_inner(phase, code, message, file, hint)
    }

    const fn error_inner(
        phase: &'static str,
        code: Option<String>,
        message: String,
        file: Option<String>,
        hint: Option<String>,
    ) -> Self {
        Self {
            phase,
            severity: "error",
            code,
            message,
            file,
            range: None,
            labels: Vec::new(),
            notes: Vec::new(),
            hint,
        }
    }

    #[must_use]
    pub fn with_code(mut self, code: Option<String>) -> Self {
        self.code = code;
        self
    }

    #[must_use]
    pub fn with_file(mut self, file: Option<String>) -> Self {
        self.file = file;
        self
    }

    #[must_use]
    pub const fn with_range(mut self, range: Option<CliDiagnosticRange>) -> Self {
        self.range = range;
        self
    }

    #[must_use]
    pub fn with_labels(mut self, labels: Vec<CliDiagnosticLabel>) -> Self {
        self.labels = labels;
        self
    }

    #[must_use]
    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes = notes;
        self
    }

    #[must_use]
    pub fn with_hint(mut self, hint: Option<String>) -> Self {
        self.hint = hint;
        self
    }
}

impl CliDiagnosticLabel {
    #[must_use]
    pub const fn new(
        file: Option<String>,
        range: Option<CliDiagnosticRange>,
        message: String,
    ) -> Self {
        Self {
            file,
            range,
            message,
        }
    }
}

impl CliDiagnosticRange {
    #[must_use]
    pub const fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}

#[must_use]
pub fn session_error_report(
    tool: &str,
    command: &str,
    package_root: Option<&Path>,
    manifest: Option<&Path>,
    session: &Session,
    error: &SessionError,
) -> CliDiagnosticsReport {
    let diags = session_error_diags(error);
    let diagnostics = diags
        .iter()
        .map(|diag| cli_diag(session.source_map(), session_phase(error), diag))
        .collect();
    CliDiagnosticsReport::error(tool, command, package_root, manifest, diagnostics)
}

#[must_use]
pub fn project_error_report(
    tool: &str,
    command: &str,
    package_root: Option<&Path>,
    manifest: Option<&Path>,
    error: &ProjectError,
) -> CliDiagnosticsReport {
    CliDiagnosticsReport::error(
        tool,
        command,
        package_root,
        manifest,
        vec![project_diag(error)],
    )
}

#[must_use]
pub fn tooling_error_report(
    tool: &str,
    command: &str,
    package_root: Option<&Path>,
    manifest: Option<&Path>,
    error: &ToolingError,
) -> CliDiagnosticsReport {
    CliDiagnosticsReport::error(
        tool,
        command,
        package_root,
        manifest,
        vec![tooling_diag(error)],
    )
}

/// # Errors
///
/// Returns [`ToolingError`] when emitting a structured session diagnostic report fails.
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

#[must_use]
pub fn render_project_error(error: &ProjectError) -> String {
    if let Some(diag) = error.source_diag() {
        return render_project_source_error(diag);
    }
    render_simple_error_line(
        error.diag_code().map(|code| code.to_string()),
        error
            .diag_message()
            .unwrap_or_else(|| error.to_string().into())
            .as_ref(),
    )
}

#[must_use]
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
        SessionError::ModuleParseFailed { syntax, .. } => syntax.diags(),
        SessionError::ModuleResolveFailed { diags, .. }
        | SessionError::ModuleSemanticCheckFailed { diags, .. }
        | SessionError::ModuleLoweringFailed { diags, .. }
        | SessionError::ModuleEmissionFailed { diags, .. } => diags,
        SessionError::ModuleNotRegistered { .. }
        | SessionError::LawSuiteSynthesisFailed { .. }
        | SessionError::SourceMapUpdateFailed { .. }
        | SessionError::ArtifactTransportFailed(_) => &[],
    }
}

const fn session_phase(error: &SessionError) -> &'static str {
    match error {
        SessionError::ModuleParseFailed { .. } => "parse",
        SessionError::ModuleResolveFailed { .. } => "resolve",
        SessionError::ModuleSemanticCheckFailed { .. } => "sema",
        SessionError::ModuleLoweringFailed { .. } => "ir",
        SessionError::ModuleEmissionFailed { .. } => "emit",
        SessionError::ModuleNotRegistered { .. } => "session",
        SessionError::LawSuiteSynthesisFailed { .. } => "laws",
        SessionError::SourceMapUpdateFailed { .. } => "source",
        SessionError::ArtifactTransportFailed(_) => "assembly",
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
    CliDiagnostic::new(phase, diag.level().label(), diag.message().to_owned())
        .with_code(diag.code().map(|code| code.to_string()))
        .with_file(primary.as_ref().map(|(file, _)| file.clone()))
        .with_range(primary.map(|(_, range)| range))
        .with_labels(
            diag.labels()
                .iter()
                .map(|label| {
                    let located = sources.get(label.source_id()).map(|source| {
                        (
                            source.path().display().to_string(),
                            range_for_label(source, label),
                        )
                    });
                    CliDiagnosticLabel::new(
                        located.as_ref().map(|(file, _)| file.clone()),
                        located.map(|(_, range)| range),
                        label.message().to_owned(),
                    )
                })
                .collect(),
        )
        .with_notes(diag.notes().to_vec())
        .with_hint(diag.hint().map(str::to_owned))
}

fn range_for_label(source: &Source, label: &DiagLabel) -> CliDiagnosticRange {
    let span = label.span();
    let (start_line, start_col) = source.line_col(span.start);
    let end_offset = span.end.saturating_sub(u32::from(!span.is_empty()));
    let (end_line, end_col) = source.line_col(end_offset);
    CliDiagnosticRange::new(start_line, start_col, end_line, end_col)
}

fn tooling_diag(error: &ToolingError) -> CliDiagnostic {
    let file = match error {
        ToolingError::MissingEntrySource { path }
        | ToolingError::ToolingIoFailed { path, .. }
        | ToolingError::MissingImport { path, .. } => Some(path.display().to_string()),
        ToolingError::PackageImportRequiresMusi { .. }
        | ToolingError::SessionCompilationFailed(_) => None,
    };
    CliDiagnostic::error_with_file(
        "tooling",
        error.diag_code().map(|code| code.to_string()),
        error
            .diag_message()
            .unwrap_or_else(|| error.to_string().into())
            .into_owned(),
        file,
    )
}

fn project_diag(error: &ProjectError) -> CliDiagnostic {
    if let Some(diag) = error.source_diag() {
        return project_source_diag(diag);
    }
    let file = match error {
        ProjectError::MissingManifest { path }
        | ProjectError::MissingManifestAncestor { path }
        | ProjectError::MissingPackageRoot { path }
        | ProjectError::MissingModule { path }
        | ProjectError::ManifestJsonInvalid { path, .. }
        | ProjectError::ProjectIoFailed { path, .. }
        | ProjectError::MissingFrozenLockfile { path }
        | ProjectError::FrozenLockfileOutOfDate { path } => Some(path.display().to_string()),
        ProjectError::ManifestValidationFailed { .. }
        | ProjectError::DuplicateWorkspaceMember { .. }
        | ProjectError::DuplicatePackageName { .. }
        | ProjectError::MissingPackageVersion { .. }
        | ProjectError::NoRootPackage
        | ProjectError::PackageEntryModuleMissing { .. }
        | ProjectError::UnknownTask { .. }
        | ProjectError::TaskDependencyCycle { .. }
        | ProjectError::PackageDependencyCycle { .. }
        | ProjectError::UnresolvedImport { .. }
        | ProjectError::MissingRegistryRoot { .. }
        | ProjectError::MissingCacheRoot { .. }
        | ProjectError::RegistryVersionNotFound { .. }
        | ProjectError::InvalidVersionRequirement { .. }
        | ProjectError::UnknownPackage { .. }
        | ProjectError::PackageGraphEntryMissing { .. }
        | ProjectError::SessionCompilationFailed(_)
        | ProjectError::SourceDiagnostic(_) => None,
    };
    CliDiagnostic::error_with_file(
        "project",
        error.diag_code().map(|code| code.to_string()),
        error
            .diag_message()
            .unwrap_or_else(|| error.to_string().into())
            .into_owned(),
        file,
    )
}

fn project_source_diag(error: &OwnedSourceDiag) -> CliDiagnostic {
    let mut sources = SourceMap::new();
    let Ok(source_id) = sources.add(error.path().to_path_buf(), error.text().to_owned()) else {
        return CliDiagnostic::error_with_file_and_hint(
            "project",
            error.diag().code().map(|code| code.to_string()),
            error.diag().message().to_owned(),
            Some(error.path().display().to_string()),
            error.diag().hint().map(str::to_owned),
        );
    };
    cli_diag(
        &sources,
        "project",
        &remap_owned_source_diag(error, source_id),
    )
}

fn path_string(path: &Path) -> String {
    path.display().to_string()
}

fn render_simple_error_line(code: Option<String>, message: &str) -> String {
    code.map_or_else(
        || format!("error: {message}\n"),
        |code| format!("error[{code}]: {message}\n"),
    )
}

fn render_project_source_error(error: &OwnedSourceDiag) -> String {
    let mut sources = SourceMap::new();
    let Ok(source_id) = sources.add(error.path().to_path_buf(), error.text().to_owned()) else {
        return render_simple_error_line(
            error.diag().code().map(|code| code.to_string()),
            error.diag().message(),
        );
    };
    let diag = remap_owned_source_diag(error, source_id);
    let mut output = Vec::new();
    if emit(&mut output, &diag, &sources, supports_color()).is_err() {
        return render_simple_error_line(
            error.diag().code().map(|code| code.to_string()),
            error.diag().message(),
        );
    }
    String::from_utf8_lossy(&output).into_owned()
}

fn remap_owned_source_diag(error: &OwnedSourceDiag, source_id: SourceId) -> Diag {
    let mut diag = match error.diag().level() {
        DiagLevel::Fatal => Diag::fatal(error.diag().message()),
        DiagLevel::Error => Diag::error(error.diag().message()),
        DiagLevel::Warning => Diag::warning(error.diag().message()),
        DiagLevel::Note => Diag::note(error.diag().message()),
    };
    if let Some(code) = error.diag().code() {
        diag = diag.with_code(code);
    }
    for label in error.diag().labels() {
        diag = diag.with_label(label.span(), source_id, label.message());
    }
    for note in error.diag().notes() {
        diag = diag.with_note(note.clone());
    }
    if let Some(hint) = error.diag().hint() {
        diag = diag.with_hint(hint.to_owned());
    }
    diag
}
