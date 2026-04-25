use std::path::{Path, PathBuf};

use crate::cli::DiagnosticsFormatArg;
use crate::error::{MusiError, MusiResult};
use musi_project::{Project, ProjectEntry, ProjectError, ProjectOptions, load_project_ancestor};
use musi_tooling::{
    CliDiagnosticsReport, DiagnosticsFormat, project_error_report, render_project_error,
    render_session_error, session_error_report,
};
use music_session::{Session, SessionError};

use super::project_target::{project_anchor, reject_workspace_target, resolve_project_entries};

pub(super) fn check(
    target: Option<&Path>,
    workspace: u8,
    diagnostics_format: DiagnosticsFormat,
) -> MusiResult {
    reject_workspace_target(workspace, target)?;
    match check_project(target, workspace) {
        Ok((project, _entries)) => {
            if diagnostics_format == DiagnosticsFormat::Json {
                let report = ok_report(
                    "musi",
                    "check",
                    Some(project.root_dir()),
                    Some(project.root_manifest_path()),
                );
                println!("{}", serde_json::to_string_pretty(&report)?);
            }
            Ok(())
        }
        Err(CheckProjectFailure::ProjectModelFailure {
            package_root,
            manifest,
            error,
        }) => {
            match diagnostics_format {
                DiagnosticsFormat::Text => eprint!("{}", render_project_error(&error)),
                DiagnosticsFormat::Json => {
                    let report = project_error_report(
                        "musi",
                        "check",
                        package_root.as_deref(),
                        manifest.as_deref(),
                        &error,
                    );
                    println!("{}", serde_json::to_string_pretty(&report)?);
                }
            }
            Err(MusiError::CheckCommandFailed)
        }
        Err(CheckProjectFailure::SessionCompilationFailure {
            project,
            session,
            error,
        }) => {
            match diagnostics_format {
                DiagnosticsFormat::Text => eprint!("{}", render_session_error(&session, &error)?),
                DiagnosticsFormat::Json => {
                    let report = session_error_report(
                        "musi",
                        "check",
                        Some(project.root_dir()),
                        Some(project.root_manifest_path()),
                        &session,
                        &error,
                    );
                    println!("{}", serde_json::to_string_pretty(&report)?);
                }
            }
            Err(MusiError::CheckCommandFailed)
        }
    }
}
fn ok_report(
    tool: &str,
    command: &str,
    package_root: Option<&Path>,
    manifest: Option<&Path>,
) -> CliDiagnosticsReport {
    CliDiagnosticsReport::ok(tool, command, package_root, manifest)
}

enum CheckProjectFailure {
    ProjectModelFailure {
        package_root: Option<PathBuf>,
        manifest: Option<PathBuf>,
        error: Box<ProjectError>,
    },
    SessionCompilationFailure {
        project: Box<Project>,
        session: Box<Session>,
        error: Box<SessionError>,
    },
}

fn check_project(
    target: Option<&Path>,
    workspace: u8,
) -> Result<(Project, Vec<ProjectEntry>), CheckProjectFailure> {
    let anchor =
        project_anchor(target).map_err(|error| CheckProjectFailure::ProjectModelFailure {
            package_root: None,
            manifest: None,
            error: match error {
                MusiError::ProjectModelFailed(project) => Box::new(project),
                other => ProjectError::ManifestValidationFailed {
                    message: other.to_string(),
                }
                .into(),
            },
        })?;
    let project = load_project_ancestor(&anchor, ProjectOptions::default()).map_err(|error| {
        CheckProjectFailure::ProjectModelFailure {
            package_root: anchor
                .is_dir()
                .then(|| anchor.clone())
                .or_else(|| anchor.parent().map(Path::to_path_buf)),
            manifest: (anchor.file_name().is_some_and(|name| name == "musi.json"))
                .then(|| anchor.clone()),
            error: Box::new(error),
        }
    })?;
    let entries = resolve_project_entries(&project, target, workspace).map_err(|error| {
        CheckProjectFailure::ProjectModelFailure {
            package_root: Some(project.root_dir().to_path_buf()),
            manifest: Some(project.root_manifest_path().to_path_buf()),
            error: match error {
                MusiError::ProjectModelFailed(project) => Box::new(project),
                other => ProjectError::ManifestValidationFailed {
                    message: other.to_string(),
                }
                .into(),
            },
        }
    })?;
    let mut session =
        project
            .build_session()
            .map_err(|error| CheckProjectFailure::ProjectModelFailure {
                package_root: Some(project.root_dir().to_path_buf()),
                manifest: Some(project.root_manifest_path().to_path_buf()),
                error: Box::new(error),
            })?;
    for entry in &entries {
        if let Err(error) = session.check_module(&entry.module_key) {
            return Err(CheckProjectFailure::SessionCompilationFailure {
                project: Box::new(project),
                session: Box::new(session),
                error: Box::new(error),
            });
        }
    }
    Ok((project, entries))
}

impl From<DiagnosticsFormatArg> for DiagnosticsFormat {
    fn from(value: DiagnosticsFormatArg) -> Self {
        match value {
            DiagnosticsFormatArg::Text => Self::Text,
            DiagnosticsFormatArg::Json => Self::Json,
        }
    }
}
