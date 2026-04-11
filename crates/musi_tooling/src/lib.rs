mod analysis;
mod artifact;
mod diagnostics;
mod direct;
mod errors;

pub use analysis::{
    ToolHover, collect_project_diagnostics, collect_project_diagnostics_with_overlay,
    hover_for_project_file, hover_for_project_file_with_overlay,
};
pub use artifact::{read_artifact_bytes, write_artifact_bytes, write_text_output};
pub use diagnostics::{
    CliDiagnostic, CliDiagnosticLabel, CliDiagnosticRange, CliDiagnosticsReport, DiagnosticsFormat,
    project_error_report, render_project_error, render_session_error, render_tooling_error,
    session_error_report, tooling_error_report,
};
pub use direct::{DirectGraph, load_direct_graph};
pub use errors::{ToolingError, ToolingResult};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
