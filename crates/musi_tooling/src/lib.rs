mod analysis;
mod analysis_support;
pub use diag::ToolingDiagKind;
mod artifact;
mod completion;
mod diag;
mod diagnostics;
mod direct;
mod errors;
mod semantic;

pub use analysis::{
    ToolHover, ToolInlayHint, ToolInlayHintKind, ToolPosition, ToolRange, ToolSymbolKind,
    collect_project_diagnostics, collect_project_diagnostics_with_overlay, hover_for_project_file,
    hover_for_project_file_with_overlay, inlay_hints_for_project_file,
    inlay_hints_for_project_file_with_overlay, module_docs_for_project_file,
    module_docs_for_project_file_with_overlay,
};
pub use artifact::{read_artifact_bytes, write_artifact_bytes, write_text_output};
pub use completion::{
    ToolCompletion, ToolCompletionKind, ToolCompletionList, completions_for_project_file,
    completions_for_project_file_with_overlay,
};
pub use diagnostics::{
    CliDiagnostic, CliDiagnosticLabel, CliDiagnosticRange, CliDiagnosticsReport, DiagnosticsFormat,
    project_error_report, render_project_error, render_session_error, render_tooling_error,
    session_error_report, tooling_error_report,
};
pub use direct::{DirectGraph, load_direct_graph};
pub use errors::{ToolingError, ToolingResult};
pub use semantic::{
    ToolSemanticModifier, ToolSemanticModifierList, ToolSemanticToken, ToolSemanticTokenKind,
    ToolSemanticTokenList, semantic_tokens_for_project_file,
    semantic_tokens_for_project_file_with_overlay,
};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
