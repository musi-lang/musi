use async_lsp::lsp_types::{self, DiagnosticSeverity};
use musi_basic::{diagnostic::Diagnostic, error::Level, source::SourceFile, span::Span};

pub fn convert_diagnostics(
    source_file: &SourceFile,
    musi_diags: &[Diagnostic],
) -> Vec<lsp_types::Diagnostic> {
    musi_diags
        .iter()
        .map(|d| convert_diagnostic(source_file, d))
        .collect()
}

fn convert_diagnostic(source_file: &SourceFile, diag: &Diagnostic) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic {
        range: convert_span(source_file, diag.span),
        severity: Some(convert_level(diag.level)),
        code: None,
        source: Some("musi".to_owned()),
        message: diag.message.clone(),
        related_information: None,
        tags: None,
        data: None,
        code_description: None,
    }
}

fn convert_span(source_file: &SourceFile, span: Span) -> lsp_types::Range {
    let (start_line, start_col) = source_file.location_at(span.lo);
    let (end_line, end_col) = source_file.location_at(span.hi);

    // LSP is 0-indexed, Musi is 1-indexed
    lsp_types::Range {
        start: lsp_types::Position {
            line: u32::try_from(start_line - 1).unwrap_or(0),
            character: u32::try_from(start_col - 1).unwrap_or(0),
        },
        end: lsp_types::Position {
            line: u32::try_from(end_line - 1).unwrap_or(0),
            character: u32::try_from(end_col - 1).unwrap_or(0),
        },
    }
}

const fn convert_level(level: Level) -> DiagnosticSeverity {
    match level {
        Level::Warning => DiagnosticSeverity::WARNING,
        Level::Note => DiagnosticSeverity::INFORMATION,
        _ => DiagnosticSeverity::ERROR,
    }
}
