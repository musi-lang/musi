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
        source: Some("musi".to_string()),
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
            line: (start_line - 1) as u32,
            character: (start_col - 1) as u32,
        },
        end: lsp_types::Position {
            line: (end_line - 1) as u32,
            character: (end_col - 1) as u32,
        },
    }
}

fn convert_level(level: Level) -> DiagnosticSeverity {
    match level {
        Level::Error => DiagnosticSeverity::ERROR,
        Level::Warning => DiagnosticSeverity::WARNING,
        Level::Note => DiagnosticSeverity::INFORMATION,
        _ => unreachable!(),
    }
}
