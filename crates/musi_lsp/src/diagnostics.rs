use std::collections::HashMap;

use musi_lex::lex;
use musi_parse::parse;
use musi_sema::{ModuleExports, analyze};
use musi_shared::{DiagnosticBag, FileId, Interner, Severity, SourceDb};
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, DiagnosticTag, Position, Range};

/// Run the Musi lex + parse pipeline on `source` and return LSP diagnostics.
/// Only parse-phase errors are reported (no type-checking in v1).
pub fn compute(source: &str) -> Vec<Diagnostic> {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let file_id = source_db.add("<document>", source);
    let lexed = lex(source, file_id, &mut interner, &mut diags);
    let _module = parse(&lexed.tokens, file_id, &mut diags, &interner);

    let imports: HashMap<String, ModuleExports> = HashMap::new();
    let _sema = analyze(&_module, &interner, file_id, &mut diags, &imports);

    diags
        .iter()
        .map(|d| {
            let start = offset_to_position(d.primary.file_id, d.primary.span.start, &source_db);
            let end = offset_to_position(
                d.primary.file_id,
                d.primary.span.end(),
                &source_db,
            );
            let tags = if d.severity == Severity::Warning
                && d.message.starts_with("unused")
            {
                Some(vec![DiagnosticTag::UNNECESSARY])
            } else {
                None
            };
            Diagnostic {
                range: Range { start, end },
                severity: Some(severity_to_lsp(d.severity)),
                message: d.message.to_string(),
                source: Some("musi".to_owned()),
                tags,
                ..Diagnostic::default()
            }
        })
        .collect()
}

fn offset_to_position(file_id: FileId, offset: u32, source_db: &SourceDb) -> Position {
    let src_len = u32::try_from(source_db.source(file_id).len()).unwrap_or(u32::MAX);
    let clamped = offset.min(src_len);
    let (line1, col1) = source_db.lookup(file_id, clamped);
    Position {
        // LSP positions are 0-based; source_db returns 1-based
        line: line1 - 1,
        character: col1 - 1,
    }
}

fn severity_to_lsp(s: Severity) -> DiagnosticSeverity {
    match s {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Note => DiagnosticSeverity::INFORMATION,
    }
}
