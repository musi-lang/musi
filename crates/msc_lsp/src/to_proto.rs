use lsp_types::{Diagnostic, DiagnosticSeverity, DiagnosticTag, Position, Range};
use msc_sema::types;
use msc_shared::{FileId, Severity, SourceDb, Span};

use crate::analysis::doc::AnalyzedDoc;
use msc_sema::{SemaResult, TypeIdx};

/// Convert a 0-based (line, character) LSP position to a byte offset in `source`.
pub fn position_to_offset(source: &str, line: u32, character: u32) -> u32 {
    let mut cur_line = 0u32;
    let mut line_start = 0usize;
    for (i, ch) in source.char_indices() {
        if cur_line == line {
            break;
        }
        if ch == '\n' {
            cur_line += 1;
            line_start = i + 1;
        }
    }
    u32::try_from(line_start + character as usize).unwrap_or(u32::MAX)
}

/// Convert a byte offset to an LSP `Position`.
pub fn offset_to_position(file_id: FileId, offset: u32, source_db: &SourceDb) -> Position {
    let src_len = u32::try_from(source_db.source(file_id).len()).unwrap_or(u32::MAX);
    let clamped = offset.min(src_len);
    let (line1, col1) = source_db.lookup(file_id, clamped);
    Position {
        line: line1 - 1,
        character: col1 - 1,
    }
}

/// Convert a byte offset to an LSP `Position` using raw source text (no `SourceDb`).
///
/// Used for cross-file navigation where only the raw source string is available.
pub fn offset_to_position_raw(offset: u32, source: &str) -> Position {
    let offset = (offset as usize).min(source.len());
    let mut line = 0u32;
    let mut col = 0u32;
    for (i, ch) in source.char_indices() {
        if i >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += 1;
        }
    }
    Position {
        line,
        character: col,
    }
}

/// Convert a `Span` to an LSP `Range`.
pub fn span_to_range(file_id: FileId, span: Span, source_db: &SourceDb) -> Range {
    Range {
        start: offset_to_position(file_id, span.start, source_db),
        end: offset_to_position(file_id, span.end(), source_db),
    }
}

/// Convert a `Span` in a raw source string to an LSP `Range` (no `SourceDb`).
pub fn span_to_range_raw(span: Span, source: &str) -> Range {
    Range {
        start: offset_to_position_raw(span.start, source),
        end: offset_to_position_raw(span.end(), source),
    }
}

pub(crate) fn to_lsp_diags<'a>(
    iter: impl Iterator<Item = &'a msc_shared::Diagnostic>,
    source_db: &SourceDb,
) -> Vec<Diagnostic> {
    iter.map(|d| {
        let start = offset_to_position(d.primary.file_id, d.primary.span.start, source_db);
        let end = offset_to_position(d.primary.file_id, d.primary.span.end(), source_db);
        let tags = if d.severity == Severity::Warning && d.message.starts_with("unused") {
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

fn severity_to_lsp(s: Severity) -> DiagnosticSeverity {
    match s {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Note => DiagnosticSeverity::INFORMATION,
    }
}

/// Format a type for LSP display (hover, inlay hints, completion, etc.).
pub fn fmt_type_lsp(ty: TypeIdx, doc: &AnalyzedDoc, sema: &SemaResult) -> String {
    types::fmt_type(
        ty,
        &sema.types,
        &sema.defs,
        &doc.interner,
        Some(&sema.unify),
    )
    .to_string()
}
