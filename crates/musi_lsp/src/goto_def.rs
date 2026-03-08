//! Go-to-definition provider (single-file + stdlib).

use tower_lsp_server::ls_types::{GotoDefinitionResponse, Location, Position, Range, Uri};

use crate::analysis::{AnalyzedDoc, expr_span, position_to_offset, span_to_range};

/// Return the definition location for the symbol under the cursor.
pub fn goto_definition(
    doc: &AnalyzedDoc,
    position: Position,
    uri: &Uri,
    root_uri: Option<&Uri>,
) -> Option<GotoDefinitionResponse> {
    let sema = doc.sema.as_ref()?;

    let offset = position_to_offset(&doc.source, position.line, position.character);

    // Find the smallest expr in expr_defs whose span contains offset.
    let def_id = sema
        .expr_defs
        .iter()
        .filter_map(|(&idx, &def_id)| {
            let span = expr_span(idx, &doc.module);
            if span.start <= offset && offset <= span.start + span.length {
                Some((def_id, span.length))
            } else {
                None
            }
        })
        .min_by_key(|&(_, len)| len)
        .map(|(def_id, _)| def_id)?;

    let def = sema.defs.get(def_id.0 as usize)?;

    // Local definition: return its location in the current file.
    if def.span != musi_shared::Span::DUMMY {
        let range = span_to_range(doc.file_id, def.span, &doc.source_db);
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: uri.clone(),
            range,
        }));
    }

    // Stdlib / prelude definition: search dep_sources for the symbol.
    resolve_stdlib_def(doc, def.name, root_uri)
}

/// Try to resolve a definition with `Span::DUMMY` to a stdlib file location.
///
/// Searches `dep_sources` for a module that has a `def_spans` entry for the
/// given symbol name. If found, constructs a file URI from `root_uri` and
/// returns a `GotoDefinitionResponse` pointing to that definition.
fn resolve_stdlib_def(
    doc: &AnalyzedDoc,
    name: musi_shared::Symbol,
    root_uri: Option<&Uri>,
) -> Option<GotoDefinitionResponse> {
    let root_uri = root_uri?;

    for (mod_key, dep_src) in &doc.dep_sources {
        let Some(&def_span) = dep_src.def_spans.get(&name) else {
            continue;
        };
        if def_span == musi_shared::Span::DUMMY {
            continue;
        }

        // Resolve the file path: <prelude> -> std/prelude.ms, std/foo -> std/foo.ms
        let rel_path = if mod_key == "<prelude>" {
            "std/prelude.ms".to_owned()
        } else {
            format!("{}.ms", mod_key)
        };

        let root_str = root_uri.as_str().trim_end_matches('/');
        let Ok(file_uri) = format!("{}/{}", root_str, rel_path).parse::<Uri>() else {
            continue;
        };

        // Compute the range within the dep source text.
        let range = dep_source_span_to_range(def_span, &dep_src.source);

        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: file_uri,
            range,
        }));
    }

    None
}

/// Convert a `Span` to an LSP `Range` using raw source text (no SourceDb needed).
fn dep_source_span_to_range(span: musi_shared::Span, source: &str) -> Range {
    let start = byte_offset_to_position(span.start, source);
    let end = byte_offset_to_position(span.start + span.length, source);
    Range { start, end }
}

/// Convert a byte offset to an LSP `Position` by scanning newlines.
fn byte_offset_to_position(offset: u32, source: &str) -> Position {
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
