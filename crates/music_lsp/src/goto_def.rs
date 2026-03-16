//! Go-to-definition provider (single-file + stdlib).

use lsp_types::{GotoDefinitionResponse, Location, Position, Range, Url};
use music_shared::{Span, Symbol};

use crate::analysis::{AnalyzedDoc, expr_span, position_to_offset, span_to_range};

/// Return the definition location for the symbol under the cursor.
pub fn goto_definition(
    doc: &AnalyzedDoc,
    position: Position,
    uri: &Url,
    root_uri: Option<&Url>,
) -> Option<GotoDefinitionResponse> {
    let sema = doc.sema.as_ref()?;

    let offset = position_to_offset(&doc.source, position.line, position.character);

    let def_id = sema
        .resolution
        .expr_defs
        .iter()
        .filter_map(|(&idx, &def_id)| {
            let span = expr_span(idx, &doc.module)?;
            if span.start <= offset && offset <= span.end() {
                Some((def_id, span.length))
            } else {
                None
            }
        })
        .min_by_key(|&(_, len)| len)
        .map(|(def_id, _)| def_id)?;

    let def = sema.defs.get(def_id.0 as usize)?;

    if def.span != Span::DUMMY {
        let range = span_to_range(doc.file_id, def.span, &doc.source_db);
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: uri.clone(),
            range,
        }));
    }

    resolve_stdlib_def(doc, def.name, root_uri)
}

fn resolve_stdlib_def(
    doc: &AnalyzedDoc,
    name: Symbol,
    root_uri: Option<&Url>,
) -> Option<GotoDefinitionResponse> {
    let root_uri = root_uri?;

    for (mod_key, dep_src) in &doc.dep_sources {
        let Some(&def_span) = dep_src.def_spans.get(&name) else {
            continue;
        };
        if def_span == Span::DUMMY {
            continue;
        }

        let rel_path = if mod_key == "<prelude>" {
            "std/prelude.ms".to_owned()
        } else {
            format!("{mod_key}.ms")
        };

        let root_str = root_uri.as_str().trim_end_matches('/');
        let Ok(file_uri) = format!("{root_str}/{rel_path}").parse::<Url>() else {
            continue;
        };

        let range = dep_source_span_to_range(def_span, &dep_src.source);

        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: file_uri,
            range,
        }));
    }

    None
}

fn dep_source_span_to_range(span: Span, source: &str) -> Range {
    let start = byte_offset_to_position(span.start, source);
    let end = byte_offset_to_position(span.end(), source);
    Range { start, end }
}

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
