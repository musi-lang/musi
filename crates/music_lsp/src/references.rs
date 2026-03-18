//! Find-references and rename providers (single-file).

use lsp_types::{
    Location, Position, PrepareRenameResponse, ReferenceContext, TextEdit, Url, WorkspaceEdit,
};
use music_shared::Span;

use crate::analysis::{
    AnalyzedDoc, def_at_cursor, def_at_offset, expr_span, find_name_token, position_to_offset,
    span_to_range,
};

/// Find all references to the symbol under the cursor (single-file).
pub fn find_references(
    doc: &AnalyzedDoc,
    position: Position,
    context: &ReferenceContext,
    uri: &Url,
) -> Option<Vec<Location>> {
    let sema = doc.sema.as_ref()?;

    let offset = position_to_offset(&doc.source, position.line, position.character);

    let target_def_id = sema
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
        .map(|(def_id, _)| def_id)
        .or_else(|| {
            sema.resolution
                .pat_defs
                .iter()
                .filter_map(|(&span, &def_id)| {
                    if span.start <= offset && offset <= span.end() {
                        Some((def_id, span.length))
                    } else {
                        None
                    }
                })
                .min_by_key(|&(_, len)| len)
                .map(|(def_id, _)| def_id)
        })?;

    let mut locations: Vec<Location> = sema
        .resolution
        .expr_defs
        .iter()
        .filter_map(|(&idx, &def_id)| {
            if def_id != target_def_id {
                return None;
            }
            let span = expr_span(idx, &doc.module)?;
            let range = span_to_range(doc.file_id, span, &doc.source_db);
            Some(Location {
                uri: uri.clone(),
                range,
            })
        })
        .collect();

    for (&span, &def_id) in &sema.resolution.pat_defs {
        if def_id == target_def_id {
            let range = span_to_range(doc.file_id, span, &doc.source_db);
            locations.push(Location {
                uri: uri.clone(),
                range,
            });
        }
    }

    if context.include_declaration
        && let Some(def) = sema.defs.get(target_def_id.0 as usize)
        && def.span != Span::DUMMY
    {
        let range = span_to_range(doc.file_id, def.span, &doc.source_db);
        let loc = Location {
            uri: uri.clone(),
            range,
        };
        if !locations.contains(&loc) {
            locations.push(loc);
        }
    }

    locations.sort_by_key(|loc| (loc.range.start.line, loc.range.start.character));
    locations.dedup_by_key(|loc| (loc.range.start.line, loc.range.start.character));

    Some(locations)
}

/// Validate that the symbol under the cursor can be renamed and return its name range.
pub fn prepare_rename(
    doc: &AnalyzedDoc,
    position: Position,
    _uri: &Url,
) -> Option<PrepareRenameResponse> {
    let offset = position_to_offset(&doc.source, position.line, position.character);
    let def = def_at_offset(offset, doc).or_else(|| def_at_cursor(offset, doc))?;
    if def.span == Span::DUMMY {
        return None;
    }
    let name_span =
        find_name_token(&doc.lexed.tokens, def.span.start, def.name).unwrap_or(def.span);
    let range = span_to_range(doc.file_id, name_span, &doc.source_db);
    Some(PrepareRenameResponse::Range(range))
}

/// Rename all occurrences of the symbol under the cursor (single-file).
pub fn rename(
    doc: &AnalyzedDoc,
    position: Position,
    new_name: String,
    uri: &Url,
) -> Option<WorkspaceEdit> {
    let ctx = ReferenceContext {
        include_declaration: true,
    };
    let locations = find_references(doc, position, &ctx, uri)?;

    let edits: Vec<TextEdit> = locations
        .into_iter()
        .map(|loc| TextEdit {
            range: loc.range,
            new_text: new_name.clone(),
        })
        .collect();

    let mut changes = std::collections::HashMap::new();
    let _prev = changes.insert(uri.clone(), edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        ..WorkspaceEdit::default()
    })
}
