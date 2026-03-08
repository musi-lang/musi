//! Find-references and rename providers (single-file).

use tower_lsp_server::ls_types::{
    Location, Position, ReferenceContext, TextEdit, Uri, WorkspaceEdit,
};

use crate::analysis::{AnalyzedDoc, expr_span, position_to_offset, span_to_range};

/// Find all references to the symbol under the cursor (single-file).
pub fn find_references(
    doc: &AnalyzedDoc,
    position: Position,
    context: &ReferenceContext,
    uri: &Uri,
) -> Option<Vec<Location>> {
    let sema = doc.sema.as_ref()?;

    let offset = position_to_offset(&doc.source, position.line, position.character);

    // Find the def_id for the symbol under the cursor.
    let target_def_id = sema
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

    // Collect all expr_defs that refer to the same def.
    let mut locations: Vec<Location> = sema
        .expr_defs
        .iter()
        .filter_map(|(&idx, &def_id)| {
            if def_id != target_def_id {
                return None;
            }
            let span = expr_span(idx, &doc.module);
            let range = span_to_range(doc.file_id, span, &doc.source_db);
            Some(Location {
                uri: uri.clone(),
                range,
            })
        })
        .collect();

    // Optionally include the declaration site.
    if context.include_declaration
        && let Some(def) = sema.defs.get(target_def_id.0 as usize)
        && def.span != musi_shared::Span::DUMMY
    {
        let range = span_to_range(doc.file_id, def.span, &doc.source_db);
        locations.push(Location {
            uri: uri.clone(),
            range,
        });
    }

    // Sort by position for a stable ordering.
    locations.sort_by_key(|loc| (loc.range.start.line, loc.range.start.character));
    locations.dedup_by_key(|loc| (loc.range.start.line, loc.range.start.character));

    Some(locations)
}

/// Rename all occurrences of the symbol under the cursor (single-file).
pub fn rename(
    doc: &AnalyzedDoc,
    position: Position,
    new_name: String,
    uri: &Uri,
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
    changes.insert(uri.clone(), edits);

    Some(WorkspaceEdit {
        changes: Some(changes),
        ..WorkspaceEdit::default()
    })
}
