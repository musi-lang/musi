//! Go-to-type-definition: navigate to the type of the symbol under the cursor.

use lsp_types::{GotoDefinitionResponse, Location, Position, Url};
use music_sema::Type;
use music_shared::Span;

use crate::analysis::{def_at_cursor, def_at_offset, AnalyzedDoc};
use crate::to_proto::{position_to_offset, span_to_range};

pub fn goto_type_definition(
    doc: &AnalyzedDoc,
    position: Position,
    uri: &Url,
) -> Option<GotoDefinitionResponse> {
    let sema = doc.sema.as_ref()?;
    let offset = position_to_offset(&doc.source, position.line, position.character);
    let def = def_at_offset(offset, doc).or_else(|| def_at_cursor(offset, doc))?;
    let ty_idx = def.ty_info.ty?;
    let resolved = sema.unify.resolve(ty_idx, &sema.types);

    let type_def_id = match &sema.types[resolved] {
        Type::Named { def, .. } => Some(*def),
        Type::Fn { ret, .. } => {
            let ret_resolved = sema.unify.resolve(*ret, &sema.types);
            match &sema.types[ret_resolved] {
                Type::Named { def, .. } => Some(*def),
                _ => None,
            }
        }
        _ => None,
    }?;

    let type_def = sema.defs.get(type_def_id.0 as usize)?;
    if type_def.span == Span::DUMMY {
        return None;
    }

    let range = span_to_range(doc.file_id, type_def.span, &doc.source_db);
    Some(GotoDefinitionResponse::Scalar(Location {
        uri: uri.clone(),
        range,
    }))
}
