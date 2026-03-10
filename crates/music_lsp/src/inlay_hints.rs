//! Inlay hints: show inferred types after `let`/`var` bindings
//! and unannotated function parameters.

use music_ast::Expr;
use music_sema::{DefKind, Type};
use tower_lsp_server::ls_types::{InlayHint, InlayHintKind, InlayHintLabel};

use crate::analysis::{AnalyzedDoc, find_name_token, offset_to_position};
use crate::hover::fmt_type_lsp;

/// Compute all inlay hints for the document.
pub fn inlay_hints(doc: &AnalyzedDoc) -> Vec<InlayHint> {
    let Some(sema) = &doc.sema else {
        return vec![];
    };

    let mut hints = Vec::new();

    let unannotated_param_spans = collect_unannotated_param_spans(doc);

    for (span, &def_id) in &sema.resolution.pat_defs {
        let Some(def) = sema.defs.get(def_id.0 as usize) else {
            continue;
        };

        match def.kind {
            DefKind::Let | DefKind::Var => {}
            DefKind::Param => {
                if !unannotated_param_spans.contains(span) {
                    continue;
                }
            }
            _ => continue,
        }

        let Some(ty) = def.ty_info.ty else { continue };

        if matches!(&sema.types[ty], Type::Var(_) | Type::Error) {
            continue;
        }

        let ty_str = fmt_type_lsp(ty, doc, sema);
        let name_span = find_name_token(&doc.lexed.tokens, span.start, def.name).unwrap_or(*span);
        let end_offset = name_span.end();
        let position = offset_to_position(doc.file_id, end_offset, &doc.source_db);

        hints.push(InlayHint {
            position,
            label: InlayHintLabel::String(format!(": {ty_str}")),
            kind: Some(InlayHintKind::TYPE),
            padding_left: Some(false),
            padding_right: Some(true),
            text_edits: None,
            tooltip: None,
            data: None,
        });
    }

    hints
}

/// Walk all `Expr::Fn` nodes and collect spans of unannotated params.
fn collect_unannotated_param_spans(
    doc: &AnalyzedDoc,
) -> std::collections::HashSet<music_shared::Span> {
    let mut spans = std::collections::HashSet::new();
    for idx in 0..doc.module.arenas.exprs.len() {
        let idx = music_shared::Idx::from_raw(u32::try_from(idx).unwrap_or(0));
        if let Expr::Fn { params, .. } = &doc.module.arenas.exprs[idx] {
            for param in params {
                if param.ty.is_none() {
                    spans.insert(param.span);
                }
            }
        }
    }
    spans
}
