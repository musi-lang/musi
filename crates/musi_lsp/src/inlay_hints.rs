//! Inlay hints: show inferred types after `const`/`var` bindings.
//!
//! Produces `: TypeStr` annotations placed immediately after the binding name,
//! giving developers instant feedback on inferred types without requiring hover.

use musi_sema::{DefKind, Type};
use tower_lsp_server::ls_types::{InlayHint, InlayHintKind, InlayHintLabel};

use crate::analysis::{AnalyzedDoc, find_name_token, offset_to_position};
use crate::hover::fmt_type;

/// Compute type-annotation inlay hints for the entire document.
pub fn inlay_hints(doc: &AnalyzedDoc) -> Vec<InlayHint> {
    let Some(sema) = &doc.sema else {
        return vec![];
    };

    let mut hints = Vec::new();

    for (span, &def_id) in &sema.pat_defs {
        let Some(def) = sema.defs.get(def_id.0 as usize) else {
            continue;
        };

        // Show hints for const/var bindings only.
        // Named fn params already carry explicit annotations; lambda params are usually
        // obvious from context. Can be extended later.
        match def.kind {
            DefKind::Const | DefKind::Var => {}
            _ => continue,
        }

        let Some(ty) = &def.ty else { continue };
        let resolved = sema.unify_table.resolve(ty.clone());

        // Skip unknown / error types — they produce unhelpful `'_` hints.
        if matches!(resolved, Type::Var(_) | Type::Error) {
            continue;
        }

        let ty_str = fmt_type(&resolved, doc, sema);

        // Place the hint right after the name token (not the full pattern span).
        let name_span = find_name_token(&doc.lexed.tokens, span.start, def.name)
            .unwrap_or(*span);
        let end_offset = name_span.start + name_span.length;
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
