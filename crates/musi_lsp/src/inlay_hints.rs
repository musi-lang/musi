//! Inlay hints: show inferred types after `const`/`var` bindings and
//! unannotated lambda parameters.
//!
//! Produces `: TypeStr` annotations placed immediately after the binding name,
//! giving developers instant feedback on inferred types without requiring hover.

use musi_parse::ast::Expr;
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

    // Collect the spans of lambda params that lack an explicit type annotation.
    // We use these to decide whether to show a hint for a Param def.
    let unannotated_param_spans = collect_unannotated_lambda_param_spans(doc);

    for (span, &def_id) in &sema.pat_defs {
        let Some(def) = sema.defs.get(def_id.0 as usize) else {
            continue;
        };

        match def.kind {
            DefKind::Const | DefKind::Var => {}
            DefKind::Param => {
                // Only show hints for lambda params that have no explicit type annotation.
                if !unannotated_param_spans.contains(span) {
                    continue;
                }
            }
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
        let name_span = find_name_token(&doc.lexed.tokens, span.start, def.name).unwrap_or(*span);
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

/// Walk all `Expr::Lambda` nodes in the module and collect the spans of params
/// that have no explicit type annotation (`param.ty.is_none()`).
fn collect_unannotated_lambda_param_spans(
    doc: &AnalyzedDoc,
) -> std::collections::HashSet<musi_shared::Span> {
    let mut spans = std::collections::HashSet::new();
    for expr in doc.module.ctx.exprs.iter() {
        if let Expr::Lambda { params, .. } = expr {
            for param in params {
                if param.ty.is_none() {
                    spans.insert(param.span);
                }
            }
        }
    }
    spans
}
