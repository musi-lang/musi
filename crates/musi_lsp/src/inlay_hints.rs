//! Inlay hints: show inferred types after `const`/`var` bindings,
//! unannotated lambda parameters, and Scala-style call-site parameter names.
//!
//! Produces `: TypeStr` annotations after bindings and `param_name: ` labels
//! before each argument at known call sites.

use musi_parse::ast::Expr;
use musi_sema::{DefKind, Type};
use tower_lsp_server::ls_types::{InlayHint, InlayHintKind, InlayHintLabel};

use crate::analysis::{AnalyzedDoc, expr_span, find_name_token, offset_to_position};
use crate::hover::fmt_type;

/// Compute all inlay hints for the document.
pub fn inlay_hints(doc: &AnalyzedDoc) -> Vec<InlayHint> {
    let Some(sema) = &doc.sema else {
        return vec![];
    };

    let mut hints = Vec::new();

    // -- Type-annotation hints (const/var/unannotated lambda params) ----------

    let unannotated_param_spans = collect_unannotated_lambda_param_spans(doc);

    for (span, &def_id) in &sema.pat_defs {
        let Some(def) = sema.defs.get(def_id.0 as usize) else {
            continue;
        };

        match def.kind {
            DefKind::Const | DefKind::Var => {}
            DefKind::Param => {
                if !unannotated_param_spans.contains(span) {
                    continue;
                }
            }
            _ => continue,
        }

        let Some(ty) = &def.ty else { continue };
        let resolved = sema.unify_table.resolve(ty.clone());

        if matches!(resolved, Type::Var(_) | Type::Error) {
            continue;
        }

        let ty_str = fmt_type(&resolved, doc, sema);
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

    // -- Call-site parameter-name hints (Scala style) -------------------------

    for (fn_def_id, arg_idxs) in &sema.call_sites {
        let Some(param_names) = sema.fn_params.get(fn_def_id) else {
            continue;
        };

        // Skip single-argument calls — less noise.
        if arg_idxs.len() <= 1 {
            continue;
        }

        for (arg_idx, param_sym) in arg_idxs.iter().zip(param_names.iter()) {
            let param_name = doc.interner.resolve(*param_sym);

            // Skip `_`-prefixed param names (internal/intentionally unnamed).
            if param_name.starts_with('_') {
                continue;
            }

            let arg_span = expr_span(*arg_idx, &doc.module);
            if arg_span.length == 0 {
                continue;
            }

            let position = offset_to_position(doc.file_id, arg_span.start, &doc.source_db);

            hints.push(InlayHint {
                position,
                label: InlayHintLabel::String(format!("{param_name}: ")),
                kind: Some(InlayHintKind::PARAMETER),
                padding_left: Some(false),
                padding_right: Some(false),
                text_edits: None,
                tooltip: None,
                data: None,
            });
        }
    }

    hints
}

/// Walk all `Expr::Lambda` nodes and collect spans of unannotated params.
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
