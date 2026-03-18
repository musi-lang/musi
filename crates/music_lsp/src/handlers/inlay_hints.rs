//! Inlay hints: show inferred types after `let`/`var` bindings
//! and unannotated function parameters.

use std::collections::HashSet;

use lsp_types::{InlayHint, InlayHintKind, InlayHintLabel};
use music_ast::Expr;
use music_lex::TokenKind;
use music_sema::{DefKind, SemaResult, Type};
use music_shared::{Idx, Span};

use crate::analysis::{AnalyzedDoc, find_name_token};
use crate::to_proto::{fmt_type_lsp, offset_to_position};

pub struct InlayHintConfig {
    pub binding_types: bool,
    pub return_types: bool,
    pub parameter_types: bool,
}

impl Default for InlayHintConfig {
    fn default() -> Self {
        Self {
            binding_types: true,
            return_types: true,
            parameter_types: true,
        }
    }
}

/// Compute all inlay hints for the document.
pub fn inlay_hints(doc: &AnalyzedDoc, config: &InlayHintConfig) -> Vec<InlayHint> {
    let Some(sema) = &doc.sema else {
        return vec![];
    };

    let mut hints = vec![];

    let unannotated_param_spans = collect_unannotated_param_spans(doc);
    let annotated_binding_spans = collect_annotated_binding_spans(doc);

    for (span, &def_id) in &sema.resolution.pat_defs {
        let Some(def) = sema.defs.get(def_id.0 as usize) else {
            continue;
        };

        match def.kind {
            DefKind::Let | DefKind::Var => {
                if !config.binding_types {
                    continue;
                }
                if annotated_binding_spans
                    .iter()
                    .any(|s| s.start <= span.start && span.end() <= s.end())
                {
                    continue;
                }
            }
            DefKind::Param => {
                if !config.parameter_types {
                    continue;
                }
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

    if config.return_types {
        collect_return_type_hints(doc, sema, &mut hints);
    }

    hints
}

/// Walk all `Expr::Fn` nodes and collect spans of unannotated params.
fn collect_unannotated_param_spans(doc: &AnalyzedDoc) -> HashSet<Span> {
    let mut spans = HashSet::new();
    for idx in 0..doc.module.arenas.exprs.len() {
        let idx = Idx::from_raw(u32::try_from(idx).unwrap_or(0));
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

/// Walk all `Expr::Binding` and `Expr::Let` nodes and collect the `LetFields`
/// span for bindings that already carry an explicit type annotation.
fn collect_annotated_binding_spans(doc: &AnalyzedDoc) -> Vec<Span> {
    let mut spans = vec![];
    for idx in 0..doc.module.arenas.exprs.len() {
        let idx = Idx::from_raw(u32::try_from(idx).unwrap_or(0));
        match &doc.module.arenas.exprs[idx] {
            Expr::Binding { fields, .. } | Expr::Let { fields, .. } => {
                if fields.ty.is_some() {
                    spans.push(fields.span);
                }
            }
            _ => {}
        }
    }
    spans
}

/// Walk all `Expr::Fn` nodes and emit ` -> RetTy` hints for those without an
/// explicit return type annotation.
fn collect_return_type_hints(doc: &AnalyzedDoc, sema: &SemaResult, hints: &mut Vec<InlayHint>) {
    for raw_idx in 0..doc.module.arenas.exprs.len() {
        let idx = Idx::from_raw(u32::try_from(raw_idx).unwrap_or(0));

        let Expr::Fn { params, ret_ty, .. } = &doc.module.arenas.exprs[idx] else {
            continue;
        };

        if ret_ty.is_some() {
            continue;
        }

        let last_param = match params.last() {
            Some(p) => p,
            None => continue,
        };

        let rparen = doc
            .lexed
            .tokens
            .iter()
            .find(|t| t.kind == TokenKind::RParen && t.span.start >= last_param.span.end());
        let rparen_end = match rparen {
            Some(t) => t.span.end(),
            None => continue,
        };

        let Some(&fn_ty_idx) = sema.expr_types.get(&idx) else {
            continue;
        };

        let ret_idx = match &sema.types[fn_ty_idx] {
            Type::Fn { ret, .. } => *ret,
            _ => continue,
        };

        let resolved_ret = sema.unify.resolve(ret_idx, &sema.types);

        match &sema.types[resolved_ret] {
            Type::Var(_) | Type::Error => continue,
            Type::Tuple { elems } if elems.is_empty() => continue,
            _ => {}
        }

        let ty_str = fmt_type_lsp(resolved_ret, doc, sema);
        let position = offset_to_position(doc.file_id, rparen_end, &doc.source_db);

        hints.push(InlayHint {
            position,
            label: InlayHintLabel::String(format!(" -> {ty_str}")),
            kind: Some(InlayHintKind::TYPE),
            padding_left: Some(false),
            padding_right: Some(true),
            text_edits: None,
            tooltip: None,
            data: None,
        });
    }
}
