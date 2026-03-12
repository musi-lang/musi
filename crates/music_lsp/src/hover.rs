//! Hover provider: shows the type and doc-comment of the symbol under the cursor.

use music_sema::DefKind;
use music_sema::types::TypeIdx;
use tower_lsp_server::ls_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};

use crate::analysis::{
    AnalyzedDoc, def_at_cursor, def_name_span, expr_span, extract_doc_comments_from_source,
    position_to_offset, span_to_range,
};

/// Produce a hover response for the given cursor position.
pub fn hover(doc: &AnalyzedDoc, position: Position) -> Option<Hover> {
    let sema = doc.sema.as_ref()?;

    let offset = position_to_offset(&doc.source, position.line, position.character);
    let def = def_at_cursor(offset, doc)?;

    let expr_hit = sema
        .resolution
        .expr_defs
        .iter()
        .filter_map(|(&idx, &def_id)| {
            if def_id != def.id {
                return None;
            }
            let span = expr_span(idx, &doc.module)?;
            if span.start <= offset && offset <= span.end() {
                Some((idx, span))
            } else {
                None
            }
        })
        .min_by_key(|(_, span)| span.length);

    let ty_str = if let Some((idx, _)) = expr_hit {
        let ty = sema.expr_types.get(&idx).copied();
        if let Some(ty) = ty {
            fmt_type_lsp(ty, doc, sema)
        } else if let Some(ty) = def.ty_info.ty {
            fmt_type_lsp(ty, doc, sema)
        } else {
            "?".to_owned()
        }
    } else if let Some(ty) = def.ty_info.ty {
        fmt_type_lsp(ty, doc, sema)
    } else {
        "?".to_owned()
    };

    let kind_kw: &str = match def.kind {
        DefKind::Fn => "let",
        DefKind::Let => "let",
        DefKind::Var => "var",
        DefKind::Param => "",
        DefKind::Type => "type",
        DefKind::Variant => "",
        DefKind::Class => "class",
        DefKind::Given => "given",
        DefKind::Effect => "effect",
        DefKind::EffectOp
        | DefKind::Import
        | DefKind::ForeignFn
        | DefKind::OpaqueType
        | DefKind::Law
        | DefKind::LawVar => "",
    };

    let name = doc.interner.try_resolve(def.name).unwrap_or("<error>");

    let display_name: String = if def.kind == DefKind::Variant {
        if let Some(parent_id) = def.parent {
            let parent_name = sema
                .defs
                .get(parent_id.0 as usize)
                .and_then(|d| doc.interner.try_resolve(d.name))
                .unwrap_or("<error>");
            format!("{parent_name}.{name}")
        } else {
            name.to_owned()
        }
    } else {
        name.to_owned()
    };

    let show_type = ty_str != "?" && !ty_str.starts_with('?') && ty_str != "<error>";

    let signature = if show_type {
        if kind_kw.is_empty() {
            format!("{display_name}: {ty_str}")
        } else {
            format!("{kind_kw} {display_name}: {ty_str}")
        }
    } else if let Some(src_sig) = extract_source_signature(&doc.source, def.span.start) {
        src_sig
    } else if kind_kw.is_empty() {
        display_name.clone()
    } else {
        format!("{kind_kw} {display_name}")
    };

    let local_doc = extract_doc_comments_from_source(def.span.start, &doc.source);
    let doc_text = if local_doc.is_empty() {
        doc.dep_sources
            .values()
            .find_map(|dep| {
                dep.def_spans.get(&def.name).and_then(|&span| {
                    let text = extract_doc_comments_from_source(span.start, &dep.source);
                    if text.is_empty() { None } else { Some(text) }
                })
            })
            .unwrap_or_default()
    } else {
        local_doc
    };

    let mut md = format!("```musi\n{signature}\n```");
    if !doc_text.is_empty() {
        md.push_str("\n\n");
        md.push_str(&doc_text);
    }

    let hover_span = expr_hit
        .map(|(_, span)| span)
        .unwrap_or_else(|| def_name_span(def, &doc.lexed.tokens));
    let range = span_to_range(doc.file_id, hover_span, &doc.source_db);

    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: md,
        }),
        range: Some(range),
    })
}

/// Extract the declaration signature from source text at a definition site.
///
/// Scans from the start of the line containing `start` forward to the first
/// `:=` or `;`, stripping the `export` keyword if present.
fn extract_source_signature(source: &str, start: u32) -> Option<String> {
    let start = usize::try_from(start).ok()?;
    if start > source.len() {
        return None;
    }
    let line_start = source.get(..start)?.rfind('\n').map_or(0, |i| i + 1);
    let rest = source.get(line_start..)?;
    let line_end = rest.find('\n').unwrap_or(rest.len());
    let sig_end = rest
        .get(..line_end)?
        .find(":=")
        .or_else(|| rest.get(..line_end)?.find(';'))
        .unwrap_or(line_end);
    let sig = rest.get(..sig_end)?.trim();
    if sig.is_empty() {
        return None;
    }
    let sig = sig
        .strip_prefix("export")
        .map(str::trim_start)
        .unwrap_or(sig);
    Some(sig.to_owned())
}

/// Format a type for LSP display (hover, inlay hints, etc.).
pub fn fmt_type_lsp(ty: TypeIdx, doc: &AnalyzedDoc, sema: &music_sema::SemaResult) -> String {
    music_sema::types::fmt_type(ty, &sema.types, &sema.defs, &doc.interner).to_string()
}
