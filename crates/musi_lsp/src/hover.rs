//! Hover provider: shows the type and doc-comment of the symbol under the cursor.

use musi_sema::{DefKind, Type, TypeFlavor};
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

    // Find the smallest expr at the cursor that maps to this def — gives us
    // both a precise type (from expr_types) and the identifier span to highlight.
    let expr_hit = sema
        .expr_defs
        .iter()
        .filter_map(|(&idx, &def_id)| {
            if def_id != def.id {
                return None;
            }
            let span = expr_span(idx, &doc.module);
            if span.start <= offset && offset <= span.start + span.length {
                Some((idx, span))
            } else {
                None
            }
        })
        .min_by_key(|(_, span)| span.length);

    let ty_str = if let Some((idx, _)) = expr_hit {
        let ty = sema.expr_types.get(&idx).cloned();
        if let Some(ty) = ty {
            fmt_type(&sema.unify_table.resolve(ty), doc, sema)
        } else if let Some(ty) = &def.ty {
            fmt_type(&sema.unify_table.resolve(ty.clone()), doc, sema)
        } else {
            "?".to_owned()
        }
    } else if let Some(ty) = &def.ty {
        fmt_type(&sema.unify_table.resolve(ty.clone()), doc, sema)
    } else {
        "?".to_owned()
    };

    // `""` = no keyword prefix (params are implicitly const; unknown-flavour types)
    let kind_kw: &str = match def.kind {
        DefKind::Fn => "fn",
        DefKind::Const => {
            let is_fn = def
                .ty
                .as_ref()
                .map(|t: &Type| matches!(sema.unify_table.resolve(t.clone()), Type::Arrow(..)))
                .unwrap_or(false);
            if is_fn { "fn" } else { "const" }
        }
        DefKind::Var => "var",
        // Params are implicitly const — only show "var" when explicitly mutable.
        DefKind::Param => {
            if def.is_var {
                "var"
            } else {
                ""
            }
        }
        DefKind::Type => match def.type_flavor {
            Some(TypeFlavor::Record) => "record",
            Some(TypeFlavor::Choice) => "choice",
            // `type` is not a Musi keyword — show nothing for imported/opaque types.
            None => "",
        },
        DefKind::Variant => "", // display name built below using parent_type
        DefKind::Namespace => return None,
        DefKind::Class => "class",
        DefKind::Given => "given",
    };

    let name = doc.interner.resolve(def.name);
    // For variants, qualify with parent type: `Shape.Circle` instead of just `Circle`.
    let display_name: String = if def.kind == DefKind::Variant {
        if let Some(parent) = def.parent_type {
            format!("{}.{name}", doc.interner.resolve(parent))
        } else {
            name.to_owned()
        }
    } else {
        name.to_owned()
    };
    let signature = if kind_kw.is_empty() {
        format!("{display_name}: {ty_str}")
    } else {
        format!("{kind_kw} {display_name}: {ty_str}")
    };

    let local_doc = extract_doc_comments_from_source(
        def.span.start,
        &doc.source,
        &doc.lexed.tokens,
        &doc.lexed.trivia,
    );
    let doc_text = if local_doc.is_empty() {
        // Imported symbol — search dep sources by interned name.
        doc.dep_sources
            .values()
            .find_map(|dep| {
                dep.def_spans.get(&def.name).and_then(|&span| {
                    let text = extract_doc_comments_from_source(
                        span.start,
                        &dep.source,
                        &dep.tokens,
                        &dep.trivia,
                    );
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

    // Highlight the identifier token under the cursor, falling back to the def's name span.
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

/// Format a [`Type`] using Musi source syntax.
pub fn fmt_type(ty: &Type, doc: &AnalyzedDoc, sema: &musi_sema::SemaResult) -> String {
    match ty {
        Type::Prim(p) => p.name().to_owned(),
        Type::Var(_) | Type::Error => "'_".to_owned(),
        Type::Tuple(elems) => {
            let inner = elems
                .iter()
                .map(|e| fmt_type(e, doc, sema))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({inner})")
        }
        Type::Array(elem, size) => {
            let elem_s = fmt_type(elem, doc, sema);
            match size {
                None => format!("[]{elem_s}"),
                Some(n) => format!("[{n}]{elem_s}"),
            }
        }
        Type::Arrow(params, ret) => {
            let ret_s = fmt_type(ret, doc, sema);
            if params.len() == 1 {
                let p_s = fmt_type(&params[0], doc, sema);
                format!("{p_s} -> {ret_s}")
            } else {
                let ps = params
                    .iter()
                    .map(|p| fmt_type(p, doc, sema))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("({ps}) -> {ret_s}")
            }
        }
        Type::Named(id, args) => {
            let name = sema
                .defs
                .get(id.0 as usize)
                .map(|d| doc.interner.resolve(d.name).to_owned())
                .unwrap_or_else(|| "?type".to_owned());
            if args.is_empty() {
                name
            } else {
                let as_ = args
                    .iter()
                    .map(|a| fmt_type(a, doc, sema))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}[{as_}]")
            }
        }
    }
}
