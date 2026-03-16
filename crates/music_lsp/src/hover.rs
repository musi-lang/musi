//! Hover provider: shows the type and doc-comment of the symbol under the cursor.

use lsp_types::{Hover, HoverContents, MarkupContent, MarkupKind, Position};
use music_ast::decl::ClassMember;
use music_ast::expr::{BindKind, LetFields, Param, ParamMode};
use music_ast::pat::Pat;
use music_ast::{AstArenas, Expr, PatIdx};
use music_sema::def::DefInfo;
use music_sema::types::{self, Type, TypeIdx};
use music_sema::{DefKind, SemaResult};
use music_shared::Idx;

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
        DefKind::Var => "let mut",
        DefKind::Param => "",
        DefKind::Type => "type",
        DefKind::Variant => "",
        DefKind::Class => "class",
        DefKind::Given => "given",
        DefKind::Effect => "effect",
        DefKind::ForeignFn => "foreign let",
        DefKind::EffectOp => "let",
        DefKind::Law => "law",
        DefKind::Import | DefKind::OpaqueType | DefKind::LawVar => "",
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

    let signature = if let Some(fn_sig) = build_fn_signature(doc, sema, def, kind_kw, &display_name)
    {
        fn_sig
    } else if show_type {
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

/// Build a rich function signature with parameter names and types.
///
/// Returns `None` for non-function definitions, falling back to the default format.
fn build_fn_signature(
    doc: &AnalyzedDoc,
    sema: &SemaResult,
    def: &DefInfo,
    kind_kw: &str,
    display_name: &str,
) -> Option<String> {
    let ty = def.ty_info.ty?;
    let (param_tys, ret_ty) = match &sema.types[ty] {
        Type::Fn { params, ret, .. } => (params.clone(), Some(*ret)),
        _ => return None,
    };

    // Try to find AST params with names for this definition.
    if let Some(params_str) = find_ast_params(doc, sema, def, &param_tys) {
        let ret_str = format_ret_type(ret_ty, doc, sema);
        let prefix = if kind_kw.is_empty() {
            String::new()
        } else {
            format!("{kind_kw} ")
        };
        Some(format!("{prefix}{display_name}({params_str}){ret_str}"))
    } else {
        // ForeignFn / EffectOp: no AST params, use type-only format.
        if matches!(def.kind, DefKind::ForeignFn | DefKind::EffectOp) {
            let params_str = param_tys
                .iter()
                .map(|&t| fmt_type_lsp(t, doc, sema))
                .collect::<Vec<_>>()
                .join(", ");
            let ret_str = format_ret_type(ret_ty, doc, sema);
            let prefix = if kind_kw.is_empty() {
                String::new()
            } else {
                format!("{kind_kw} ")
            };
            Some(format!("{prefix}{display_name}({params_str}){ret_str}"))
        } else {
            None
        }
    }
}

/// Format return type as `: T` suffix, omitting unit `()`.
fn format_ret_type(ret_ty: Option<TypeIdx>, doc: &AnalyzedDoc, sema: &SemaResult) -> String {
    let Some(ret) = ret_ty else {
        return String::new();
    };
    match &sema.types[ret] {
        Type::Tuple { elems } if elems.is_empty() => String::new(),
        _ => format!(": {}", fmt_type_lsp(ret, doc, sema)),
    }
}

/// Search the AST for parameter names matching a definition site.
fn find_ast_params(
    doc: &AnalyzedDoc,
    sema: &SemaResult,
    def: &DefInfo,
    param_tys: &[TypeIdx],
) -> Option<String> {
    let arenas = &doc.module.arenas;

    // Path 1: `let f(x, y) := ...` — Pat::Variant in a binding
    for raw_idx in 0..arenas.pats.len() {
        let idx = Idx::from_raw(u32::try_from(raw_idx).ok()?);
        if let Pat::Variant { name, args, span } = &arenas.pats[idx] {
            if *span == def.span {
                return Some(format_pat_params(doc, sema, args, param_tys));
            }
            let _ = name;
        }
    }

    // Path 2: `fn(x, y) => ...` — Expr::Fn where a param span matches
    for raw_idx in 0..arenas.exprs.len() {
        let idx = Idx::from_raw(u32::try_from(raw_idx).ok()?);
        if let Expr::Fn { params, span, .. } = &arenas.exprs[idx]
            && *span == def.span
        {
            return Some(format_expr_params(doc, sema, params, param_tys));
        }
    }

    // Path 3: Expr::Let / Expr::Binding with fn-typed pat
    for raw_idx in 0..arenas.exprs.len() {
        let idx = Idx::from_raw(u32::try_from(raw_idx).ok()?);
        let fields = match &arenas.exprs[idx] {
            Expr::Let { fields, .. } | Expr::Binding { fields, .. } => fields,
            _ => continue,
        };
        if let Some(params_str) = check_let_fields(doc, sema, def, fields, arenas, param_tys) {
            return Some(params_str);
        }
    }

    // Path 4: class/instance members
    for raw_idx in 0..arenas.exprs.len() {
        let idx = Idx::from_raw(u32::try_from(raw_idx).ok()?);
        let members = match &arenas.exprs[idx] {
            Expr::Class { members, .. } | Expr::Instance { members, .. } => members,
            _ => continue,
        };
        for member in members {
            if let ClassMember::Fn { sig, .. } = member
                && sig.span == def.span
            {
                return Some(format_expr_params(doc, sema, &sig.params, param_tys));
            }
        }
    }

    None
}

/// Check if a `LetFields` contains a `Pat::Variant` matching the def.
fn check_let_fields(
    doc: &AnalyzedDoc,
    sema: &SemaResult,
    def: &DefInfo,
    fields: &LetFields,
    arenas: &AstArenas,
    param_tys: &[TypeIdx],
) -> Option<String> {
    let pat = &arenas.pats[fields.pat];
    if let Pat::Variant { args, span, .. } = pat
        && *span == def.span
    {
        return Some(format_pat_params(doc, sema, args, param_tys));
    }
    None
}

/// Format params from `Pat::Variant` args (binding patterns).
fn format_pat_params(
    doc: &AnalyzedDoc,
    sema: &SemaResult,
    args: &[PatIdx],
    param_tys: &[TypeIdx],
) -> String {
    args.iter()
        .enumerate()
        .map(|(i, &pat_idx)| {
            let pat = &doc.module.arenas.pats[pat_idx];
            let name = match pat {
                Pat::Bind { name, .. } => doc.interner.try_resolve(*name).unwrap_or("_"),
                _ => "_",
            };
            let mut_prefix = match pat {
                Pat::Bind { kind, .. } if *kind == BindKind::Mut => "mut ",
                _ => "",
            };
            let ty_str = param_tys
                .get(i)
                .map(|&t| fmt_type_lsp(t, doc, sema))
                .unwrap_or_else(|| "?".to_owned());
            format!("{mut_prefix}{name}: {ty_str}")
        })
        .collect::<Vec<_>>()
        .join(", ")
}

/// Format params from `Param` list (Expr::Fn / ClassMember::Fn).
fn format_expr_params(
    doc: &AnalyzedDoc,
    sema: &SemaResult,
    params: &[Param],
    param_tys: &[TypeIdx],
) -> String {
    params
        .iter()
        .enumerate()
        .map(|(i, p)| {
            let name = doc.interner.try_resolve(p.name).unwrap_or("_");
            let mut_prefix = if p.mode == ParamMode::Mut { "mut " } else { "" };
            let ty_str = param_tys
                .get(i)
                .map(|&t| fmt_type_lsp(t, doc, sema))
                .unwrap_or_else(|| "?".to_owned());
            format!("{mut_prefix}{name}: {ty_str}")
        })
        .collect::<Vec<_>>()
        .join(", ")
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
pub fn fmt_type_lsp(ty: TypeIdx, doc: &AnalyzedDoc, sema: &SemaResult) -> String {
    types::fmt_type(
        ty,
        &sema.types,
        &sema.defs,
        &doc.interner,
        Some(&sema.unify),
    )
    .to_string()
}
