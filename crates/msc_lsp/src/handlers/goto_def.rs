//! Go-to-definition provider (single-file + stdlib).

use lsp_types::{GotoDefinitionResponse, Location, LocationLink, Position, Range, Url};
use msc_ast::Expr;
use msc_lex::TokenKind;
use msc_sema::{DefKind, SemaResult};
use msc_shared::{Idx, Span, Symbol};

use crate::analysis::{AnalyzedDoc, def_at_cursor, def_at_offset, field_at_cursor};
use crate::to_proto::{position_to_offset, span_to_range, span_to_range_raw};

/// Return the definition location for the symbol under the cursor.
pub fn goto_definition(
    doc: &AnalyzedDoc,
    position: Position,
    uri: &Url,
    root_uri: Option<&Url>,
) -> Option<GotoDefinitionResponse> {
    let offset = position_to_offset(&doc.source, position.line, position.character);

    if let Some(resp) = import_at_offset(doc, offset) {
        return Some(resp);
    }

    let sema = doc.sema.as_ref()?;

    let def = match def_at_offset(offset, doc).or_else(|| def_at_cursor(offset, doc)) {
        Some(d) => d,
        None => return goto_field(doc, sema, offset, uri, root_uri),
    };

    if def.span != Span::DUMMY {
        if def.file_id != doc.file_id {
            // Definition lives in a dependency file — try cross-file navigation.
            if let Some(resp) = resolve_stdlib_def(doc, def.name, def.span, root_uri) {
                return Some(resp);
            }
        }
        let range = span_to_range(doc.file_id, def.span, &doc.source_db);
        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: uri.clone(),
            range,
        }));
    }

    resolve_stdlib_def(doc, def.name, def.span, root_uri)
}

fn resolve_stdlib_def(
    doc: &AnalyzedDoc,
    name: Symbol,
    span_hint: Span,
    root_uri: Option<&Url>,
) -> Option<GotoDefinitionResponse> {
    let root_uri = root_uri?;

    for (mod_key, dep_src) in &doc.dep_sources {
        let def_span = if span_hint != Span::DUMMY {
            // Use the span from DefInfo directly when available.
            if dep_src.def_spans.contains_key(&name) {
                span_hint
            } else {
                continue;
            }
        } else {
            let Some(&s) = dep_src.def_spans.get(&name) else {
                continue;
            };
            if s == Span::DUMMY {
                continue;
            }
            s
        };

        let rel_path = if mod_key == "<prelude>" {
            "std/prelude.ms".to_owned()
        } else {
            format!("{mod_key}.ms")
        };

        let root_str = root_uri.as_str().trim_end_matches('/');
        let Ok(file_uri) = format!("{root_str}/{rel_path}").parse::<Url>() else {
            continue;
        };

        let range = span_to_range_raw(def_span, &dep_src.source);

        return Some(GotoDefinitionResponse::Scalar(Location {
            uri: file_uri,
            range,
        }));
    }

    None
}

fn goto_field(
    doc: &AnalyzedDoc,
    sema: &SemaResult,
    offset: u32,
    uri: &Url,
    root_uri: Option<&Url>,
) -> Option<GotoDefinitionResponse> {
    let (expr_idx, field_name, _field_span) = field_at_cursor(offset, doc)?;

    let object_idx = match &doc.module.arenas.exprs[expr_idx] {
        Expr::Field { object, .. } => *object,
        _ => return None,
    };

    // Check variant constructors first (e.g. `.Failed()`), before the alias
    // early-return which would otherwise short-circuit and miss the variant.
    if let Some(obj_ty) = sema.expr_types.get(&object_idx).copied() {
        let resolved = sema.unify.resolve(obj_ty, &sema.types);
        if let msc_sema::Type::Named { def, .. } = &sema.types[resolved] {
            if let Some(variant_def) = sema.defs.iter().find(|d| {
                d.kind == DefKind::Variant && d.parent == Some(*def) && d.name == field_name
            }) {
                if variant_def.span != Span::DUMMY {
                    if variant_def.file_id != doc.file_id {
                        return resolve_stdlib_def(
                            doc,
                            variant_def.name,
                            variant_def.span,
                            root_uri,
                        );
                    }
                    let range = span_to_range(doc.file_id, variant_def.span, &doc.source_db);
                    return Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range,
                    }));
                }
            }

            let type_def = sema.defs.get(def.0 as usize);
            if let Some(type_def) = type_def {
                if type_def.span != Span::DUMMY {
                    if type_def.file_id != doc.file_id {
                        return resolve_stdlib_def(doc, type_def.name, type_def.span, root_uri);
                    }
                    let range = span_to_range(doc.file_id, type_def.span, &doc.source_db);
                    return Some(GotoDefinitionResponse::Scalar(Location {
                        uri: uri.clone(),
                        range,
                    }));
                }
            }
        }
    }

    // Import alias field access (e.g. `t.run_suite`).
    if let Some(&alias_def_id) = sema.resolution.expr_defs.get(&object_idx) {
        if let Some(resp) = resolve_stdlib_def(doc, field_name, Span::DUMMY, root_uri) {
            return Some(resp);
        }
        let alias_def = sema.defs.get(alias_def_id.0 as usize)?;
        if alias_def.file_id == doc.file_id && alias_def.span != Span::DUMMY {
            let range = span_to_range(doc.file_id, alias_def.span, &doc.source_db);
            return Some(GotoDefinitionResponse::Scalar(Location {
                uri: uri.clone(),
                range,
            }));
        }
    }

    None
}

/// If the cursor is on an import expression (either the `import` keyword or the
/// string literal path), return a goto-definition response pointing to the
/// resolved file.
fn import_at_offset(doc: &AnalyzedDoc, offset: u32) -> Option<GotoDefinitionResponse> {
    // Path 1: cursor directly on the StringLit token.
    if let Some(tok) = doc.lexed.tokens.iter().find(|t| {
        t.kind == TokenKind::StringLit && t.span.start <= offset && offset <= t.span.end()
    }) {
        let raw = doc
            .source
            .get(tok.span.start as usize..tok.span.end() as usize)?;
        // Strip surrounding quotes — the interner stores content only.
        let content = raw
            .strip_prefix('"')
            .and_then(|s| s.strip_suffix('"'))
            .unwrap_or(raw);
        if let Some(resp) = resolve_import_path(doc, content, tok.span) {
            return Some(resp);
        }
    }

    // Path 2: cursor on the `import` keyword — find the Expr::Import node.
    let _kw = doc.lexed.tokens.iter().find(|t| {
        t.kind == TokenKind::KwImport && t.span.start <= offset && offset <= t.span.end()
    })?;

    let arenas = &doc.module.arenas;
    for raw_idx in 0..arenas.exprs.len() {
        let Some(idx) = u32::try_from(raw_idx).ok().map(Idx::from_raw) else {
            continue;
        };
        if let Expr::Import { path, span, .. } = &arenas.exprs[idx] {
            if span.start <= offset && offset <= span.end() {
                let path_str = doc.interner.try_resolve(*path)?;
                // Find the StringLit token within the import expression for a
                // tight origin_selection_range (not the entire import statement).
                let string_span = doc
                    .lexed
                    .tokens
                    .iter()
                    .find(|t| {
                        t.kind == TokenKind::StringLit
                            && t.span.start >= span.start
                            && t.span.end() <= span.end()
                    })
                    .map(|t| t.span)
                    .unwrap_or(*span);
                return resolve_import_path(doc, path_str, string_span);
            }
        }
    }
    None
}

/// Look up `path_str` (e.g. `@std/testing`) in `resolved_imports` and return a
/// `LocationLink` if found.
fn resolve_import_path(
    doc: &AnalyzedDoc,
    path_str: &str,
    origin_span: Span,
) -> Option<GotoDefinitionResponse> {
    let resolved_path = doc
        .resolved_imports
        .iter()
        .find(|(sym, _)| doc.interner.try_resolve(**sym) == Some(path_str))
        .map(|(_, path)| path)?;

    let origin_range = span_to_range(doc.file_id, origin_span, &doc.source_db);
    let target_uri = Url::from_file_path(resolved_path).ok()?;

    Some(GotoDefinitionResponse::Link(vec![LocationLink {
        origin_selection_range: Some(origin_range),
        target_uri,
        target_range: Range::default(),
        target_selection_range: Range::default(),
    }]))
}
