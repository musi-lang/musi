//! Document symbols provider: returns all top-level definitions for outline / go-to-symbol.

use std::collections::HashMap;

use lsp_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind};
use music_sema::{DefId, DefKind, Type};
use music_shared::Span;

use crate::analysis::{AnalyzedDoc, def_name_span};
use crate::to_proto::{fmt_type_lsp, span_to_range};

/// Produce the outline symbols for a document.
pub fn document_symbols(doc: &AnalyzedDoc) -> DocumentSymbolResponse {
    let Some(sema) = &doc.sema else {
        return DocumentSymbolResponse::Nested(vec![]);
    };

    let symbols_with_id: Vec<(DefId, Option<DefId>, DocumentSymbol)> = sema
        .defs
        .iter()
        .filter(|def| {
            def.span != Span::DUMMY
                && !matches!(def.kind, DefKind::Param | DefKind::Var | DefKind::Import)
                && doc
                    .interner
                    .try_resolve(def.name)
                    .is_some_and(|n| !n.starts_with('_'))
        })
        .filter_map(|def| {
            let kind = match def.kind {
                DefKind::Fn | DefKind::ForeignFn | DefKind::EffectOp => SymbolKind::FUNCTION,
                DefKind::Let => {
                    let is_fn = def
                        .ty_info
                        .ty
                        .map(|t| matches!(&sema.types[t], Type::Fn { .. }))
                        .unwrap_or(false);
                    if is_fn {
                        SymbolKind::FUNCTION
                    } else {
                        SymbolKind::CONSTANT
                    }
                }
                DefKind::Type | DefKind::OpaqueType => SymbolKind::CLASS,
                DefKind::Variant => SymbolKind::ENUM_MEMBER,
                DefKind::Class | DefKind::Effect => SymbolKind::INTERFACE,
                DefKind::Instance => SymbolKind::MODULE,
                DefKind::Law => SymbolKind::PROPERTY,
                _ => return None,
            };

            let name = doc.interner.try_resolve(def.name)?.to_owned();
            let detail = def.ty_info.ty.map(|ty| fmt_type_lsp(ty, doc, sema));

            let name_span = def_name_span(def, &doc.lexed.tokens);
            let range = span_to_range(doc.file_id, def.span, &doc.source_db);
            let selection_range = span_to_range(doc.file_id, name_span, &doc.source_db);

            // In multi-file mode, defs from imported modules have spans from
            // other files. Skip any def whose selection_range falls outside
            // its full range (LSP protocol requires containment).
            if selection_range.start < range.start || selection_range.end > range.end {
                return None;
            }

            #[allow(deprecated)]
            let sym = DocumentSymbol {
                name,
                detail,
                kind,
                tags: None,
                deprecated: None,
                range,
                selection_range,
                children: None,
            };
            Some((def.id, def.parent, sym))
        })
        .collect();

    let mut children_map: HashMap<DefId, Vec<DocumentSymbol>> = HashMap::new();
    let mut top_level: Vec<(DefId, DocumentSymbol)> = vec![];

    for (id, parent, sym) in symbols_with_id {
        if let Some(pid) = parent {
            children_map.entry(pid).or_default().push(sym);
        } else {
            top_level.push((id, sym));
        }
    }

    for (id, sym) in &mut top_level {
        if let Some(mut children) = children_map.remove(id) {
            children.sort_by_key(|s| (s.range.start.line, s.range.start.character));
            sym.children = Some(children);
        }
    }

    top_level.sort_by_key(|(_, s)| (s.range.start.line, s.range.start.character));

    DocumentSymbolResponse::Nested(top_level.into_iter().map(|(_, s)| s).collect())
}
