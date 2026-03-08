//! Document symbols provider: returns all top-level definitions for outline / go-to-symbol.

use musi_sema::{DefKind, Type};
use musi_shared::Span;
use tower_lsp_server::ls_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind};

use crate::analysis::{AnalyzedDoc, def_name_span, span_to_range};
use crate::hover::fmt_type;

/// Produce the outline symbols for a document.
pub fn document_symbols(doc: &AnalyzedDoc) -> DocumentSymbolResponse {
    let Some(sema) = &doc.sema else {
        return DocumentSymbolResponse::Nested(vec![]);
    };

    let mut symbols: Vec<DocumentSymbol> = sema
        .defs
        .iter()
        .filter(|def| {
            // Only defs with a real source location (not prelude/imports)
            def.span != Span::DUMMY
                // Params, vars, and namespaces are too noisy for the outline
                && !matches!(def.kind, DefKind::Param | DefKind::Var | DefKind::Namespace)
                // Skip `_`-prefixed names
                && !doc.interner.resolve(def.name).starts_with('_')
        })
        .filter_map(|def| {
            let kind = match def.kind {
                DefKind::Fn => SymbolKind::FUNCTION,
                DefKind::Const => {
                    let is_fn = def
                        .ty
                        .as_ref()
                        .map(|t| {
                            matches!(sema.unify_table.resolve(t.clone()), Type::Arrow(..))
                        })
                        .unwrap_or(false);
                    if is_fn { SymbolKind::FUNCTION } else { SymbolKind::CONSTANT }
                }
                DefKind::Type => SymbolKind::CLASS,
                DefKind::Variant => SymbolKind::ENUM_MEMBER,
                // Param / Var / Namespace handled by the filter above
                _ => return None,
            };

            let name = doc.interner.resolve(def.name).to_owned();
            let detail = def.ty.as_ref().map(|ty| {
                fmt_type(&sema.unify_table.resolve(ty.clone()), doc, sema)
            });

            let name_span = def_name_span(def, &doc.lexed.tokens);
            let range = span_to_range(doc.file_id, def.span, &doc.source_db);
            let selection_range = span_to_range(doc.file_id, name_span, &doc.source_db);

            #[allow(deprecated)]
            Some(DocumentSymbol {
                name,
                detail,
                kind,
                tags: None,
                deprecated: None,
                range,
                selection_range,
                children: None,
            })
        })
        .collect();

    // Sort by line so the outline matches source order.
    symbols.sort_by_key(|s| (s.range.start.line, s.range.start.character));

    DocumentSymbolResponse::Nested(symbols)
}
