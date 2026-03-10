//! Document symbols provider: returns all top-level definitions for outline / go-to-symbol.

use music_sema::{DefKind, Type};
use music_shared::Span;
use tower_lsp_server::ls_types::{DocumentSymbol, DocumentSymbolResponse, SymbolKind};

use crate::analysis::{AnalyzedDoc, def_name_span, span_to_range};
use crate::hover::fmt_type_lsp;

/// Produce the outline symbols for a document.
pub fn document_symbols(doc: &AnalyzedDoc) -> DocumentSymbolResponse {
    let Some(sema) = &doc.sema else {
        return DocumentSymbolResponse::Nested(vec![]);
    };

    let mut symbols: Vec<DocumentSymbol> = sema
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
                DefKind::Given => SymbolKind::MODULE,
                _ => return None,
            };

            let name = doc.interner.try_resolve(def.name)?.to_owned();
            let detail = def.ty_info.ty.map(|ty| fmt_type_lsp(ty, doc, sema));

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

    symbols.sort_by_key(|s| (s.range.start.line, s.range.start.character));

    DocumentSymbolResponse::Nested(symbols)
}
