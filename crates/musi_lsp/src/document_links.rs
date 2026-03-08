//! Document link provider: makes import/export path strings Ctrl+Click navigable.

use std::str::FromStr;

use musi_lex::TokenKind;
use musi_parse::ast::Expr;
use tower_lsp_server::ls_types::{DocumentLink, Range, Uri};

use crate::analysis::{AnalyzedDoc, span_to_range};

/// Compute document links for all import/export path strings in `doc`.
///
/// Each import path string becomes a clickable link to the resolved `.ms` file.
/// Relative imports (`./` or `../`) are resolved against the document's own directory;
/// non-relative imports are resolved against the workspace root.
pub fn document_links(
    doc: &AnalyzedDoc,
    doc_uri: &Uri,
    root_uri: Option<&Uri>,
) -> Vec<DocumentLink> {
    // Base URI for resolving non-relative imports (workspace root).
    let root_base: String = root_uri.map_or_else(String::new, |root| {
        let s = root.as_str();
        if s.ends_with('/') {
            s.to_owned()
        } else {
            format!("{s}/")
        }
    });

    // Base URI for resolving relative imports: document's directory.
    let doc_dir: String = {
        let s = doc_uri.as_str();
        if let Some(pos) = s.rfind('/') {
            format!("{}/", &s[..pos])
        } else {
            root_base.clone()
        }
    };

    let mut links = Vec::new();

    for &item_idx in doc.module.ctx.expr_lists.get_slice(doc.module.items) {
        let (path_sym, _expr_span) = match doc.module.ctx.exprs.get(item_idx) {
            Expr::Import { path, span, .. } | Expr::Export { path, span, .. } => (*path, *span),
            _ => continue,
        };

        let Some(raw) = doc.interner.try_resolve(path_sym) else {
            continue;
        };
        // Path symbol includes surrounding quotes; strip them.
        let raw = raw.trim_matches('"');
        // Skip native module paths (e.g. `musi:math`).
        if raw.contains(':') {
            continue;
        }

        let module_key = raw.strip_suffix(".ms").unwrap_or(raw);

        // Resolve relative vs absolute import paths against the correct base.
        let base = if module_key.starts_with("./") || module_key.starts_with("../") {
            &doc_dir
        } else {
            &root_base
        };
        if base.is_empty() {
            continue;
        }

        let target_str = format!("{base}{module_key}.ms");
        let Ok(target) = Uri::from_str(&target_str) else {
            continue;
        };

        // Find the string token span (includes quotes).
        let Some(link_range) = find_string_token_range(doc, path_sym) else {
            continue;
        };

        links.push(DocumentLink {
            range: link_range,
            target: Some(target),
            tooltip: Some(format!("{module_key}.ms")),
            data: None,
        });
    }

    links
}

/// Find the StringLit token whose content matches `path_sym` and return its LSP range.
fn find_string_token_range(doc: &AnalyzedDoc, path_sym: musi_shared::Symbol) -> Option<Range> {
    let expected = doc.interner.try_resolve(path_sym)?;
    for tok in &doc.lexed.tokens {
        if tok.kind == TokenKind::StringLit {
            let src = doc
                .source
                .get(tok.span.start as usize..(tok.span.start + tok.span.length) as usize)?;
            if src == expected {
                return Some(span_to_range(doc.file_id, tok.span, &doc.source_db));
            }
        }
    }
    None
}
