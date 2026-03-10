//! Document link provider: makes import/export path strings Ctrl+Click navigable.

use std::str::FromStr;

use music_ast::Expr;
use music_lex::TokenKind;
use tower_lsp_server::ls_types::{DocumentLink, Range, Uri};

use crate::analysis::{AnalyzedDoc, span_to_range};

/// Compute document links for all import/export path strings in `doc`.
pub fn document_links(
    doc: &AnalyzedDoc,
    doc_uri: &Uri,
    root_uri: Option<&Uri>,
) -> Vec<DocumentLink> {
    let root_base: String = root_uri.map_or_else(String::new, |root| {
        let s = root.as_str();
        if s.ends_with('/') {
            s.to_owned()
        } else {
            format!("{s}/")
        }
    });

    let doc_dir: String = {
        let s = doc_uri.as_str();
        if let Some(pos) = s.rfind('/') {
            format!("{}/", &s[..pos])
        } else {
            root_base.clone()
        }
    };

    let mut links = Vec::new();

    for stmt in &doc.module.stmts {
        let path_sym = match &doc.module.arenas.exprs[stmt.expr] {
            Expr::Import { path, .. } => *path,
            Expr::Export {
                source: Some(source),
                ..
            } => *source,
            _ => continue,
        };

        let Some(raw) = doc.interner.try_resolve(path_sym) else {
            continue;
        };
        let raw = raw.trim_matches('"');
        if raw.contains(':') {
            continue;
        }

        let module_key = raw.strip_suffix(".ms").unwrap_or(raw);

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

fn find_string_token_range(doc: &AnalyzedDoc, path_sym: music_shared::Symbol) -> Option<Range> {
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
