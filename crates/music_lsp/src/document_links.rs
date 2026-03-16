//! Document link provider: makes import/export path strings Ctrl+Click navigable.

use lsp_types::{DocumentLink, Range, Url};
use music_ast::{AstArenas, Expr, ExprIdx, Stmt};
use music_lex::TokenKind;
use music_shared::Symbol;

use crate::analysis::{AnalyzedDoc, span_to_range};

/// Compute document links for all import/export path strings in `doc`.
pub fn document_links(doc: &AnalyzedDoc, doc_uri: &Url) -> Vec<DocumentLink> {
    let _ = doc_uri; // reserved for future single-file fallback
    let mut links = vec![];

    for path_sym in collect_import_paths(&doc.module.arenas, &doc.module.stmts) {
        let Some(resolved_path) = doc.resolved_imports.get(&path_sym) else {
            continue;
        };

        let Ok(target) = Url::from_file_path(resolved_path) else {
            continue;
        };

        let Some(link_range) = find_string_token_range(doc, path_sym) else {
            continue;
        };

        let tooltip = resolved_path
            .file_name()
            .map(|f| f.to_string_lossy().into_owned())
            .unwrap_or_default();

        links.push(DocumentLink {
            range: link_range,
            target: Some(target),
            tooltip: Some(tooltip),
            data: None,
        });
    }

    links
}

/// Collect all import path symbols from top-level statements, walking through
/// `Binding`, `Let`, `Annotated`, and `Export` wrappers.
fn collect_import_paths(arenas: &AstArenas, stmts: &[Stmt]) -> Vec<Symbol> {
    let mut paths = vec![];
    for stmt in stmts {
        collect_from_expr(arenas, stmt.expr, &mut paths);
    }
    paths
}

fn collect_from_expr(arenas: &AstArenas, idx: ExprIdx, out: &mut Vec<Symbol>) {
    match &arenas.exprs[idx] {
        Expr::Import { path, .. } => out.push(*path),
        Expr::Export {
            source: Some(source),
            ..
        } => out.push(*source),
        Expr::Annotated { inner, .. } => collect_from_expr(arenas, *inner, out),
        Expr::Binding { fields, .. } | Expr::Let { fields, .. } => {
            if let Some(val) = fields.value {
                collect_from_expr(arenas, val, out);
            }
        }
        _ => {}
    }
}

fn find_string_token_range(doc: &AnalyzedDoc, path_sym: Symbol) -> Option<Range> {
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
