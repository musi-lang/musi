use music_base::{SourceId, Span};
use music_syntax::{SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree, TokenKind};

use crate::ModuleSpecifier;
use crate::string_lit::decode_string_lit;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportSiteKind {
    Static { spec: ModuleSpecifier },
    Dynamic,
    InvalidStringLit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportSite {
    pub source_id: SourceId,
    pub span: Span,
    pub kind: ImportSiteKind,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ModuleExportSummary {
    exports: Vec<Box<str>>,
    opaque: Vec<Box<str>>,
}

impl ModuleExportSummary {
    pub fn exports(&self) -> impl Iterator<Item = &str> {
        self.exports.iter().map(Box::as_ref)
    }

    #[must_use]
    pub fn is_export_opaque(&self, name: &str) -> bool {
        self.opaque.iter().any(|it| it.as_ref() == name)
    }

    fn push_export(&mut self, name: &str, is_opaque: bool) {
        if self.exports.iter().any(|it| it.as_ref() == name) {
            return;
        }
        let boxed: Box<str> = name.into();
        self.exports.push(boxed.clone());
        if is_opaque {
            self.opaque.push(boxed);
        }
    }
}

#[must_use]
pub fn collect_import_sites(source_id: SourceId, tree: &SyntaxTree<'_>) -> Vec<ImportSite> {
    let mut out = Vec::new();
    walk_nodes(tree.root(), &mut |node| {
        if node.kind() != SyntaxNodeKind::ImportExpr {
            return;
        }
        let span = node.span();
        let kind = classify_import_expr(node);
        out.push(ImportSite {
            source_id,
            span,
            kind,
        });
    });
    out
}

#[must_use]
pub fn collect_export_summary(_source_id: SourceId, tree: &SyntaxTree<'_>) -> ModuleExportSummary {
    let mut summary = ModuleExportSummary::default();
    walk_nodes(tree.root(), &mut |node| {
        if node.kind() != SyntaxNodeKind::ExportExpr {
            return;
        }
        let is_opaque = node
            .child_tokens()
            .any(|tok| tok.kind() == TokenKind::KwOpaque);
        let Some(let_expr) = node
            .child_nodes()
            .find(|child| child.kind() == SyntaxNodeKind::LetExpr)
        else {
            return;
        };
        let Some(pat) = let_expr.child_nodes().find(|n| n.kind().is_pat()) else {
            return;
        };
        let mut binders = Vec::<Box<str>>::new();
        collect_binders_in_pattern(pat, &mut binders);
        for name in binders {
            summary.push_export(name.as_ref(), is_opaque);
        }
    });
    summary
}

fn classify_import_expr(node: SyntaxNode<'_, '_>) -> ImportSiteKind {
    let mut children = node.child_nodes();
    let Some(expr) = children.next() else {
        return ImportSiteKind::Dynamic;
    };
    if expr.kind() != SyntaxNodeKind::LiteralExpr {
        return ImportSiteKind::Dynamic;
    }
    let Some(tok) = expr.child_tokens().next() else {
        return ImportSiteKind::Dynamic;
    };
    if tok.kind() != TokenKind::String {
        return ImportSiteKind::Dynamic;
    }
    let Some(raw) = tok.text() else {
        return ImportSiteKind::InvalidStringLit;
    };
    let Ok(decoded) = decode_string_lit(raw) else {
        return ImportSiteKind::InvalidStringLit;
    };
    ImportSiteKind::Static {
        spec: ModuleSpecifier::new(decoded),
    }
}

fn walk_nodes<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
    f: &mut impl FnMut(SyntaxNode<'tree, 'src>),
) {
    f(node);
    for child in node.children() {
        if let Some(node) = child.into_node() {
            walk_nodes(node, f);
        }
    }
}

fn collect_binders_in_pattern(pat: SyntaxNode<'_, '_>, out: &mut Vec<Box<str>>) {
    match pat.kind() {
        SyntaxNodeKind::BindPat => {
            if let Some(name) = first_ident_text(pat) {
                out.push(name.into());
            }
        }
        SyntaxNodeKind::AsPat => {
            let mut nodes = pat.child_nodes();
            if let Some(inner) = nodes.next() {
                collect_binders_in_pattern(inner, out);
            }
            if let Some(alias) = pat
                .child_tokens()
                .find(|tok| tok.kind() == TokenKind::Ident)
                .and_then(SyntaxToken::text)
            {
                out.push(alias.into());
            }
        }
        SyntaxNodeKind::OrPat => {
            for child in pat.child_nodes() {
                if child.kind().is_pat() {
                    collect_binders_in_pattern(child, out);
                }
            }
        }
        SyntaxNodeKind::RecordPat => collect_record_pat_binders(pat, out),
        kind if kind.is_pat() => {
            for child in pat.child_nodes() {
                if child.kind().is_pat() {
                    collect_binders_in_pattern(child, out);
                }
            }
        }
        _ => {}
    }
}

fn collect_record_pat_binders(pat: SyntaxNode<'_, '_>, out: &mut Vec<Box<str>>) {
    let children: Vec<SyntaxElement<'_, '_>> = pat.children().collect();
    let mut i: usize = 0;
    while let Some(elem) = children.get(i).copied() {
        let Some(tok) = elem.into_token() else {
            i += 1;
            continue;
        };
        if tok.kind() != TokenKind::Ident {
            i += 1;
            continue;
        }
        let name = tok.text();
        let is_colon = children
            .get(i + 1)
            .copied()
            .and_then(SyntaxElement::into_token)
            .is_some_and(|t| t.kind() == TokenKind::Colon);
        if is_colon {
            let Some(value_pat) = children
                .get(i + 2)
                .copied()
                .and_then(SyntaxElement::into_node)
            else {
                i += 1;
                continue;
            };
            collect_binders_in_pattern(value_pat, out);
            i += 3;
            continue;
        }
        if let Some(name) = name {
            out.push(name.into());
        }
        i += 1;
    }
}

fn first_ident_text<'src>(node: SyntaxNode<'_, 'src>) -> Option<&'src str> {
    node.child_tokens()
        .find(|tok| tok.kind() == TokenKind::Ident)
        .and_then(SyntaxToken::text)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
