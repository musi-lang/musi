use music_base::{SourceId, Span};
use music_syntax::{
    SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree, TokenKind, canonical_name_text,
    pattern_binder_tokens,
};

use crate::ModuleSpecifier;
use crate::string_lit::decode_string_lit;

type ExportNameList = Vec<Box<str>>;
type ExportedGivenSiteList = Vec<ExportedGivenSite>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportSiteKind {
    Static { spec: ModuleSpecifier },
    NonLiteral,
    InvalidStringLit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportSite {
    pub source_id: SourceId,
    pub span: Span,
    pub kind: ImportSiteKind,
}

impl ImportSite {
    #[must_use]
    pub const fn new(source_id: SourceId, span: Span, kind: ImportSiteKind) -> Self {
        Self {
            source_id,
            span,
            kind,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ExportedGivenSite {
    pub source_id: SourceId,
    pub span: Span,
}

impl ExportedGivenSite {
    #[must_use]
    pub const fn new(source_id: SourceId, span: Span) -> Self {
        Self { source_id, span }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ModuleExportSummary {
    exports: ExportNameList,
    opaque: ExportNameList,
    exported_givens: ExportedGivenSiteList,
}

impl ModuleExportSummary {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn exports(&self) -> impl Iterator<Item = &str> {
        self.exports.iter().map(Box::as_ref)
    }

    pub fn exported_givens(&self) -> impl Iterator<Item = ExportedGivenSite> + '_ {
        self.exported_givens.iter().copied()
    }

    #[must_use]
    pub const fn exported_given_count(&self) -> usize {
        self.exported_givens.len()
    }

    #[must_use]
    pub fn is_export_opaque(&self, name: &str) -> bool {
        self.opaque.iter().any(|it| it.as_ref() == name)
    }

    fn push_export(&mut self, name: &str, is_opaque: bool) {
        if self.exports.iter().any(|it| it.as_ref() == name) {
            if is_opaque && !self.is_export_opaque(name) {
                self.opaque.push(name.into());
            }
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
pub fn collect_import_sites(source_id: SourceId, tree: &SyntaxTree) -> Vec<ImportSite> {
    let mut out = Vec::new();
    walk_nodes(tree.root(), &mut |node| {
        if node.kind() != SyntaxNodeKind::ImportExpr {
            return;
        }
        collect_import_expr_sites(source_id, node, &mut out);
    });
    out
}

#[must_use]
pub fn collect_export_summary(_source_id: SourceId, tree: &SyntaxTree) -> ModuleExportSummary {
    let mut summary = ModuleExportSummary::new();
    walk_nodes(tree.root(), &mut |node| {
        if node.kind() != SyntaxNodeKind::AttributedExpr {
            return;
        }
        if !node
            .child_nodes()
            .any(|child| child.kind() == SyntaxNodeKind::ExportMod)
        {
            return;
        }
        let is_opaque = node
            .child_tokens()
            .any(|tok| tok.kind() == TokenKind::KwHidden);

        if let Some(let_expr) = node
            .child_nodes()
            .find(|child| child.kind() == SyntaxNodeKind::LetExpr)
        {
            let Some(pat) = let_expr.child_nodes().find(|n| n.kind().is_pat()) else {
                return;
            };
            for token in pattern_binder_tokens(pat) {
                if let Some(name) = canonical_token_text(token) {
                    summary.push_export(name, is_opaque);
                }
            }
        }
    });
    summary
}

fn collect_import_expr_sites(
    source_id: SourceId,
    node: SyntaxNode<'_, '_>,
    out: &mut Vec<ImportSite>,
) {
    let Some(expr) = node.child_nodes().next() else {
        out.push(ImportSite::new(
            source_id,
            node.span(),
            ImportSiteKind::NonLiteral,
        ));
        return;
    };
    push_import_sites_from_arg(source_id, node.span(), expr, out);
}

fn push_import_sites_from_arg(
    source_id: SourceId,
    import_span: Span,
    expr: SyntaxNode<'_, '_>,
    out: &mut Vec<ImportSite>,
) {
    if expr.kind() == SyntaxNodeKind::TupleExpr {
        let mut pushed = false;
        for child in expr.child_nodes() {
            pushed = true;
            push_import_sites_from_arg(source_id, child.span(), child, out);
        }
        if pushed {
            return;
        }
    }
    let kind = classify_import_arg(expr);
    out.push(ImportSite::new(source_id, import_span, kind));
}

fn classify_import_arg(expr: SyntaxNode<'_, '_>) -> ImportSiteKind {
    match expr.kind() {
        SyntaxNodeKind::LiteralExpr => {
            let Some(tok) = expr.child_tokens().next() else {
                return ImportSiteKind::NonLiteral;
            };
            if tok.kind() != TokenKind::String {
                return ImportSiteKind::NonLiteral;
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
        _ => ImportSiteKind::NonLiteral,
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

fn canonical_token_text<'src>(tok: SyntaxToken<'src, 'src>) -> Option<&'src str> {
    let raw = tok.text()?;
    Some(canonical_name_text(tok.kind(), raw))
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
