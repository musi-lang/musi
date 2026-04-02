use music_base::{SourceId, Span};
use music_syntax::{
    SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree, TokenKind, canonical_name_text,
    pattern_binder_tokens,
};

use crate::ModuleSpecifier;
use crate::string_lit::{decode_string_lit, decode_template_lit};

type ExportNameList = Vec<Box<str>>;

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
    exports: ExportNameList,
    opaque: ExportNameList,
    exported_instances: usize,
}

impl ModuleExportSummary {
    pub fn exports(&self) -> impl Iterator<Item = &str> {
        self.exports.iter().map(Box::as_ref)
    }

    #[must_use]
    pub const fn exported_instance_count(&self) -> usize {
        self.exported_instances
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
        let has_foreign = node
            .child_tokens()
            .any(|tok| tok.kind() == TokenKind::KwForeign);

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
            return;
        }

        if has_foreign {
            if let Some(group) = node
                .child_nodes()
                .find(|child| child.kind() == SyntaxNodeKind::MemberList)
            {
                collect_foreign_group_exports(group, &mut summary, is_opaque);
                return;
            }
        }

        if node
            .child_nodes()
            .any(|child| child.kind() == SyntaxNodeKind::InstanceExpr)
        {
            summary.exported_instances = summary.exported_instances.saturating_add(1);
        }
    });
    summary
}

fn classify_import_expr(node: SyntaxNode<'_, '_>) -> ImportSiteKind {
    let mut children = node.child_nodes();
    let Some(expr) = children.next() else {
        return ImportSiteKind::Dynamic;
    };
    match expr.kind() {
        SyntaxNodeKind::LiteralExpr => {
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
        SyntaxNodeKind::TemplateExpr => classify_static_template_import(expr),
        _ => ImportSiteKind::Dynamic,
    }
}

fn classify_static_template_import(template: SyntaxNode<'_, '_>) -> ImportSiteKind {
    if template.child_nodes().next().is_some() {
        return ImportSiteKind::Dynamic;
    }
    let Some(tok) = template.child_tokens().next() else {
        return ImportSiteKind::Dynamic;
    };
    if tok.kind() != TokenKind::TemplateNoSubst {
        return ImportSiteKind::Dynamic;
    }
    let Some(raw) = tok.text() else {
        return ImportSiteKind::InvalidStringLit;
    };
    let Ok(decoded) = decode_template_lit(raw) else {
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
    if node.kind() == SyntaxNodeKind::QuoteExpr {
        return;
    }
    for child in node.children() {
        if let Some(node) = child.into_node() {
            walk_nodes(node, f);
        }
    }
}

fn collect_foreign_group_exports(
    group: SyntaxNode<'_, '_>,
    out: &mut ModuleExportSummary,
    is_opaque: bool,
) {
    for member in group
        .child_nodes()
        .filter(|n| n.kind() == SyntaxNodeKind::Member)
    {
        if let Some(name) = first_name_text(member) {
            out.push_export(name, is_opaque);
        }
    }
}

fn first_name_text<'src>(node: SyntaxNode<'_, 'src>) -> Option<&'src str> {
    node.child_tokens()
        .find(|tok| is_name_token(tok.kind()))
        .and_then(canonical_token_text)
}

fn canonical_token_text<'src>(tok: SyntaxToken<'_, 'src>) -> Option<&'src str> {
    let raw = tok.text()?;
    Some(canonical_name_text(tok.kind(), raw))
}

const fn is_name_token(kind: TokenKind) -> bool {
    matches!(kind, TokenKind::Ident | TokenKind::OpIdent)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
