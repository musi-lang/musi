use music_base::{SourceId, Span};
use music_syntax::{
    SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree, TokenKind, canonical_name_text,
    pattern_binder_tokens,
};

use crate::ModuleSpecifier;
use crate::string_lit::{decode_string_lit, decode_template_lit};

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
pub fn collect_export_summary(source_id: SourceId, tree: &SyntaxTree) -> ModuleExportSummary {
    let mut summary = ModuleExportSummary::new();
    walk_nodes(tree.root(), &mut |node| {
        if node.kind() != SyntaxNodeKind::AttributedExpr {
            return;
        }
        let Some(export_mod) = node
            .child_nodes()
            .find(|child| child.kind() == SyntaxNodeKind::ExportMod)
        else {
            return;
        };
        let is_opaque = export_mod
            .child_tokens()
            .any(|tok| tok.kind() == TokenKind::KwOpaque);

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

        if let Some(group) = node
            .child_nodes()
            .find(|child| child.kind() == SyntaxNodeKind::MemberList)
        {
            collect_foreign_group_exports(group, &mut summary, is_opaque);
            return;
        }

        if let Some(sequence) = node
            .child_nodes()
            .find(|child| child.kind() == SyntaxNodeKind::SequenceExpr)
        {
            collect_exported_sequence(sequence, &mut summary, is_opaque);
            return;
        }

        if node
            .child_nodes()
            .any(|child| child.kind() == SyntaxNodeKind::GivenExpr)
        {
            summary
                .exported_givens
                .push(ExportedGivenSite::new(source_id, node.span()));
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
    if matches!(
        expr.kind(),
        SyntaxNodeKind::SequenceExpr | SyntaxNodeKind::TupleExpr
    ) {
        for child in expr.child_nodes().filter(|child| child.kind().is_expr()) {
            let kind = classify_import_arg(child);
            out.push(ImportSite::new(source_id, child.span(), kind));
        }
        return;
    }
    let kind = classify_import_arg(expr);
    out.push(ImportSite::new(source_id, node.span(), kind));
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
        SyntaxNodeKind::TemplateExpr => classify_static_template_import(expr),
        _ => ImportSiteKind::NonLiteral,
    }
}

fn classify_static_template_import(template: SyntaxNode<'_, '_>) -> ImportSiteKind {
    if template.child_nodes().next().is_some() {
        return ImportSiteKind::NonLiteral;
    }
    let Some(tok) = template.child_tokens().next() else {
        return ImportSiteKind::NonLiteral;
    };
    if tok.kind() != TokenKind::TemplateNoSubst {
        return ImportSiteKind::NonLiteral;
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

fn collect_exported_sequence(
    sequence: SyntaxNode<'_, '_>,
    out: &mut ModuleExportSummary,
    is_opaque: bool,
) {
    for let_expr in sequence
        .child_nodes()
        .filter(|node| node.kind() == SyntaxNodeKind::LetExpr)
    {
        let Some(pat) = let_expr.child_nodes().find(|node| node.kind().is_pat()) else {
            continue;
        };
        for token in pattern_binder_tokens(pat) {
            if let Some(name) = canonical_token_text(token) {
                out.push_export(name, is_opaque);
            }
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

fn first_name_text<'src>(node: SyntaxNode<'src, 'src>) -> Option<&'src str> {
    node.child_tokens()
        .find(|tok| is_name_token(tok.kind()))
        .and_then(canonical_token_text)
}

fn canonical_token_text<'src>(tok: SyntaxToken<'src, 'src>) -> Option<&'src str> {
    let raw = tok.text()?;
    Some(canonical_name_text(tok.kind(), raw))
}

const fn is_name_token(kind: TokenKind) -> bool {
    matches!(kind, TokenKind::Ident | TokenKind::OpIdent)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
