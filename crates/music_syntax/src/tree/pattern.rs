use super::{SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken};

use crate::TokenKind;

type PatternBinderToken<'tree, 'src> = SyntaxToken<'tree, 'src>;
type PatternBinderTokenList<'tree, 'src> = Vec<PatternBinderToken<'tree, 'src>>;

struct PatternBinderCollector<'tree, 'src> {
    out: Vec<PatternBinderToken<'tree, 'src>>,
}

impl<'tree, 'src> PatternBinderCollector<'tree, 'src> {
    const fn new() -> Self {
        Self { out: Vec::new() }
    }

    fn collect(mut self, node: SyntaxNode<'tree, 'src>) -> Vec<PatternBinderToken<'tree, 'src>> {
        self.collect_pattern_binder_tokens(node);
        self.out
    }

    fn collect_pattern_binder_tokens(&mut self, node: SyntaxNode<'tree, 'src>) {
        match node.kind() {
            SyntaxNodeKind::BindPat => self.push_bind_pat_token(node),
            SyntaxNodeKind::AsPat => {
                if let Some(inner) = node.child_nodes().find(|child| child.kind().is_pat()) {
                    self.collect_pattern_binder_tokens(inner);
                }
                self.push_bind_pat_token(node);
            }
            SyntaxNodeKind::OrPat => {
                for child in node.child_nodes().filter(|child| child.kind().is_pat()) {
                    self.collect_pattern_binder_tokens(child);
                }
            }
            SyntaxNodeKind::RecordPat => self.collect_record_pattern_binder_tokens(node),
            SyntaxNodeKind::VariantPat
            | SyntaxNodeKind::VariantPayloadList
            | SyntaxNodeKind::VariantPatArg => {
                for child in node.child_nodes() {
                    self.collect_pattern_binder_tokens(child);
                }
            }
            kind if kind.is_pat() => {
                for child in node.child_nodes().filter(|child| child.kind().is_pat()) {
                    self.collect_pattern_binder_tokens(child);
                }
            }
            _ => {}
        }
    }

    fn push_bind_pat_token(&mut self, node: SyntaxNode<'tree, 'src>) {
        if let Some(token) = node
            .child_tokens()
            .find(|token| Self::is_bind_name_token(token.kind()))
        {
            self.out.push(token);
        }
    }

    const fn is_bind_name_token(kind: TokenKind) -> bool {
        matches!(kind, TokenKind::Ident)
    }

    fn collect_record_pattern_binder_tokens(&mut self, node: SyntaxNode<'tree, 'src>) {
        let children: Vec<_> = node.children().collect();
        let mut index = 0usize;
        while index < children.len() {
            let Some(name_token) = children
                .get(index)
                .copied()
                .and_then(SyntaxElement::into_token)
                .filter(|token| Self::is_bind_name_token(token.kind()))
            else {
                index += 1;
                continue;
            };
            index += 1;

            if children
                .get(index)
                .copied()
                .and_then(SyntaxElement::into_token)
                .is_some_and(|token| token.kind() == TokenKind::Colon)
            {
                index += 1;
                if let Some(value_pattern) = children
                    .get(index)
                    .copied()
                    .and_then(SyntaxElement::into_node)
                    .filter(|child| child.kind().is_pat())
                {
                    self.collect_pattern_binder_tokens(value_pattern);
                    index += 1;
                }
                continue;
            }

            self.out.push(name_token);
        }
    }
}

#[must_use]
pub fn pattern_binder_tokens<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
) -> PatternBinderTokenList<'tree, 'src> {
    PatternBinderCollector::new().collect(node)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
