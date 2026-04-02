use super::{SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken};

use crate::TokenKind;

#[must_use]
pub fn pattern_binder_tokens<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
) -> Vec<SyntaxToken<'tree, 'src>> {
    let mut out = Vec::new();
    collect_pattern_binder_tokens(node, &mut out);
    out
}

fn collect_pattern_binder_tokens<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
    out: &mut Vec<SyntaxToken<'tree, 'src>>,
) {
    match node.kind() {
        SyntaxNodeKind::BindPat => push_bind_pat_token(node, out),
        SyntaxNodeKind::AsPat => {
            if let Some(inner) = node.child_nodes().find(|child| child.kind().is_pat()) {
                collect_pattern_binder_tokens(inner, out);
            }
            push_bind_pat_token(node, out);
        }
        SyntaxNodeKind::OrPat => {
            for child in node.child_nodes().filter(|child| child.kind().is_pat()) {
                collect_pattern_binder_tokens(child, out);
            }
        }
        SyntaxNodeKind::RecordPat => collect_record_pattern_binder_tokens(node, out),
        kind if kind.is_pat() => {
            for child in node.child_nodes().filter(|child| child.kind().is_pat()) {
                collect_pattern_binder_tokens(child, out);
            }
        }
        _ => {}
    }
}

fn push_bind_pat_token<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
    out: &mut Vec<SyntaxToken<'tree, 'src>>,
) {
    if let Some(token) = node
        .child_tokens()
        .find(|token| token.kind() == TokenKind::Ident)
    {
        out.push(token);
    }
}

fn collect_record_pattern_binder_tokens<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
    out: &mut Vec<SyntaxToken<'tree, 'src>>,
) {
    let children: Vec<_> = node.children().collect();
    let mut index = 0usize;
    while index < children.len() {
        if children[index]
            .into_token()
            .is_some_and(|token| token.kind() == TokenKind::KwMut)
        {
            index += 1;
            continue;
        }

        let Some(name_token) = children
            .get(index)
            .copied()
            .and_then(SyntaxElement::into_token)
            .filter(|token| token.kind() == TokenKind::Ident)
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
                collect_pattern_binder_tokens(value_pattern, out);
                index += 1;
            }
            continue;
        }

        out.push(name_token);
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
