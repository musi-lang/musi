use music_syntax::{SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxTree, TokenKind};

use crate::source::TokenFormatRole;

pub fn collect_token_roles(tree: &SyntaxTree) -> Vec<TokenFormatRole> {
    let mut roles = vec![TokenFormatRole::Regular; tree.token_count()];
    let mut index = 0usize;
    collect_node_token_roles(tree.root(), &mut roles, &mut index);
    roles
}

fn collect_node_token_roles(
    node: SyntaxNode<'_, '_>,
    roles: &mut [TokenFormatRole],
    index: &mut usize,
) {
    if node.kind() == SyntaxNodeKind::Attr {
        collect_attr_token_roles(node, roles, index);
        return;
    }
    for child in node.children() {
        match child {
            SyntaxElement::Node(child_node) => collect_node_token_roles(child_node, roles, index),
            SyntaxElement::Token(token) => {
                if let Some(role) = roles.get_mut(*index) {
                    *role = token_role_for(node.kind(), token.kind());
                }
                *index = index.saturating_add(1);
            }
        }
    }
}

fn collect_attr_token_roles(
    node: SyntaxNode<'_, '_>,
    roles: &mut [TokenFormatRole],
    index: &mut usize,
) {
    let token_count = count_node_tokens(node);
    let end_index = index.saturating_add(token_count.saturating_sub(1));
    collect_attr_token_roles_inner(node, roles, index, end_index);
}

fn collect_attr_token_roles_inner(
    node: SyntaxNode<'_, '_>,
    roles: &mut [TokenFormatRole],
    index: &mut usize,
    end_index: usize,
) {
    for child in node.children() {
        match child {
            SyntaxElement::Node(child_node) => {
                collect_attr_token_roles_inner(child_node, roles, index, end_index);
            }
            SyntaxElement::Token(_) => {
                if let Some(role) = roles.get_mut(*index) {
                    *role = if *index == end_index {
                        TokenFormatRole::AttributeEnd
                    } else {
                        TokenFormatRole::Attribute
                    };
                }
                *index = index.saturating_add(1);
            }
        }
    }
}

fn count_node_tokens(node: SyntaxNode<'_, '_>) -> usize {
    node.children()
        .map(|child| match child {
            SyntaxElement::Node(child_node) => count_node_tokens(child_node),
            SyntaxElement::Token(_) => 1,
        })
        .sum()
}

const fn token_role_for(parent: SyntaxNodeKind, token: TokenKind) -> TokenFormatRole {
    match (parent, token) {
        (SyntaxNodeKind::SequenceExpr, TokenKind::LParen | TokenKind::RParen) => {
            TokenFormatRole::SequenceParen
        }
        (SyntaxNodeKind::MatchExpr, TokenKind::LParen | TokenKind::RParen) => {
            TokenFormatRole::MatchParen
        }
        (SyntaxNodeKind::MemberList, TokenKind::LParen | TokenKind::RParen) => {
            TokenFormatRole::ForeignGroupParen
        }
        (SyntaxNodeKind::ParamList, TokenKind::LParen | TokenKind::RParen) => {
            TokenFormatRole::ParamParen
        }
        (SyntaxNodeKind::TypeParamList, TokenKind::LBracket | TokenKind::RBracket) => {
            TokenFormatRole::TypeParamBracket
        }
        (SyntaxNodeKind::ArrayTy, TokenKind::LBracket | TokenKind::RBracket) => {
            TokenFormatRole::ArrayTypeBracket
        }
        _ => TokenFormatRole::Regular,
    }
}
