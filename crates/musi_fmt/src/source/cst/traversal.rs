use std::ops::Range;

use music_base::Span;
use music_syntax::{
    LexedSource, SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree, TokenKind,
};

use crate::FormatOptions;

use super::layout::{format_bind_layout, format_match_arrow_layout, format_record_layout};
use super::{CstFormatter, CstLeafRole, TokenWriteOptions};

pub(super) fn format_cst_source(
    source: &str,
    tree: &SyntaxTree,
    options: &FormatOptions,
    protected_ranges: Vec<Range<usize>>,
) -> String {
    let lexed = tree.lexed();
    let mut formatter = CstFormatter::new(source, options, protected_ranges);
    let mut token_index = 0usize;
    formatter.write_node(tree.root(), lexed, &mut token_index, None);
    formatter.write_lexed_leaf_tail(lexed, &mut token_index);
    let formatted_text = format_bind_layout(formatter.finish(), options);
    let formatted_text = format_record_layout(formatted_text, options);
    format_match_arrow_layout(formatted_text, options)
}

impl CstFormatter<'_> {
    fn write_node(
        &mut self,
        node: SyntaxNode<'_, '_>,
        lexed: &LexedSource,
        token_index: &mut usize,
        attr_last_token: Option<(TokenKind, Span)>,
    ) {
        let attr_last_token = if node.kind() == SyntaxNodeKind::Attr {
            last_node_token(node).map(|token| (token.kind(), token.span()))
        } else {
            attr_last_token
        };
        for child in node.children() {
            match child {
                SyntaxElement::Node(child_node) => {
                    self.write_node(child_node, lexed, token_index, attr_last_token);
                }
                SyntaxElement::Token(token) => {
                    self.write_syntax_token(node, token, lexed, token_index, attr_last_token);
                }
            }
        }
    }

    fn write_syntax_token(
        &mut self,
        parent: SyntaxNode<'_, '_>,
        token: SyntaxToken<'_, '_>,
        lexed: &LexedSource,
        token_index: &mut usize,
        attr_last_token: Option<(TokenKind, Span)>,
    ) {
        if token.kind() == TokenKind::Eof {
            return;
        }
        let current_index = self.write_tokens_before(token, lexed, token_index);
        let role = attr_last_token.map_or_else(
            || leaf_role_for(parent, token.kind()),
            |(last_kind, last_span)| {
                if last_kind == token.kind() && last_span == token.span() {
                    CstLeafRole::AttributeEnd
                } else {
                    CstLeafRole::Attribute
                }
            },
        );
        self.write_lexed_leaf(lexed, current_index, role);
        *token_index = current_index.saturating_add(1);
    }

    fn write_tokens_before(
        &mut self,
        token: SyntaxToken<'_, '_>,
        lexed: &LexedSource,
        token_index: &mut usize,
    ) -> usize {
        while let Some(lexed_token) = lexed.tokens().get(*token_index) {
            if lexed_token.kind == token.kind() && lexed_token.span == token.span() {
                return *token_index;
            }
            self.write_lexed_leaf(lexed, *token_index, CstLeafRole::Regular);
            *token_index = token_index.saturating_add(1);
        }
        token_index.saturating_sub(1)
    }

    fn write_lexed_leaf_tail(&mut self, lexed: &LexedSource, token_index: &mut usize) {
        while let Some(token) = lexed.tokens().get(*token_index) {
            if token.kind == TokenKind::Eof {
                break;
            }
            self.write_lexed_leaf(lexed, *token_index, CstLeafRole::Regular);
            *token_index = token_index.saturating_add(1);
        }
    }

    fn write_lexed_leaf(&mut self, lexed: &LexedSource, token_index: usize, role: CstLeafRole) {
        let Some(token) = lexed.tokens().get(token_index) else {
            return;
        };
        if token.kind == TokenKind::Eof {
            return;
        }
        for trivia in lexed.token_trivia(token_index) {
            if self.write_protected_if_needed(trivia.span) {
                continue;
            }
            if trivia.kind.is_comment() {
                let Some(text) = self.original.get(
                    usize::try_from(trivia.span.start).unwrap_or(usize::MAX)
                        ..usize::try_from(trivia.span.end).unwrap_or(usize::MAX),
                ) else {
                    continue;
                };
                self.write_comment(text, trivia.kind, trivia.span);
            }
        }
        if self.write_protected_if_needed(token.span) {
            return;
        }
        let Some(text) = lexed.token_text(token_index) else {
            return;
        };
        self.preserve_blank_separator_if_needed(token.span);
        let token_options = TokenWriteOptions::empty()
            .with_break_after_comma(self.should_break_after_current_comma(lexed, token_index))
            .with_skip_current_comma(self.should_skip_current_comma(lexed, token_index))
            .with_break_before_operator(
                self.should_break_before_current_operator(lexed, token_index),
            )
            .with_break_before_else(self.should_break_before_current_else(lexed, token_index))
            .with_break_after_colon_eq(self.should_break_after_current_colon_eq(lexed, token_index))
            .with_break_after_open_group(self.should_break_after_current_open_group(
                lexed,
                token_index,
                role,
            ));
        self.write_token(token.kind, text, role, token.span, token_options);
    }
}

fn last_node_token<'tree, 'src>(node: SyntaxNode<'tree, 'src>) -> Option<SyntaxToken<'tree, 'src>> {
    node.children().filter_map(last_element_token).last()
}

fn last_element_token<'tree, 'src>(
    element: SyntaxElement<'tree, 'src>,
) -> Option<SyntaxToken<'tree, 'src>> {
    match element {
        SyntaxElement::Node(node) => last_node_token(node),
        SyntaxElement::Token(token) => Some(token),
    }
}

fn leaf_role_for(parent: SyntaxNode<'_, '_>, token: TokenKind) -> CstLeafRole {
    match (parent.kind(), token) {
        (SyntaxNodeKind::SequenceExpr, TokenKind::LParen | TokenKind::RParen) => {
            CstLeafRole::SequenceParen
        }
        (SyntaxNodeKind::MatchExpr, TokenKind::LParen | TokenKind::RParen) => {
            CstLeafRole::MatchParen
        }
        (SyntaxNodeKind::MemberList, TokenKind::LParen | TokenKind::RParen) => {
            CstLeafRole::ForeignGroupParen
        }
        (SyntaxNodeKind::CallExpr, TokenKind::LParen | TokenKind::RParen) => CstLeafRole::CallParen,
        (SyntaxNodeKind::ParamList, TokenKind::LParen | TokenKind::RParen)
            if parent
                .parent()
                .is_some_and(|node| node.kind() == SyntaxNodeKind::Member) =>
        {
            CstLeafRole::MemberParamParen
        }
        (SyntaxNodeKind::ParamList, TokenKind::LParen | TokenKind::RParen) => {
            CstLeafRole::ParamParen
        }
        (SyntaxNodeKind::TypeParamList, TokenKind::LBracket | TokenKind::RBracket) => {
            CstLeafRole::TypeParamBracket
        }
        (SyntaxNodeKind::ApplyExpr, TokenKind::LBracket | TokenKind::RBracket) => {
            CstLeafRole::ApplyBracket
        }
        (SyntaxNodeKind::ArrayTy, TokenKind::LBracket | TokenKind::RBracket) => {
            CstLeafRole::ArrayTypeBracket
        }
        (
            SyntaxNodeKind::RecordExpr | SyntaxNodeKind::RecordPat,
            TokenKind::LBrace | TokenKind::RBrace,
        ) => CstLeafRole::CommaListBrace,
        (SyntaxNodeKind::Attr, _) => CstLeafRole::Attribute,
        _ => CstLeafRole::Regular,
    }
}
