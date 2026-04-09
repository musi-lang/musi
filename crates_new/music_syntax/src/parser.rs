use core::mem;

use music_arena::{Arena, SliceArena};
use music_base::Span;

use crate::errors::{ParseError, ParseErrorKind, ParseErrorList, ParseResult};
use crate::tree::{
    SyntaxElementId, SyntaxNodeData, SyntaxNodeId, SyntaxNodeKind, SyntaxTokenId, SyntaxTree,
};
use crate::{LexedSource, Token, TokenKind};

mod expr;
mod forms;
mod pat;

const PREFIX_BP: u8 = 24;
const SYMBOLIC_BP: u8 = 22;
const MUL_BP: u8 = 20;
const ADD_BP: u8 = 18;
const SHIFT_BP: u8 = 16;
const COMPARE_BP: u8 = 14;
const AND_BP: u8 = 12;
const XOR_BP: u8 = 10;
const OR_BP: u8 = 8;
const ARROW_BP: u8 = 7;
const PIPE_BP: u8 = 6;
const ASSIGN_BP: u8 = 2;

#[derive(Debug, Clone)]
pub struct ParsedSource {
    tree: SyntaxTree,
    errors: ParseErrorList,
}

impl ParsedSource {
    #[must_use]
    pub const fn new(tree: SyntaxTree, errors: ParseErrorList) -> Self {
        Self { tree, errors }
    }

    #[must_use]
    pub const fn tree(&self) -> &SyntaxTree {
        &self.tree
    }

    #[must_use]
    pub fn errors(&self) -> &[ParseError] {
        &self.errors
    }
}

type SyntaxElementList = Vec<SyntaxElementId>;

#[must_use]
pub fn parse(lexed: LexedSource) -> ParsedSource {
    let token_spans = lexed.tokens().iter().map(|token| token.span).collect();
    let mut builder = SyntaxTreeBuilder::new(token_spans);
    let mut errors = Vec::new();
    let mut parser = Parser::new(&lexed, &mut builder, &mut errors);
    let mut children = parser.parse_root_children();
    children.push(parser.advance_element());
    let root = builder.push_node_from_children(SyntaxNodeKind::SourceFile, children);
    let tree = builder.finish(lexed, root);
    ParsedSource::new(tree, errors)
}

#[derive(Debug)]
struct SyntaxTreeBuilder {
    token_spans: Vec<Span>,
    nodes: Arena<SyntaxNodeData>,
    children: SliceArena<SyntaxElementId>,
    token_parents: Vec<Option<SyntaxNodeId>>,
}

impl SyntaxTreeBuilder {
    fn new(token_spans: Vec<Span>) -> Self {
        let token_count = token_spans.len();
        Self {
            token_spans,
            nodes: Arena::new(),
            children: SliceArena::new(),
            token_parents: vec![None; token_count],
        }
    }

    fn push_node_from_children(
        &mut self,
        kind: SyntaxNodeKind,
        children: SyntaxElementList,
    ) -> SyntaxNodeId {
        let span = self.children_span(&children);
        let range = self.children.alloc_from_iter(children.iter().copied());
        let node = self.nodes.alloc(SyntaxNodeData {
            kind,
            span,
            parent: None,
            children: range,
        });
        for child in children {
            match child {
                SyntaxElementId::Node(id) => self.nodes.get_mut(id).parent = Some(node),
                SyntaxElementId::Token(id) => {
                    let index = usize::try_from(id.raw()).unwrap_or(usize::MAX);
                    if let Some(parent) = self.token_parents.get_mut(index) {
                        *parent = Some(node);
                    }
                }
            }
        }
        node
    }

    fn push_error_node(&mut self, children: SyntaxElementList) -> SyntaxNodeId {
        self.push_node_from_children(SyntaxNodeKind::Error, children)
    }

    fn node_kind(&self, id: SyntaxNodeId) -> SyntaxNodeKind {
        self.nodes.get(id).kind
    }

    fn children_span(&self, children: &[SyntaxElementId]) -> Span {
        let Some(first) = children.first().copied() else {
            return Span::DUMMY;
        };
        let Some(last) = children.last().copied() else {
            return Span::DUMMY;
        };
        self.element_span(first).to(self.element_span(last))
    }

    fn element_span(&self, child: SyntaxElementId) -> Span {
        match child {
            SyntaxElementId::Node(id) => self.nodes.get(id).span,
            SyntaxElementId::Token(id) => {
                let index = usize::try_from(id.raw()).unwrap_or(usize::MAX);
                self.token_spans.get(index).copied().unwrap_or_default()
            }
        }
    }

    fn finish(self, lexed: LexedSource, root: SyntaxNodeId) -> SyntaxTree {
        SyntaxTree::new(lexed, self.nodes, self.children, self.token_parents, root)
    }
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    builder: &'a mut SyntaxTreeBuilder,
    errors: &'a mut ParseErrorList,
    comparison_exprs: Vec<SyntaxNodeId>,
    lparen_match: Vec<Option<usize>>,
    quote_depth: u32,
}

impl<'a> Parser<'a> {
    fn new(
        lexed: &'a LexedSource,
        builder: &'a mut SyntaxTreeBuilder,
        errors: &'a mut ParseErrorList,
    ) -> Self {
        let tokens = lexed.tokens();
        Self {
            tokens,
            pos: 0,
            builder,
            errors,
            comparison_exprs: Vec::new(),
            lparen_match: compute_matching(tokens, TokenKind::LParen, TokenKind::RParen),
            quote_depth: 0,
        }
    }

    fn parse_root_children(&mut self) -> SyntaxElementList {
        let mut children = Vec::new();
        while !self.at(TokenKind::Eof) {
            let before = self.pos;
            let res =
                if self.at_any(&[TokenKind::KwInfixl, TokenKind::KwInfixr, TokenKind::KwInfix]) {
                    self.parse_fixity_directive()
                } else {
                    self.parse_stmt()
                };
            match res {
                Ok(node) => children.push(SyntaxElementId::Node(node)),
                Err(error) => {
                    self.error(error);
                    children.push(SyntaxElementId::Node(self.parse_error_stmt()));
                    if self.pos == before {
                        children.push(self.advance_element());
                    }
                    self.synchronize_stmt();
                }
            }
        }
        children
    }

    fn parse_fixity_directive(&mut self) -> ParseResult<SyntaxNodeId> {
        let kw = self.advance_element();
        let prec = self.expect_token(TokenKind::Int)?;
        let op = self.expect_token(TokenKind::OpIdent)?;
        let semi = self.expect_token(TokenKind::Semicolon)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::FixityDirective, vec![kw, prec, op, semi]))
    }

    fn parse_stmt(&mut self) -> ParseResult<SyntaxNodeId> {
        let expr = self.parse_expr(0)?;
        let semi = self.expect_token(TokenKind::Semicolon)?;
        Ok(self.node2(
            SyntaxNodeKind::SequenceExpr,
            SyntaxElementId::Node(expr),
            semi,
        ))
    }

    fn parse_error_stmt(&mut self) -> SyntaxNodeId {
        let mut children = Vec::new();
        while !self.at_any(&[
            TokenKind::Semicolon,
            TokenKind::RParen,
            TokenKind::RBracket,
            TokenKind::RBrace,
            TokenKind::Pipe,
            TokenKind::Eof,
        ]) {
            children.push(self.advance_element());
        }
        if self.at(TokenKind::Semicolon) {
            children.push(self.advance_element());
        }
        self.builder.push_error_node(children)
    }

    fn node(&mut self, kind: SyntaxNodeKind, children: SyntaxElementList) -> SyntaxNodeId {
        self.builder.push_node_from_children(kind, children)
    }

    fn node1(&mut self, kind: SyntaxNodeKind, a: SyntaxElementId) -> SyntaxNodeId {
        self.node(kind, vec![a])
    }

    fn node2(
        &mut self,
        kind: SyntaxNodeKind,
        a: SyntaxElementId,
        b: SyntaxElementId,
    ) -> SyntaxNodeId {
        self.node(kind, vec![a, b])
    }

    fn node3(
        &mut self,
        kind: SyntaxNodeKind,
        a: SyntaxElementId,
        b: SyntaxElementId,
        c: SyntaxElementId,
    ) -> SyntaxNodeId {
        self.node(kind, vec![a, b, c])
    }

    fn wrap_list(
        &mut self,
        kind: SyntaxNodeKind,
        open: SyntaxElementId,
        mut inner: SyntaxElementList,
        close: SyntaxElementId,
    ) -> SyntaxNodeId {
        let mut children = vec![open];
        children.append(&mut inner);
        children.push(close);
        self.node(kind, children)
    }

    fn rewrap_node(&mut self, node: SyntaxNodeId, children: SyntaxElementList) -> SyntaxNodeId {
        let kind = self.builder.node_kind(node);
        self.node(kind, children)
    }

    fn is_lambda_paren(&self) -> bool {
        let Some(close) = self.lparen_match.get(self.pos).copied().flatten() else {
            return false;
        };
        self.tokens
            .get(close + 1)
            .is_some_and(|token| matches!(token.kind, TokenKind::EqGt | TokenKind::Colon))
    }

    fn is_pi_paren(&self) -> bool {
        if !matches!(self.nth_kind(1), TokenKind::Ident) {
            return false;
        }
        if self.nth_kind(2) != TokenKind::Colon {
            return false;
        }
        let Some(close) = self.lparen_match.get(self.pos).copied().flatten() else {
            return false;
        };
        self.tokens
            .get(close + 1)
            .is_some_and(|token| matches!(token.kind, TokenKind::MinusGt | TokenKind::TildeGt))
    }

    fn is_comparison_expr(&self, node: SyntaxNodeId) -> bool {
        self.comparison_exprs.contains(&node)
    }

    fn synchronize_stmt(&mut self) {
        while !self.at_any(&[
            TokenKind::Semicolon,
            TokenKind::RParen,
            TokenKind::RBracket,
            TokenKind::RBrace,
            TokenKind::Pipe,
            TokenKind::Eof,
        ]) {
            let _ = self.advance();
        }
        if self.at(TokenKind::Semicolon) {
            let _ = self.advance();
        }
    }

    fn peek(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or_else(|| self.tokens.last().expect("lexer emits EOF token"))
    }

    fn nth(&self, offset: usize) -> &Token {
        self.tokens
            .get(self.pos + offset)
            .unwrap_or_else(|| self.tokens.last().expect("lexer emits EOF token"))
    }

    fn peek_kind(&self) -> TokenKind {
        self.peek().kind
    }

    fn nth_kind(&self, offset: usize) -> TokenKind {
        self.nth(offset).kind
    }

    fn at(&self, expected: TokenKind) -> bool {
        same_kind(self.peek_kind(), expected)
    }

    fn at_any(&self, expected: &[TokenKind]) -> bool {
        expected.iter().copied().any(|kind| self.at(kind))
    }

    fn advance(&mut self) -> &Token {
        let index = self.pos;
        if !self.at(TokenKind::Eof) {
            self.pos += 1;
        }
        self.tokens
            .get(index)
            .unwrap_or_else(|| self.tokens.last().expect("lexer emits EOF token"))
    }

    fn advance_element(&mut self) -> SyntaxElementId {
        let index = self.pos;
        let _ = self.advance();
        let raw = u32::try_from(index).expect("token index fits in u32");
        SyntaxElementId::Token(SyntaxTokenId::from_raw(raw))
    }

    fn eat(&mut self, expected: TokenKind) -> Option<SyntaxElementId> {
        self.at(expected).then(|| self.advance_element())
    }

    fn expect_token(&mut self, expected: TokenKind) -> ParseResult<SyntaxElementId> {
        if self.at(expected) {
            return Ok(self.advance_element());
        }
        Err(self.expected_token(expected))
    }

    fn expected_token(&self, expected: TokenKind) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedToken {
                expected,
                found: self.found_token(),
            },
            span: self.span(),
        }
    }

    fn expect_ident_element(&mut self) -> ParseResult<SyntaxElementId> {
        match self.peek_kind() {
            TokenKind::Ident => Ok(self.advance_element()),
            _ => Err(ParseError {
                kind: ParseErrorKind::ExpectedIdentifier {
                    found: self.found_token(),
                },
                span: self.span(),
            }),
        }
    }

    fn expect_name_element(&mut self) -> ParseResult<SyntaxElementId> {
        match self.peek_kind() {
            TokenKind::Ident | TokenKind::OpIdent => Ok(self.advance_element()),
            _ => Err(ParseError {
                kind: ParseErrorKind::ExpectedIdentifier {
                    found: self.found_token(),
                },
                span: self.span(),
            }),
        }
    }

    fn span(&self) -> Span {
        self.peek().span
    }

    fn found_token(&self) -> TokenKind {
        self.peek_kind()
    }

    fn error(&mut self, error: ParseError) {
        self.errors.push(error);
    }

    fn expected_expression(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedExpression {
                found: self.found_token(),
            },
            span: self.span(),
        }
    }

    fn expected_pattern(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedPattern {
                found: self.found_token(),
            },
            span: self.span(),
        }
    }

    fn expected_member(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedMember {
                found: self.found_token(),
            },
            span: self.span(),
        }
    }

    fn expected_splice_target(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedSpliceTarget {
                found: self.found_token(),
            },
            span: self.span(),
        }
    }

    fn expected_operator_member_name(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedOperatorMemberName {
                found: self.found_token(),
            },
            span: self.span(),
        }
    }

    fn expected_field_target(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedFieldTarget {
                found: self.found_token(),
            },
            span: self.span(),
        }
    }

    fn expected_constraint_operator(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedConstraintOperator {
                found: self.found_token(),
            },
            span: self.span(),
        }
    }

    fn expected_attr_value(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedAttrValue {
                found: self.found_token(),
            },
            span: self.span(),
        }
    }
}

fn same_kind(left: TokenKind, right: TokenKind) -> bool {
    mem::discriminant(&left) == mem::discriminant(&right)
}

fn compute_matching(tokens: &[Token], open: TokenKind, close: TokenKind) -> Vec<Option<usize>> {
    let mut matches = vec![None; tokens.len()];
    let mut stack = Vec::new();
    for (index, token) in tokens.iter().enumerate() {
        if same_kind(token.kind, open) {
            stack.push(index);
        } else if same_kind(token.kind, close) {
            if let Some(open_index) = stack.pop() {
                matches[open_index] = Some(index);
            }
        }
    }
    matches
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
