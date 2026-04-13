use std::mem;

use music_arena::{Arena, SliceArena};
use music_base::Span;

use crate::errors::{ParseError, ParseErrorKind, ParseErrorList, ParseResult};
use crate::tree::{
    SyntaxElementId, SyntaxNodeData, SyntaxNodeId, SyntaxNodeKind, SyntaxTokenId, SyntaxTree,
};
use crate::{LexedSource, Token, TokenKind};

mod core;
mod expr;
mod forms;
mod pat;
mod recover;

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
type SyntaxNodeParseResult = ParseResult<SyntaxNodeId>;
type SyntaxElementParseResult = ParseResult<SyntaxElementId>;

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
        let node = self.nodes.alloc(SyntaxNodeData::new(kind, span, range));
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
