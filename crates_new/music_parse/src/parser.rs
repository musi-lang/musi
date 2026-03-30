use core::mem;

use music_ast::{
    SyntaxElementId, SyntaxElementIds, SyntaxNodeId, SyntaxNodeKind, SyntaxTree, SyntaxTreeBuilder,
};
use music_basic::{SourceId, Span};
use music_lex::{LexedSource, Token, TokenKind};

use super::*;

pub struct ParsedSource {
    tree: SyntaxTree,
    errors: Vec<ParseError>,
}

impl ParsedSource {
    #[must_use]
    pub const fn new(tree: SyntaxTree, errors: Vec<ParseError>) -> Self {
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

#[must_use]
pub fn parse(source_id: SourceId, lexed: &LexedSource<'_>) -> ParsedSource {
    let mut parser = Parser::new(source_id, lexed);
    parser.parse_root()
}

pub struct Parser<'lex, 'src> {
    pub(super) source_id: SourceId,
    pub(super) lexed: &'lex LexedSource<'src>,
    pub(super) pos: usize,
    pub(super) builder: SyntaxTreeBuilder,
    pub(super) errors: Vec<ParseError>,
    pub(super) comparison_exprs: Vec<SyntaxNodeId>,
    pub(super) lparen_match: Vec<Option<usize>>,
}

impl<'lex, 'src> Parser<'lex, 'src> {
    pub(crate) fn new(source_id: SourceId, lexed: &'lex LexedSource<'src>) -> Self {
        let lparen_match = compute_lparen_matches(lexed.tokens());
        Self {
            source_id,
            lexed,
            pos: 0,
            builder: SyntaxTreeBuilder::with_capacity(
                source_id,
                lexed.tokens().len().saturating_mul(2),
                lexed.tokens().len(),
            ),
            errors: Vec::new(),
            comparison_exprs: Vec::new(),
            lparen_match,
        }
    }

    pub(crate) fn parse_root(&mut self) -> ParsedSource {
        let mut children = vec![];

        while !self.at(&TokenKind::Eof) {
            let before = self.pos;
            match self.parse_stmt() {
                Ok(stmt) => children.push(SyntaxElementId::Node(stmt)),
                Err(error) => {
                    self.errors.push(error);
                    children.push(SyntaxElementId::Node(self.parse_error_stmt()));
                    if self.pos == before {
                        let token = self.advance_element();
                        children.push(token);
                    }
                    self.synchronize_stmt();
                }
            }
        }

        children.push(self.advance_element());
        let tree = mem::replace(&mut self.builder, SyntaxTreeBuilder::new(self.source_id))
            .finish_root(children);
        ParsedSource::new(tree, mem::take(&mut self.errors))
    }

    pub(crate) fn parse_stmt(&mut self) -> ParseResult<SyntaxNodeId> {
        let expr = self.parse_expr(0)?;
        let semi = self.expect_token(&TokenKind::Semi)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::SequenceExpr,
            [SyntaxElementId::Node(expr), semi],
        ))
    }

    fn parse_error_stmt(&mut self) -> SyntaxNodeId {
        let mut children = vec![];
        while !self.at(&TokenKind::Semi) && !self.at(&TokenKind::Eof) {
            children.push(self.advance_element());
        }
        if self.at(&TokenKind::Semi) {
            children.push(self.advance_element());
        }
        self.builder.push_error_node(children)
    }

    pub(crate) fn tokens(&self) -> &[Token] {
        self.lexed.tokens()
    }

    pub(crate) fn peek(&self) -> &Token {
        self.tokens()
            .get(self.pos)
            .unwrap_or_else(|| self.tokens().last().expect("lexer emits EOF token"))
    }

    pub(crate) fn nth_kind(&self, offset: usize) -> &TokenKind {
        &self.nth(offset).kind
    }

    pub(crate) fn nth(&self, offset: usize) -> &Token {
        self.tokens()
            .get(self.pos + offset)
            .unwrap_or_else(|| self.tokens().last().expect("lexer emits EOF token"))
    }

    pub(crate) fn peek_kind(&self) -> &TokenKind {
        &self.peek().kind
    }

    pub(crate) fn at(&self, expected: &TokenKind) -> bool {
        same_kind(self.peek_kind(), expected)
    }

    pub(crate) fn at_any(&self, expected: &[TokenKind]) -> bool {
        expected.iter().any(|kind| self.at(kind))
    }

    pub(crate) fn advance(&mut self) -> &Token {
        let index = self.pos;
        if !self.at(&TokenKind::Eof) {
            self.pos += 1;
        }
        self.tokens()
            .get(index)
            .unwrap_or_else(|| self.tokens().last().expect("lexer emits EOF token"))
    }

    pub(crate) fn advance_element(&mut self) -> SyntaxElementId {
        let token = self.advance().clone();
        self.builder.push_lex_element(&token)
    }

    pub(crate) fn eat(&mut self, expected: &TokenKind) -> Option<SyntaxElementId> {
        self.at(expected).then(|| self.advance_element())
    }

    pub(crate) fn expect_token(&mut self, expected: &TokenKind) -> ParseResult<SyntaxElementId> {
        if self.at(expected) {
            return Ok(self.advance_element());
        }

        Err(ParseError {
            kind: ParseErrorKind::ExpectedToken {
                expected: Box::new(expected.clone()),
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        })
    }

    pub(crate) fn span(&self) -> Span {
        self.peek().span
    }

    pub(crate) fn expect_ident_element(&mut self) -> ParseResult<SyntaxElementId> {
        match self.peek_kind() {
            TokenKind::Ident | TokenKind::EscapedIdent => Ok(self.advance_element()),
            _ => Err(ParseError {
                kind: ParseErrorKind::ExpectedIdentifier {
                    found: Box::new(self.found_token()),
                },
                span: self.span(),
            }),
        }
    }

    pub(crate) fn expect_string_element(&mut self) -> ParseResult<SyntaxElementId> {
        if self.at(&TokenKind::StringLit) {
            Ok(self.advance_element())
        } else {
            Err(ParseError {
                kind: ParseErrorKind::ExpectedStringLiteral {
                    found: Box::new(self.found_token()),
                },
                span: self.span(),
            })
        }
    }

    pub(crate) fn expected_expression(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedExpression {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_pattern(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedPattern {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_type(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedType {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_member(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedMember {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_splice_target(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedSpliceTarget {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_operator_member_name(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedOperatorMemberName {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_effect_item(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedEffectItem {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_foreign_binding(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedForeignBinding {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_array_dimension(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedArrayDimension {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_field_target(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedFieldTarget {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn expected_constraint_operator(&self) -> ParseError {
        ParseError {
            kind: ParseErrorKind::ExpectedConstraintOperator {
                found: Box::new(self.found_token()),
            },
            span: self.span(),
        }
    }

    pub(crate) fn found_token(&self) -> TokenKind {
        self.peek_kind().clone()
    }

    pub(crate) fn error(&mut self, error: ParseError) {
        self.errors.push(error);
    }

    pub(crate) fn synchronize_stmt(&mut self) {
        while !self.at_any(&[
            TokenKind::Semi,
            TokenKind::RParen,
            TokenKind::RBracket,
            TokenKind::RBrace,
            TokenKind::Pipe,
            TokenKind::Eof,
        ]) {
            let _ = self.advance();
        }
        if self.at(&TokenKind::Semi) {
            let _ = self.advance();
        }
    }

    pub(crate) fn parse_expr_list(&mut self, close: &TokenKind) -> ParseResult<SyntaxElementIds> {
        let mut children = vec![];
        if self.at(close) {
            return Ok(children);
        }
        loop {
            let expr = self.parse_expr(0)?;
            children.push(SyntaxElementId::Node(expr));
            if let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                if self.at(close) {
                    break;
                }
                continue;
            }
            break;
        }
        Ok(children)
    }

    pub(crate) fn parse_ident_list_opt(&mut self, close: &TokenKind) -> SyntaxElementIds {
        let mut children = vec![];
        if self.at(close) {
            return children;
        }

        // Allow leading separators for better recovery/IDE tolerance.
        while let Some(comma) = self.eat(&TokenKind::Comma) {
            children.push(comma);
        }

        if self.at(close) {
            // Reject comma-only lists (e.g. `f(,)`) as syntactically invalid.
            self.error(ParseError {
                kind: ParseErrorKind::ExpectedIdentifier {
                    found: Box::new(self.found_token()),
                },
                span: self.span(),
            });
            return children;
        }

        loop {
            if matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
                children.push(self.advance_element());
            } else {
                self.error(ParseError {
                    kind: ParseErrorKind::ExpectedIdentifier {
                        found: Box::new(self.found_token()),
                    },
                    span: self.span(),
                });

                // Recover inside a list: skip until the next separator or close.
                while !self.at_any(&[TokenKind::Comma, close.clone(), TokenKind::Eof]) {
                    children.push(self.advance_element());
                }
            }

            let mut comma_count = 0usize;
            let mut extra_span = None;
            while self.at(&TokenKind::Comma) {
                comma_count += 1;
                if comma_count == 2 {
                    extra_span = Some(self.span());
                }
                children.push(self.advance_element());
            }

            if comma_count == 0 || self.at(close) {
                break;
            }

            // Multiple separators between items are an error (but we still parse the next item).
            if comma_count > 1 {
                self.error(ParseError {
                    kind: ParseErrorKind::ExpectedIdentifier {
                        found: Box::new(TokenKind::Comma),
                    },
                    span: extra_span.expect("count > 1"),
                });
            }

            // Allow leading commas before the next item as well.
            while let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
            }

            if self.at(close) {
                self.error(ParseError {
                    kind: ParseErrorKind::ExpectedIdentifier {
                        found: Box::new(self.found_token()),
                    },
                    span: self.span(),
                });
                break;
            }
        }
        children
    }

    pub(crate) fn wrap_list(
        &mut self,
        kind: SyntaxNodeKind,
        open: SyntaxElementId,
        mut inner: SyntaxElementIds,
        close: SyntaxElementId,
    ) -> SyntaxNodeId {
        let mut children = vec![open];
        children.append(&mut inner);
        children.push(close);
        self.builder.push_node_from_children(kind, children)
    }

    pub(crate) fn parse_op_or_ident_name(&mut self) -> ParseResult<SyntaxElementIds> {
        if matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
            return Ok(vec![self.advance_element()]);
        }

        if !self.at(&TokenKind::LParen) {
            return Err(self.expected_operator_member_name());
        }

        let mut children = vec![self.advance_element()];
        if matches!(
            self.peek_kind(),
            TokenKind::SymOp
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Eq
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::Amp
                | TokenKind::Caret
                | TokenKind::Tilde
        ) {
            children.push(self.advance_element());
        } else {
            return Err(self.expected_operator_member_name());
        }
        children.push(self.expect_token(&TokenKind::RParen)?);
        Ok(children)
    }
}

fn same_kind(left: &TokenKind, right: &TokenKind) -> bool {
    mem::discriminant(left) == mem::discriminant(right)
}

fn compute_lparen_matches(tokens: &[Token]) -> Vec<Option<usize>> {
    let mut matches = vec![None; tokens.len()];
    let mut stack = Vec::new();

    for (index, token) in tokens.iter().enumerate() {
        match token.kind {
            TokenKind::LParen => stack.push(index),
            TokenKind::RParen => {
                if let Some(open) = stack.pop() {
                    matches[open] = Some(index);
                }
            }
            _ => {}
        }
    }

    matches
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
