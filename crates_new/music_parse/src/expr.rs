use core::mem;

use music_ast::{SyntaxElementId, SyntaxNodeId, SyntaxNodeKind};
use music_basic::Span;
use music_lex::{LexErrorKind, Lexer, Token, TokenKind};

use crate::parser::Parser;

use super::*;

const PREFIX_BP: u8 = 24;
const ASSIGN_BP: u8 = 2;

#[derive(Clone, Copy, PartialEq, Eq)]
enum InfixClass {
    Comparison,
    Other,
}

impl Parser<'_, '_, '_> {
    pub(crate) fn parse_expr(&mut self, min_bp: u8) -> ParseResult<SyntaxNodeId> {
        let mut left = self.parse_prefix_expr()?;

        loop {
            if let Some(next_left) = self.try_postfix(left)? {
                left = next_left;
                continue;
            }

            let Some((left_bp, right_bp, class)) = infix_binding_power(self.peek_kind()) else {
                break;
            };
            if left_bp < min_bp {
                break;
            }

            let op = self.advance_element();
            let right = self.parse_expr(right_bp)?;
            if class == InfixClass::Comparison && self.is_comparison_expr(left) {
                self.error(ParseError {
                    kind: ParseErrorKind::NonAssociativeChain,
                    span: self.span(),
                });
            }

            left = self.builder.push_node_from_children(
                SyntaxNodeKind::BinaryExpr,
                [
                    SyntaxElementId::Node(left),
                    op,
                    SyntaxElementId::Node(right),
                ],
            );
            if class == InfixClass::Comparison {
                self.comparison_exprs.push(left);
            }
        }

        Ok(left)
    }

    fn parse_prefix_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        if self.at_any(&[TokenKind::Minus, TokenKind::KwNot, TokenKind::KwMut]) {
            let op = self.advance_element();
            let operand = self.parse_expr(PREFIX_BP)?;
            return Ok(self.builder.push_node_from_children(
                SyntaxNodeKind::PrefixExpr,
                [op, SyntaxElementId::Node(operand)],
            ));
        }

        self.parse_atom_expr()
    }

    fn parse_atom_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::IntLit | TokenKind::FloatLit | TokenKind::StringLit | TokenKind::RuneLit => {
                Ok(self.parse_literal_expr())
            }
            TokenKind::FStringLit(_) => Ok(self.parse_fstring_expr()),
            TokenKind::Ident | TokenKind::EscapedIdent => self.parse_name_expr(),
            TokenKind::LParen => self.parse_paren_expr(),
            TokenKind::LBracket => self.parse_array_expr(),
            TokenKind::LBrace => self.parse_record_expr(),
            TokenKind::DotLBrace => {
                self.error(ParseError {
                    kind: ParseErrorKind::RecordLiteralUsesDotBrace,
                    span: self.span(),
                });
                self.parse_record_expr_dot_brace()
            }
            TokenKind::Dot => self.parse_dot_prefix_expr(),
            TokenKind::KwCase => self.parse_case_expr(),
            TokenKind::KwLet => self.parse_let_expr_required_body(vec![]),
            TokenKind::KwResume => self.parse_resume_expr(),
            TokenKind::KwImport => self.parse_import_expr(),
            TokenKind::KwData => self.parse_data_expr(),
            TokenKind::KwEffect => self.parse_effect_expr(),
            TokenKind::KwClass => self.parse_class_expr(),
            TokenKind::KwInstance => self.parse_instance_expr(vec![]),
            TokenKind::KwPerform => self.parse_perform_expr(),
            TokenKind::KwHandle => self.parse_handle_expr(),
            TokenKind::KwForeign => self.parse_foreign_expr(vec![]),
            TokenKind::KwQuote => self.parse_quote_expr(),
            TokenKind::Hash | TokenKind::SpliceLParen | TokenKind::SpliceLBracket => {
                self.parse_splice_expr()
            }
            TokenKind::At => self.parse_with_attrs_expr(),
            TokenKind::KwExport => self.parse_export_expr(),
            _ => Err(self.expected_expression()),
        }
    }

    fn parse_literal_expr(&mut self) -> SyntaxNodeId {
        let literal = self.advance_element();
        self.builder
            .push_node_from_children(SyntaxNodeKind::LiteralExpr, [literal])
    }

    fn parse_fstring_expr(&mut self) -> SyntaxNodeId {
        let token = self.peek().clone();
        let parts = match token.kind.clone() {
            TokenKind::FStringLit(parts) => parts,
            _ => {
                let fallback = self.advance_element();
                return self
                    .builder
                    .push_node_from_children(SyntaxNodeKind::LiteralExpr, [fallback]);
            }
        };

        let token_el = self.advance_element();
        let mut children = vec![token_el];

        for part in parts {
            if part.kind != music_lex::FStringPartKind::Interpolation {
                continue;
            }

            let expr = self.parse_fstring_interpolation_expr(part.span);
            children.push(SyntaxElementId::Node(expr));
        }

        self.builder
            .push_node_from_children(SyntaxNodeKind::FStringExpr, children)
    }

    fn parse_fstring_interpolation_expr(&mut self, span: Span) -> SyntaxNodeId {
        let start = usize::try_from(span.start).unwrap_or(0);
        let end = usize::try_from(span.end).unwrap_or(start);
        let Some(text) = self.source.get(start..end) else {
            self.error(ParseError {
                kind: ParseErrorKind::InvalidFStringInterpolation {
                    kind: Box::new(LexErrorKind::UnexpectedChar('\0')),
                },
                span,
            });
            return self.builder.push_error_node([]);
        };

        let sub = Lexer::new(text).lex();
        let base = span.start;

        for err in sub.errors().iter().cloned() {
            self.error(ParseError {
                kind: ParseErrorKind::InvalidFStringInterpolation {
                    kind: Box::new(err.kind),
                },
                span: offset_span(err.span, base),
            });
        }

        let mut tokens: Vec<Token> = sub.tokens().iter().cloned().collect();
        for token in &mut tokens {
            offset_token(token, base);
        }

        let mut parser = Parser::new(
            self.source_id,
            self.source,
            &tokens,
            &mut *self.builder,
            &mut *self.errors,
        );

        match parser.parse_expr(0) {
            Ok(expr) => {
                if !parser.at(&TokenKind::Eof) {
                    parser.error(ParseError {
                        kind: ParseErrorKind::ExpectedToken {
                            expected: Box::new(TokenKind::Eof),
                            found: Box::new(parser.found_token()),
                        },
                        span: parser.span(),
                    });
                }
                expr
            }
            Err(error) => {
                parser.error(error);
                let mut junk = vec![];
                while !parser.at(&TokenKind::Eof) {
                    junk.push(parser.advance_element());
                }
                parser.builder.push_error_node(junk)
            }
        }
    }

    fn parse_name_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let ident = self.expect_ident_element()?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::NameExpr, [ident]))
    }

    fn parse_paren_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        if self.is_lambda_paren() {
            return self.parse_lambda_expr();
        }

        let open = self.expect_token(&TokenKind::LParen)?;
        if self.at(&TokenKind::RParen) {
            let close = self.advance_element();
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::TupleExpr, [open, close]));
        }
        if self.at(&TokenKind::Comma) {
            let comma = self.advance_element();
            let close = self.expect_token(&TokenKind::RParen)?;
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::TupleExpr, [open, comma, close]));
        }
        if self.at(&TokenKind::Semi) {
            let semi = self.advance_element();
            let close = self.expect_token(&TokenKind::RParen)?;
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::SequenceExpr, [open, semi, close]));
        }

        let first = self.parse_expr(0)?;
        if self.at(&TokenKind::Comma) {
            let mut children = vec![open, SyntaxElementId::Node(first)];
            while let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                if self.at(&TokenKind::RParen) {
                    break;
                }
                let expr = self.parse_expr(0)?;
                children.push(SyntaxElementId::Node(expr));
            }
            let close = self.expect_token(&TokenKind::RParen)?;
            children.push(close);
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::TupleExpr, children));
        }
        if self.at(&TokenKind::Semi) {
            let mut children = vec![open, SyntaxElementId::Node(first)];
            while let Some(semi) = self.eat(&TokenKind::Semi) {
                children.push(semi);
                if self.at(&TokenKind::RParen) {
                    break;
                }
                let expr = self.parse_expr(0)?;
                children.push(SyntaxElementId::Node(expr));
            }
            let close = self.expect_token(&TokenKind::RParen)?;
            children.push(close);
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::SequenceExpr, children));
        }

        let close = self.expect_token(&TokenKind::RParen)?;
        Ok(self.rewrap_node(first, [open, SyntaxElementId::Node(first), close]))
    }

    fn parse_lambda_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LParen)?;
        let params = self.parse_param_list_contents(&TokenKind::RParen)?;
        let close = self.expect_token(&TokenKind::RParen)?;
        let param_list = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);

        let mut children = vec![SyntaxElementId::Node(param_list)];
        if let Some(colon) = self.eat(&TokenKind::Colon) {
            children.push(colon);
            let ret = self.parse_ty()?;
            children.push(SyntaxElementId::Node(ret));
        }
        let arrow = self.expect_token(&TokenKind::EqGt)?;
        children.push(arrow);
        let body = self.parse_expr(0)?;
        children.push(SyntaxElementId::Node(body));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::LambdaExpr, children))
    }

    fn parse_array_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LBracket)?;
        let mut children = vec![open];
        while !self.at(&TokenKind::RBracket) && !self.at(&TokenKind::Eof) {
            if let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                continue;
            }
            let item = self.parse_array_item()?;
            children.push(SyntaxElementId::Node(item));
        }
        let close = self.expect_token(&TokenKind::RBracket)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ArrayExpr, children))
    }

    fn parse_array_item(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![];
        if let Some(spread) = self.eat(&TokenKind::DotDotDot) {
            children.push(spread);
        }
        let expr = self.parse_expr(0)?;
        children.push(SyntaxElementId::Node(expr));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ArrayItem, children))
    }

    fn parse_record_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LBrace)?;
        let mut children = vec![open];
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            if let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                continue;
            }
            let item = self.parse_record_item()?;
            children.push(SyntaxElementId::Node(item));
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordExpr, children))
    }

    fn parse_record_expr_dot_brace(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::DotLBrace)?;
        let mut children = vec![open];
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            if let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                continue;
            }
            let item = self.parse_record_item()?;
            children.push(SyntaxElementId::Node(item));
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordExpr, children))
    }

    fn parse_record_item(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![];
        if let Some(spread) = self.eat(&TokenKind::DotDotDot) {
            children.push(spread);
            let expr = self.parse_expr(0)?;
            children.push(SyntaxElementId::Node(expr));
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::RecordItem, children));
        }
        let ident = self.expect_ident_element()?;
        children.push(ident);
        if let Some(assign) = self.eat(&TokenKind::ColonEq) {
            children.push(assign);
            let expr = self.parse_expr(0)?;
            children.push(SyntaxElementId::Node(expr));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordItem, children))
    }

    fn parse_dot_prefix_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let dot = self.expect_token(&TokenKind::Dot)?;
        let ident = self.expect_ident_element()?;
        let mut children = vec![dot, ident];
        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            children.extend(self.parse_expr_list(&TokenKind::RParen)?);
            let close = self.expect_token(&TokenKind::RParen)?;
            children.push(close);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::VariantExpr, children))
    }

    fn parse_case_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let case_kw = self.expect_token(&TokenKind::KwCase)?;
        let scrutinee = self.parse_expr(0)?;
        let of_kw = self.expect_token(&TokenKind::KwOf)?;
        let open = self.expect_token(&TokenKind::LParen)?;
        let mut children = vec![case_kw, SyntaxElementId::Node(scrutinee), of_kw, open];
        if let Some(pipe) = self.eat(&TokenKind::Pipe) {
            children.push(pipe);
        }
        while !self.at(&TokenKind::RParen) && !self.at(&TokenKind::Eof) {
            children.extend(self.parse_case_arm()?);
            if let Some(pipe) = self.eat(&TokenKind::Pipe) {
                children.push(pipe);
                if self.at(&TokenKind::RParen) {
                    break;
                }
                continue;
            }
            break;
        }
        let close = self.expect_token(&TokenKind::RParen)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::CaseExpr, children))
    }

    fn parse_case_arm(&mut self) -> ParseResult<Vec<SyntaxElementId>> {
        let mut children = self.parse_attrs()?;
        let pat = self.parse_pat()?;
        children.push(SyntaxElementId::Node(pat));
        if let Some(if_kw) = self.eat(&TokenKind::KwIf) {
            children.push(if_kw);
            let guard = self.parse_expr(0)?;
            children.push(SyntaxElementId::Node(guard));
        }
        let arrow = self.expect_token(&TokenKind::EqGt)?;
        children.push(arrow);
        let body = self.parse_expr(0)?;
        children.push(SyntaxElementId::Node(body));
        Ok(children)
    }

    fn parse_resume_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let resume = self.expect_token(&TokenKind::KwResume)?;
        let mut children = vec![resume];
        if !self.at_any(&[
            TokenKind::Semi,
            TokenKind::RParen,
            TokenKind::Pipe,
            TokenKind::Eof,
        ]) {
            let expr = self.parse_expr(0)?;
            children.push(SyntaxElementId::Node(expr));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ResumeExpr, children))
    }

    fn parse_perform_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let perform = self.expect_token(&TokenKind::KwPerform)?;
        let expr = self.parse_expr(PREFIX_BP)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::PerformExpr,
            [perform, SyntaxElementId::Node(expr)],
        ))
    }

    fn parse_handle_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let handle = self.expect_token(&TokenKind::KwHandle)?;
        let expr = self.parse_expr(0)?;
        let with_kw = self.expect_token(&TokenKind::KwWith)?;
        let binder = self.expect_ident_element()?;
        let of_kw = self.expect_token(&TokenKind::KwOf)?;
        let open = self.expect_token(&TokenKind::LParen)?;
        let mut children = vec![
            handle,
            SyntaxElementId::Node(expr),
            with_kw,
            binder,
            of_kw,
            open,
        ];
        if let Some(pipe) = self.eat(&TokenKind::Pipe) {
            children.push(pipe);
        }
        while !self.at(&TokenKind::RParen) && !self.at(&TokenKind::Eof) {
            let clause = self.parse_handle_clause()?;
            children.push(SyntaxElementId::Node(clause));
            if let Some(pipe) = self.eat(&TokenKind::Pipe) {
                children.push(pipe);
                if self.at(&TokenKind::RParen) {
                    break;
                }
                continue;
            }
            break;
        }
        let close = self.expect_token(&TokenKind::RParen)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::HandleExpr, children))
    }

    fn parse_handle_clause(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![self.expect_ident_element()?];
        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            children.extend(self.parse_ident_list_opt(&TokenKind::RParen));
            let close = self.expect_token(&TokenKind::RParen)?;
            children.push(close);
        }
        let arrow = self.expect_token(&TokenKind::EqGt)?;
        children.push(arrow);
        let body = self.parse_expr(0)?;
        children.push(SyntaxElementId::Node(body));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::HandlerClause, children))
    }

    fn parse_quote_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let quote = self.expect_token(&TokenKind::KwQuote)?;
        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            let expr = self.parse_expr(0)?;
            let close = self.expect_token(&TokenKind::RParen)?;
            return Ok(self.builder.push_node_from_children(
                SyntaxNodeKind::QuoteExpr,
                [quote, open, SyntaxElementId::Node(expr), close],
            ));
        }

        let open = self.expect_token(&TokenKind::LBrace)?;
        let mut children = vec![quote, open];
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            let stmt = self.parse_stmt()?;
            children.push(SyntaxElementId::Node(stmt));
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::QuoteExpr, children))
    }

    fn parse_splice_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::Hash => {
                let hash = self.advance_element();
                let target = self
                    .expect_ident_element()
                    .map_err(|_| self.expected_splice_target())?;
                Ok(self
                    .builder
                    .push_node_from_children(SyntaxNodeKind::SpliceExpr, [hash, target]))
            }
            TokenKind::SpliceLParen => {
                let open = self.advance_element();
                let expr = self.parse_expr(0)?;
                let close = self.expect_token(&TokenKind::RParen)?;
                Ok(self.builder.push_node_from_children(
                    SyntaxNodeKind::SpliceExpr,
                    [open, SyntaxElementId::Node(expr), close],
                ))
            }
            TokenKind::SpliceLBracket => {
                let open = self.advance_element();
                let mut children = vec![open];
                if self.at(&TokenKind::RBracket) {
                    self.error(self.expected_expression());
                } else {
                    children.extend(self.parse_expr_list(&TokenKind::RBracket)?);
                }
                let close = self.expect_token(&TokenKind::RBracket)?;
                children.push(close);
                Ok(self
                    .builder
                    .push_node_from_children(SyntaxNodeKind::SpliceExpr, children))
            }
            _ => Err(self.expected_splice_target()),
        }
    }

    fn try_postfix(&mut self, left: SyntaxNodeId) -> ParseResult<Option<SyntaxNodeId>> {
        let kind = self.peek_kind();
        if same_kind(kind, &TokenKind::LParen) {
            return self.parse_call_expr(left).map(Some);
        }
        if same_kind(kind, &TokenKind::DotLBracket) {
            return self.parse_index_expr(left).map(Some);
        }
        if same_kind(kind, &TokenKind::DotLBrace) {
            return self.parse_record_update_expr(left).map(Some);
        }
        if self.at_any(&[TokenKind::Dot, TokenKind::QuestionDot, TokenKind::BangDot]) {
            return self.parse_field_expr(left).map(Some);
        }
        if self.at(&TokenKind::ColonQuestion) {
            return self.parse_type_test_expr(left).map(Some);
        }
        if self.at(&TokenKind::ColonQuestionGt) {
            return self.parse_type_cast_expr(left).map(Some);
        }
        Ok(None)
    }

    fn parse_call_expr(&mut self, callee: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LParen)?;
        let mut children = vec![SyntaxElementId::Node(callee), open];
        children.extend(self.parse_arg_list()?);
        let close = self.expect_token(&TokenKind::RParen)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::CallExpr, children))
    }

    fn parse_index_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::DotLBracket)?;
        let mut children = vec![SyntaxElementId::Node(base), open];
        children.extend(self.parse_expr_list(&TokenKind::RBracket)?);
        let close = self.expect_token(&TokenKind::RBracket)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::IndexExpr, children))
    }

    fn parse_record_update_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::DotLBrace)?;
        let mut children = vec![SyntaxElementId::Node(base), open];
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            if let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                continue;
            }
            let field = self.parse_record_item()?;
            children.push(SyntaxElementId::Node(field));
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordUpdateExpr, children))
    }

    fn parse_arg_list(&mut self) -> ParseResult<Vec<SyntaxElementId>> {
        let mut children = vec![];
        if self.at(&TokenKind::RParen) {
            return Ok(children);
        }

        let arg = self.parse_arg()?;
        children.push(SyntaxElementId::Node(arg));
        while let Some(comma) = self.eat(&TokenKind::Comma) {
            children.push(comma);
            if self.at(&TokenKind::RParen) {
                return Err(self.expected_expression());
            }
            let next = self.parse_arg()?;
            children.push(SyntaxElementId::Node(next));
        }
        Ok(children)
    }

    fn parse_arg(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![];
        if let Some(spread) = self.eat(&TokenKind::DotDotDot) {
            children.push(spread);
            let expr = self.parse_expr(0)?;
            children.push(SyntaxElementId::Node(expr));
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::Arg, children));
        }

        let expr = self.parse_expr(0)?;
        children.push(SyntaxElementId::Node(expr));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Arg, children))
    }

    fn parse_field_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let access = self.advance_element();
        let target = match self.peek_kind() {
            TokenKind::Ident | TokenKind::EscapedIdent | TokenKind::IntLit => {
                self.advance_element()
            }
            _ => return Err(self.expected_field_target()),
        };
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::FieldExpr,
            [SyntaxElementId::Node(base), access, target],
        ))
    }

    fn parse_type_test_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let op = self.expect_token(&TokenKind::ColonQuestion)?;
        let ty = self.parse_named_ty()?;
        let mut children = vec![SyntaxElementId::Node(base), op, SyntaxElementId::Node(ty)];
        if let Some(as_kw) = self.eat(&TokenKind::KwAs) {
            children.push(as_kw);
            children.push(self.expect_ident_element()?);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::TypeTestExpr, children))
    }

    fn parse_type_cast_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let op = self.expect_token(&TokenKind::ColonQuestionGt)?;
        let ty = self.parse_named_ty()?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::TypeCastExpr,
            [SyntaxElementId::Node(base), op, SyntaxElementId::Node(ty)],
        ))
    }

    fn is_lambda_paren(&self) -> bool {
        if !self.at(&TokenKind::LParen) {
            return false;
        }

        let Some(close) = self.lparen_match.get(self.pos).copied().flatten() else {
            return false;
        };
        let next = self.tokens().get(close + 1);
        next.is_some_and(|token| {
            same_kind(&token.kind, &TokenKind::EqGt) || same_kind(&token.kind, &TokenKind::Colon)
        })
    }

    fn rewrap_node(
        &mut self,
        node: SyntaxNodeId,
        children: impl IntoIterator<Item = SyntaxElementId>,
    ) -> SyntaxNodeId {
        let kind = self.builder.node_kind(node);
        self.builder.push_node_from_children(kind, children)
    }

    fn is_comparison_expr(&self, node: SyntaxNodeId) -> bool {
        self.comparison_exprs.contains(&node)
    }
}

const fn infix_binding_power(kind: &TokenKind) -> Option<(u8, u8, InfixClass)> {
    match kind {
        TokenKind::LtMinus => Some((1, ASSIGN_BP, InfixClass::Other)),
        TokenKind::PipeGt => Some((4, 5, InfixClass::Other)),
        TokenKind::KwOr => Some((6, 7, InfixClass::Other)),
        TokenKind::KwXor => Some((8, 9, InfixClass::Other)),
        TokenKind::KwAnd => Some((10, 11, InfixClass::Other)),
        TokenKind::Eq
        | TokenKind::SlashEq
        | TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::LtEq
        | TokenKind::GtEq => Some((12, 13, InfixClass::Comparison)),
        TokenKind::KwShl | TokenKind::KwShr => Some((14, 15, InfixClass::Other)),
        TokenKind::Plus | TokenKind::Minus => Some((16, 17, InfixClass::Other)),
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
            Some((18, 19, InfixClass::Other))
        }
        TokenKind::SymOp | TokenKind::Amp | TokenKind::Caret | TokenKind::Tilde => {
            Some((20, 21, InfixClass::Other))
        }
        _ => None,
    }
}

fn same_kind(left: &TokenKind, right: &TokenKind) -> bool {
    mem::discriminant(left) == mem::discriminant(right)
}

fn offset_span(span: Span, base: u32) -> Span {
    Span::new(span.start + base, span.end + base)
}

fn offset_token(token: &mut Token, base: u32) {
    token.span = offset_span(token.span, base);
    for trivia in token
        .leading_trivia
        .iter_mut()
        .chain(token.trailing_trivia.iter_mut())
    {
        trivia.span = offset_span(trivia.span, base);
    }
    if let TokenKind::FStringLit(parts) = &mut token.kind {
        for part in parts.iter_mut() {
            part.span = offset_span(part.span, base);
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
