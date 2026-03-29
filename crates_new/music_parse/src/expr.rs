use core::mem;

use music_ast::{SyntaxElementId, SyntaxNodeId, SyntaxNodeKind};
use music_lex::TokenKind;

use crate::parser::Parser;

use super::*;

const PREFIX_BP: u8 = 24;
const ASSIGN_BP: u8 = 2;

#[derive(Clone, Copy, PartialEq, Eq)]
enum InfixClass {
    Comparison,
    Other,
}

impl Parser<'_, '_> {
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
            TokenKind::IntLit
            | TokenKind::FloatLit
            | TokenKind::StringLit
            | TokenKind::FStringLit(_)
            | TokenKind::RuneLit => Ok(self.parse_literal_expr()),
            TokenKind::Ident | TokenKind::EscapedIdent => self.parse_name_expr(),
            TokenKind::LParen => self.parse_paren_expr(),
            TokenKind::LBracket => self.parse_array_expr(),
            TokenKind::DotLBrace => self.parse_record_expr(),
            TokenKind::Dot => self.parse_dot_prefix_expr(),
            TokenKind::KwCase => self.parse_case_expr(),
            TokenKind::KwLet => self.parse_let_expr(vec![]),
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
        if !self.at(&TokenKind::RBracket) {
            loop {
                let item = self.parse_array_item()?;
                children.push(SyntaxElementId::Node(item));
                if let Some(comma) = self.eat(&TokenKind::Comma) {
                    children.push(comma);
                    if self.at(&TokenKind::RBracket) {
                        break;
                    }
                    continue;
                }
                break;
            }
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
        let open = self.expect_token(&TokenKind::DotLBrace)?;
        let mut children = vec![open];
        if !self.at(&TokenKind::RBrace) {
            loop {
                let item = self.parse_record_item()?;
                children.push(SyntaxElementId::Node(item));
                if let Some(comma) = self.eat(&TokenKind::Comma) {
                    children.push(comma);
                    if self.at(&TokenKind::RBrace) {
                        break;
                    }
                    continue;
                }
                break;
            }
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
            children.extend(self.parse_ident_list(&TokenKind::RParen)?);
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
        children.extend(self.parse_expr_list(&TokenKind::RParen)?);
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
        if !self.at(&TokenKind::RBrace) {
            loop {
                let field = self.parse_record_item()?;
                children.push(SyntaxElementId::Node(field));
                if let Some(comma) = self.eat(&TokenKind::Comma) {
                    children.push(comma);
                    if self.at(&TokenKind::RBrace) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordUpdateExpr, children))
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

        let mut depth = 0_u32;
        for (index, token) in self.tokens()[self.pos..].iter().enumerate() {
            match token.kind {
                TokenKind::LParen => depth += 1,
                TokenKind::RParen => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        let next = self.tokens().get(self.pos + index + 1);
                        return next.is_some_and(|token| {
                            same_kind(&token.kind, &TokenKind::EqGt)
                                || same_kind(&token.kind, &TokenKind::Colon)
                        });
                    }
                }
                _ => {}
            }
        }
        false
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
        _ => None,
    }
}

fn same_kind(left: &TokenKind, right: &TokenKind) -> bool {
    mem::discriminant(left) == mem::discriminant(right)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
