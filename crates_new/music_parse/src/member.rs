use music_ast::{SyntaxElementId, SyntaxNodeId, SyntaxNodeKind};
use music_lex::TokenKind;

use super::*;
use crate::parser::Parser;

impl Parser<'_, '_> {
    pub(crate) fn parse_member(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::KwLet => self.parse_let_member(),
            TokenKind::KwLaw => self.parse_law_member(),
            _ => Err(self.expected_member()),
        }
    }

    fn parse_let_member(&mut self) -> ParseResult<SyntaxNodeId> {
        let let_kw = self.expect_token(&TokenKind::KwLet)?;
        let mut children = vec![let_kw];
        children.extend(self.parse_op_or_ident_name()?);
        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            let params = self.parse_param_list_contents(&TokenKind::RParen)?;
            let close = self.expect_token(&TokenKind::RParen)?;
            let list = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
            children.push(SyntaxElementId::Node(list));
        }
        if let Some(colon) = self.eat(&TokenKind::Colon) {
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_ty()?));
        }
        if let Some(bind) = self.eat(&TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Member, children))
    }

    fn parse_law_member(&mut self) -> ParseResult<SyntaxNodeId> {
        let law_kw = self.expect_token(&TokenKind::KwLaw)?;
        let mut children = vec![law_kw, self.expect_ident_element()?];
        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            let params = self.parse_param_list_contents(&TokenKind::RParen)?;
            let close = self.expect_token(&TokenKind::RParen)?;
            let list = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
            children.push(SyntaxElementId::Node(list));
        }
        let bind = self.expect_token(&TokenKind::ColonEq)?;
        children.push(bind);
        children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Member, children))
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
