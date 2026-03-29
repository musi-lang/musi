use core::mem;

use music_ast::{SyntaxElementId, SyntaxNodeId, SyntaxNodeKind};
use music_lex::TokenKind;

use crate::errors::ParseResult;
use crate::parser::Parser;

impl Parser<'_, '_> {
    pub(crate) fn parse_ty(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_function_ty()
    }

    pub(crate) fn parse_named_ty(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_named_ty_base()
    }

    fn parse_function_ty(&mut self) -> ParseResult<SyntaxNodeId> {
        let left = self.parse_binary_ty(0)?;
        if self.at_any(&[TokenKind::MinusGt, TokenKind::TildeGt]) {
            let op = self.advance_element();
            let right = self.parse_function_ty()?;
            return Ok(self.builder.push_node_from_children(
                SyntaxNodeKind::FunctionTy,
                [
                    SyntaxElementId::Node(left),
                    op,
                    SyntaxElementId::Node(right),
                ],
            ));
        }
        Ok(left)
    }

    fn parse_binary_ty(&mut self, min_bp: u8) -> ParseResult<SyntaxNodeId> {
        let mut left = self.parse_ty_atom()?;
        while let Some((left_bp, right_bp)) = ty_binding_power(self.peek_kind()) {
            if left_bp < min_bp {
                break;
            }
            let op = self.advance_element();
            let right = self.parse_binary_ty(right_bp)?;
            left = self.builder.push_node_from_children(
                SyntaxNodeKind::BinaryTy,
                [
                    SyntaxElementId::Node(left),
                    op,
                    SyntaxElementId::Node(right),
                ],
            );
        }
        Ok(left)
    }

    fn parse_ty_atom(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::KwMut => self.parse_mut_ty(),
            TokenKind::Ident | TokenKind::EscapedIdent => self.parse_named_ty_base(),
            TokenKind::LParen => self.parse_paren_ty(),
            TokenKind::LBracket => self.parse_array_ty(),
            _ => Err(self.expected_type()),
        }
    }

    fn parse_mut_ty(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut_kw = self.expect_token(&TokenKind::KwMut)?;
        let inner = self.parse_ty_atom()?;
        Ok(self.rewrap_ty(inner, [mut_kw, SyntaxElementId::Node(inner)]))
    }

    fn parse_named_ty_base(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![];
        children.push(self.expect_ident_element()?);
        if self.at(&TokenKind::LBracket) {
            let open = self.advance_element();
            children.push(open);
            if !self.at(&TokenKind::RBracket) {
                children.push(SyntaxElementId::Node(self.parse_ty()?));
                while let Some(comma) = self.eat(&TokenKind::Comma) {
                    children.push(comma);
                    if self.at(&TokenKind::RBracket) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_ty()?));
                }
            }
            let close = self.expect_token(&TokenKind::RBracket)?;
            children.push(close);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::NamedTy, children))
    }

    fn parse_paren_ty(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LParen)?;

        if matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent)
            && same_kind(self.nth_kind(1), &TokenKind::Colon)
        {
            let ident = self.advance_element();
            let colon = self.advance_element();
            let param_ty = self.parse_ty()?;
            let close = self.expect_token(&TokenKind::RParen)?;
            if self.at(&TokenKind::MinusGt) {
                let arrow = self.advance_element();
                let ret = self.parse_ty()?;
                return Ok(self.builder.push_node_from_children(
                    SyntaxNodeKind::PiTy,
                    [
                        open,
                        ident,
                        colon,
                        SyntaxElementId::Node(param_ty),
                        close,
                        arrow,
                        SyntaxElementId::Node(ret),
                    ],
                ));
            }
            let grouped = self.rewrap_ty(param_ty, [open, SyntaxElementId::Node(param_ty), close]);
            return Ok(grouped);
        }

        if self.at(&TokenKind::RParen) {
            let close = self.advance_element();
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::TupleTy, [open, close]));
        }

        let first = self.parse_ty()?;
        if let Some(comma) = self.eat(&TokenKind::Comma) {
            let mut children = vec![open, SyntaxElementId::Node(first), comma];
            if !self.at(&TokenKind::RParen) {
                children.push(SyntaxElementId::Node(self.parse_ty()?));
                while let Some(next_comma) = self.eat(&TokenKind::Comma) {
                    children.push(next_comma);
                    if self.at(&TokenKind::RParen) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_ty()?));
                }
            }
            let close = self.expect_token(&TokenKind::RParen)?;
            children.push(close);
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::TupleTy, children));
        }

        let close = self.expect_token(&TokenKind::RParen)?;
        Ok(self.rewrap_ty(first, [open, SyntaxElementId::Node(first), close]))
    }

    fn parse_array_ty(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LBracket)?;
        let mut children = vec![open];
        if !self.at(&TokenKind::RBracket) {
            loop {
                match self.peek_kind() {
                    TokenKind::IntLit | TokenKind::Ident | TokenKind::EscapedIdent => {
                        children.push(self.advance_element());
                    }
                    _ => {
                        return Err(self.expected_array_dimension());
                    }
                }
                if let Some(comma) = self.eat(&TokenKind::Comma) {
                    children.push(comma);
                    continue;
                }
                break;
            }
        }
        let close = self.expect_token(&TokenKind::RBracket)?;
        children.push(close);
        let elem = self.parse_ty()?;
        children.push(SyntaxElementId::Node(elem));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ArrayTy, children))
    }

    fn rewrap_ty(
        &mut self,
        node: SyntaxNodeId,
        children: impl IntoIterator<Item = SyntaxElementId>,
    ) -> SyntaxNodeId {
        let kind = self.builder.node_kind(node);
        self.builder.push_node_from_children(kind, children)
    }
}

const fn ty_binding_power(kind: &TokenKind) -> Option<(u8, u8)> {
    match kind {
        TokenKind::Plus => Some((2, 3)),
        TokenKind::Star => Some((4, 5)),
        _ => None,
    }
}

fn same_kind(left: &TokenKind, right: &TokenKind) -> bool {
    mem::discriminant(left) == mem::discriminant(right)
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
