use super::*;

impl Parser<'_> {
    pub(crate) fn parse_root_children(&mut self) -> SyntaxElementList {
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

    pub(crate) fn parse_fixity_directive(&mut self) -> SyntaxNodeParseResult {
        let kw = self.advance_element();
        let prec = self.expect_token(TokenKind::Int)?;
        let op = self.expect_token(TokenKind::OpIdent)?;
        let semi = self.expect_token(TokenKind::Semicolon)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::FixityDirective, vec![kw, prec, op, semi]))
    }

    pub(crate) fn parse_stmt(&mut self) -> SyntaxNodeParseResult {
        let expr = self.parse_expr(0)?;
        let semi = self.expect_token(TokenKind::Semicolon)?;
        Ok(self.node2(
            SyntaxNodeKind::SequenceExpr,
            SyntaxElementId::Node(expr),
            semi,
        ))
    }
}
