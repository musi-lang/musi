use super::*;

impl Parser<'_> {
    pub(crate) fn parse_root_children(&mut self) -> SyntaxElementList {
        let mut children = Vec::new();
        while !self.at(TokenKind::Eof) {
            let before = self.pos;
            let res = self.parse_stmt();
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
