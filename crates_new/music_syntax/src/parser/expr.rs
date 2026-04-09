use super::*;

#[derive(Clone, Copy, PartialEq, Eq)]
enum InfixClass {
    Comparison,
    Other,
}

impl Parser<'_> {
    pub(super) fn parse_expr(&mut self, min_bp: u8) -> ParseResult<SyntaxNodeId> {
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
                vec![
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

    pub(super) fn parse_expr_without_colon_eq(&mut self, min_bp: u8) -> ParseResult<SyntaxNodeId> {
        let mut left = self.parse_prefix_expr()?;
        loop {
            if let Some(next_left) = self.try_postfix(left)? {
                left = next_left;
                continue;
            }
            let Some((left_bp, right_bp, class)) =
                infix_binding_power_without_colon_eq(self.peek_kind())
            else {
                break;
            };
            if left_bp < min_bp {
                break;
            }
            let op = self.advance_element();
            let right = self.parse_expr_without_colon_eq(right_bp)?;
            if class == InfixClass::Comparison && self.is_comparison_expr(left) {
                self.error(ParseError {
                    kind: ParseErrorKind::NonAssociativeChain,
                    span: self.span(),
                });
            }
            left = self.builder.push_node_from_children(
                SyntaxNodeKind::BinaryExpr,
                vec![
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
                vec![op, SyntaxElementId::Node(operand)],
            ));
        }
        self.parse_atom_expr()
    }

    fn try_postfix(&mut self, left: SyntaxNodeId) -> ParseResult<Option<SyntaxNodeId>> {
        if self.at(TokenKind::LParen) {
            return self.parse_call_expr(left).map(Some);
        }
        if self.at(TokenKind::LBracket) {
            return self.parse_apply_expr(left).map(Some);
        }
        if self.at(TokenKind::DotLBracket) {
            return self.parse_index_expr(left).map(Some);
        }
        if self.at(TokenKind::DotLBrace) {
            return self.parse_record_update_expr(left).map(Some);
        }
        if self.at(TokenKind::Dot) {
            return self.parse_field_expr(left).map(Some);
        }
        if self.at(TokenKind::ColonQuestion) {
            return self.parse_type_test_expr(left).map(Some);
        }
        if self.at(TokenKind::ColonQuestionGt) {
            return self.parse_type_cast_expr(left).map(Some);
        }
        Ok(None)
    }

    fn parse_call_expr(&mut self, callee: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LParen)?;
        let mut children = vec![SyntaxElementId::Node(callee), open];
        children.extend(self.parse_separated_nodes(
            TokenKind::Comma,
            TokenKind::RParen,
            Parser::parse_arg,
        )?);
        children.push(self.expect_token(TokenKind::RParen)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::CallExpr, children))
    }

    fn parse_arg(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = Vec::new();
        if let Some(spread) = self.eat(TokenKind::DotDotDot) {
            children.push(spread);
        }
        children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Arg, children))
    }

    fn parse_apply_expr(&mut self, callee: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LBracket)?;
        let mut children = vec![SyntaxElementId::Node(callee), open];
        children.extend(self.parse_separated_nodes(
            TokenKind::Comma,
            TokenKind::RBracket,
            Parser::parse_expr_node,
        )?);
        children.push(self.expect_token(TokenKind::RBracket)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ApplyExpr, children))
    }

    fn parse_index_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::DotLBracket)?;
        let mut children = vec![SyntaxElementId::Node(base), open];
        children.extend(self.parse_separated_nodes(
            TokenKind::Comma,
            TokenKind::RBracket,
            Parser::parse_expr_node,
        )?);
        children.push(self.expect_token(TokenKind::RBracket)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::IndexExpr, children))
    }

    fn parse_record_update_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::DotLBrace)?;
        let mut children = vec![SyntaxElementId::Node(base), open];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            if let Some(comma) = self.eat(TokenKind::Comma) {
                children.push(comma);
                continue;
            }
            children.push(SyntaxElementId::Node(self.parse_record_item()?));
        }
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordUpdateExpr, children))
    }

    fn parse_field_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let access = self.advance_element();
        let target = match self.peek_kind() {
            TokenKind::Ident | TokenKind::Int => self.advance_element(),
            _ => return Err(self.expected_field_target()),
        };
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::FieldExpr,
            vec![SyntaxElementId::Node(base), access, target],
        ))
    }

    fn parse_type_test_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let op = self.expect_token(TokenKind::ColonQuestion)?;
        let ty = self.parse_expr(ARROW_BP)?;
        let mut children = vec![SyntaxElementId::Node(base), op, SyntaxElementId::Node(ty)];
        if let Some(as_kw) = self.eat(TokenKind::KwAs) {
            children.push(as_kw);
            children.push(self.expect_ident_element()?);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::TypeTestExpr, children))
    }

    fn parse_type_cast_expr(&mut self, base: SyntaxNodeId) -> ParseResult<SyntaxNodeId> {
        let op = self.expect_token(TokenKind::ColonQuestionGt)?;
        let ty = self.parse_expr(ARROW_BP)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::TypeCastExpr,
            vec![SyntaxElementId::Node(base), op, SyntaxElementId::Node(ty)],
        ))
    }
}

const fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8, InfixClass)> {
    match kind {
        TokenKind::ColonEq => Some((1, ASSIGN_BP, InfixClass::Other)),
        TokenKind::PipeGt => Some((PIPE_BP, PIPE_BP + 1, InfixClass::Other)),
        TokenKind::MinusGt | TokenKind::TildeGt => Some((ARROW_BP, ARROW_BP, InfixClass::Other)),
        TokenKind::KwOr => Some((OR_BP, OR_BP + 1, InfixClass::Other)),
        TokenKind::KwXor => Some((XOR_BP, XOR_BP + 1, InfixClass::Other)),
        TokenKind::KwAnd => Some((AND_BP, AND_BP + 1, InfixClass::Other)),
        TokenKind::Eq
        | TokenKind::SlashEq
        | TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::LtEq
        | TokenKind::GtEq
        | TokenKind::KwIn => Some((COMPARE_BP, COMPARE_BP + 1, InfixClass::Comparison)),
        TokenKind::KwShl | TokenKind::KwShr => Some((SHIFT_BP, SHIFT_BP + 1, InfixClass::Other)),
        TokenKind::Plus | TokenKind::Minus => Some((ADD_BP, ADD_BP + 1, InfixClass::Other)),
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
            Some((MUL_BP, MUL_BP + 1, InfixClass::Other))
        }
        TokenKind::SymbolicOp => Some((SYMBOLIC_BP, SYMBOLIC_BP + 1, InfixClass::Other)),
        _ => None,
    }
}

const fn infix_binding_power_without_colon_eq(kind: TokenKind) -> Option<(u8, u8, InfixClass)> {
    if matches!(kind, TokenKind::ColonEq) {
        return None;
    }
    infix_binding_power(kind)
}
