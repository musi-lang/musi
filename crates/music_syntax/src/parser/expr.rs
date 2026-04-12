use super::*;

type SyntaxNodeParseResult = ParseResult<SyntaxNodeId>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum InfixClass {
    Comparison,
    Other,
}

impl Parser<'_> {
    pub(super) fn parse_expr(&mut self, min_bp: u8) -> SyntaxNodeParseResult {
        self.parse_binary_expr_with(min_bp, Self::parse_expr, infix_binding_power)
    }

    pub(super) fn parse_expr_without_colon_eq(&mut self, min_bp: u8) -> SyntaxNodeParseResult {
        self.parse_binary_expr_with(
            min_bp,
            Self::parse_expr_without_colon_eq,
            infix_binding_power_without_colon_eq,
        )
    }

    fn parse_binary_expr_with(
        &mut self,
        min_bp: u8,
        parse_right: fn(&mut Self, u8) -> SyntaxNodeParseResult,
        binding_power: fn(TokenKind) -> Option<(u8, u8, InfixClass)>,
    ) -> SyntaxNodeParseResult {
        let mut left = self.parse_prefix_expr()?;
        loop {
            if let Some(next_left) = self.try_postfix(left)? {
                left = next_left;
                continue;
            }
            let Some((left_bp, right_bp, class)) = binding_power(self.peek_kind()) else {
                break;
            };
            if left_bp < min_bp {
                break;
            }
            let op = self.advance_element();
            let right = parse_right(self, right_bp)?;
            if class == InfixClass::Comparison && self.is_comparison_expr(left) {
                self.error(ParseError::new(
                    ParseErrorKind::NonAssociativeChain,
                    self.span(),
                ));
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

    fn parse_prefix_expr(&mut self) -> SyntaxNodeParseResult {
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

    fn parse_call_expr(&mut self, callee: SyntaxNodeId) -> SyntaxNodeParseResult {
        self.parse_postfix_list_expr(
            callee,
            SyntaxNodeKind::CallExpr,
            TokenKind::LParen,
            TokenKind::RParen,
            Parser::parse_arg,
        )
    }

    fn parse_arg(&mut self) -> SyntaxNodeParseResult {
        let mut children = Vec::new();
        if let Some(spread) = self.eat(TokenKind::DotDotDot) {
            children.push(spread);
        }
        children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Arg, children))
    }

    fn parse_apply_expr(&mut self, callee: SyntaxNodeId) -> SyntaxNodeParseResult {
        self.parse_postfix_list_expr(
            callee,
            SyntaxNodeKind::ApplyExpr,
            TokenKind::LBracket,
            TokenKind::RBracket,
            Parser::parse_expr_node,
        )
    }

    fn parse_index_expr(&mut self, base: SyntaxNodeId) -> SyntaxNodeParseResult {
        self.parse_postfix_list_expr(
            base,
            SyntaxNodeKind::IndexExpr,
            TokenKind::DotLBracket,
            TokenKind::RBracket,
            Parser::parse_expr_node,
        )
    }

    fn parse_postfix_list_expr(
        &mut self,
        base: SyntaxNodeId,
        kind: SyntaxNodeKind,
        open_kind: TokenKind,
        close_kind: TokenKind,
        parse_item: fn(&mut Self) -> SyntaxNodeParseResult,
    ) -> SyntaxNodeParseResult {
        let open = self.expect_token(open_kind)?;
        let mut children = vec![SyntaxElementId::Node(base), open];
        children.extend(self.parse_separated_nodes(TokenKind::Comma, close_kind, parse_item)?);
        children.push(self.expect_token(close_kind)?);
        Ok(self.builder.push_node_from_children(kind, children))
    }

    fn parse_record_update_expr(&mut self, base: SyntaxNodeId) -> SyntaxNodeParseResult {
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

    fn parse_field_expr(&mut self, base: SyntaxNodeId) -> SyntaxNodeParseResult {
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

    fn parse_type_test_expr(&mut self, base: SyntaxNodeId) -> SyntaxNodeParseResult {
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

    fn parse_type_cast_expr(&mut self, base: SyntaxNodeId) -> SyntaxNodeParseResult {
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
