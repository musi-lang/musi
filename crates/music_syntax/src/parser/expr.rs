use super::*;

type SyntaxNodeParseResult = ParseResult<SyntaxNodeId>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum InfixGroup {
    Comparison,
    Other,
}

impl Parser<'_> {
    pub(super) fn parse_expr(&mut self, min_bp: u8) -> SyntaxNodeParseResult {
        self.parse_binary_expr_with(min_bp, Self::parse_expr, infix_binding_power)
    }

    pub(crate) fn parse_type_expr(&mut self, min_bp: u8) -> SyntaxNodeParseResult {
        self.parse_binary_type_expr_with(min_bp)
    }

    fn parse_binary_expr_with(
        &mut self,
        min_bp: u8,
        parse_right: fn(&mut Self, u8) -> SyntaxNodeParseResult,
        binding_power: fn(TokenKind) -> Option<(u8, u8, InfixGroup)>,
    ) -> SyntaxNodeParseResult {
        let mut left = self.parse_prefix_expr()?;
        loop {
            if let Some(next_left) = self.try_postfix(left)? {
                left = next_left;
                continue;
            }
            let Some((left_bp, right_bp, group)) = binding_power(self.peek_kind()) else {
                break;
            };
            if left_bp < min_bp {
                break;
            }
            let op = self.advance_element();
            let right = parse_right(self, right_bp)?;
            if group == InfixGroup::Comparison && self.is_comparison_expr(left) {
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
            if group == InfixGroup::Comparison {
                self.comparison_exprs.push(left);
            }
        }
        Ok(left)
    }

    fn parse_binary_type_expr_with(&mut self, min_bp: u8) -> SyntaxNodeParseResult {
        let mut left = self.parse_type_prefix_expr()?;
        loop {
            if let Some(next_left) = self.try_postfix(left)? {
                left = next_left;
                continue;
            }
            let Some((left_bp, right_bp, group)) = type_infix_binding_power(self.peek_kind())
            else {
                break;
            };
            if left_bp < min_bp {
                break;
            }
            let op = self.advance_element();
            let right = self.parse_type_expr(right_bp)?;
            if group == InfixGroup::Comparison && self.is_comparison_expr(left) {
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
            if group == InfixGroup::Comparison {
                self.comparison_exprs.push(left);
            }
        }
        Ok(left)
    }

    fn parse_prefix_expr(&mut self) -> SyntaxNodeParseResult {
        if self.at_any(&[
            TokenKind::Minus,
            TokenKind::KwKnown,
            TokenKind::KwNot,
            TokenKind::KwMut,
        ]) {
            let op = self.advance_element();
            let operand = self.parse_expr(PREFIX_BP)?;
            return Ok(self.builder.push_node_from_children(
                SyntaxNodeKind::PrefixExpr,
                vec![op, SyntaxElementId::Node(operand)],
            ));
        }
        let mut ty = self.parse_atom_expr()?;
        while self.at(TokenKind::LBracket) {
            ty = self.parse_apply_expr(ty)?;
        }
        Ok(ty)
    }

    fn parse_type_prefix_expr(&mut self) -> SyntaxNodeParseResult {
        if self.at_stack_effect_expr() {
            return self.parse_stack_effect_expr();
        }
        if self.at_array_type_prefix() {
            return self.parse_array_type_expr();
        }
        if self.at_any(&[
            TokenKind::Question,
            TokenKind::KwMut,
            TokenKind::KwErased,
            TokenKind::KwHidden,
        ]) {
            let op = self.advance_element();
            let operand = self.parse_type_expr(PREFIX_BP)?;
            return Ok(self.builder.push_node_from_children(
                SyntaxNodeKind::PrefixExpr,
                vec![op, SyntaxElementId::Node(operand)],
            ));
        }
        self.parse_atom_expr()
    }

    fn try_postfix(&mut self, left: SyntaxNodeId) -> ParseResult<Option<SyntaxNodeId>> {
        if self.at(TokenKind::LParen) {
            if self.nth_kind(1) == TokenKind::Pipe {
                return Ok(None);
            }
            return self.parse_call_expr(left).map(Some);
        }
        if self.at(TokenKind::LBracket) {
            return self.parse_apply_expr(left).map(Some);
        }
        if self.at(TokenKind::DotLBracket) {
            return self.parse_index_expr(left).map(Some);
        }
        if self.at(TokenKind::Dot) {
            return self.parse_field_expr(left).map(Some);
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
        if self.peek_kind() == TokenKind::Ident && self.nth_kind(1) == TokenKind::ColonEq {
            children.push(self.advance_element());
            children.push(self.advance_element());
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

    fn at_array_type_prefix(&self) -> bool {
        if !self.at(TokenKind::LBracket) {
            return false;
        }
        let mut cursor = self.pos;
        let token_count = self.tokens.len();
        while cursor < token_count && same_kind(self.tokens[cursor].kind, TokenKind::LBracket) {
            cursor += 1;
            if cursor >= token_count {
                return false;
            }
            if same_kind(self.tokens[cursor].kind, TokenKind::RBracket) {
            } else {
                let kind = self.tokens[cursor].kind;
                if !matches!(
                    kind,
                    TokenKind::Int | TokenKind::Ident | TokenKind::Underscore
                ) {
                    return false;
                }
                cursor += 1;
                if cursor >= token_count
                    || !same_kind(self.tokens[cursor].kind, TokenKind::RBracket)
                {
                    return false;
                }
            }
            cursor += 1;
        }
        cursor < token_count
            && matches!(
                self.tokens[cursor].kind,
                TokenKind::Ident
                    | TokenKind::OpIdent
                    | TokenKind::LParen
                    | TokenKind::LBrace
                    | TokenKind::LBracket
                    | TokenKind::KwMut
                    | TokenKind::KwErased
                    | TokenKind::KwHidden
            )
    }

    pub(crate) fn at_stack_effect_expr(&self) -> bool {
        if !self.at(TokenKind::LBracket) {
            return false;
        }
        let mut depth = 0usize;
        let mut offset = 0usize;
        loop {
            match self.nth_kind(offset) {
                TokenKind::LBracket => depth += 1,
                TokenKind::RBracket => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return false;
                    }
                }
                TokenKind::Semicolon if depth == 1 => return true,
                TokenKind::Eof => return false,
                _ => {}
            }
            offset += 1;
        }
    }

    fn parse_array_type_expr(&mut self) -> SyntaxNodeParseResult {
        let mut children = Vec::new();
        loop {
            if !self.at(TokenKind::LBracket) {
                break;
            }
            children.push(self.expect_token(TokenKind::LBracket)?);
            if !self.at(TokenKind::RBracket) {
                match self.peek_kind() {
                    TokenKind::Int | TokenKind::Ident | TokenKind::Underscore => {
                        children.push(self.advance_element());
                    }
                    _ => return Err(self.expected_expression()),
                }
            }
            children.push(self.expect_token(TokenKind::RBracket)?);
            if !self.at(TokenKind::LBracket) {
                break;
            }
        }
        children.push(SyntaxElementId::Node(self.parse_type_expr(PREFIX_BP)?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ArrayTy, children))
    }
}

const fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8, InfixGroup)> {
    match kind {
        TokenKind::ColonEq => Some((1, ASSIGN_BP, InfixGroup::Other)),
        TokenKind::PipeGt => Some((PIPE_BP, PIPE_BP + 1, InfixGroup::Other)),
        TokenKind::MinusGt => Some((ARROW_BP, ARROW_BP, InfixGroup::Other)),
        TokenKind::KwOr
        | TokenKind::QuestionQuestion
        | TokenKind::ColonGt
        | TokenKind::ColonQuestionGt => Some((OR_BP, OR_BP + 1, InfixGroup::Other)),
        TokenKind::KwXor => Some((XOR_BP, XOR_BP + 1, InfixGroup::Other)),
        TokenKind::KwAnd => Some((AND_BP, AND_BP + 1, InfixGroup::Other)),
        TokenKind::Eq
        | TokenKind::SlashEq
        | TokenKind::TildeEq
        | TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::LtEq
        | TokenKind::GtEq
        | TokenKind::DotDot
        | TokenKind::DotDotLt
        | TokenKind::KwIn => Some((COMPARE_BP, COMPARE_BP + 1, InfixGroup::Comparison)),
        TokenKind::Plus | TokenKind::Minus => Some((ADD_BP, ADD_BP + 1, InfixGroup::Other)),
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
            Some((MUL_BP, MUL_BP + 1, InfixGroup::Other))
        }
        _ => None,
    }
}

const fn type_infix_binding_power(kind: TokenKind) -> Option<(u8, u8, InfixGroup)> {
    match kind {
        TokenKind::ColonEq => None,
        TokenKind::Bang => Some((ARROW_BP + 1, ARROW_BP + 2, InfixGroup::Other)),
        TokenKind::MinusGt => Some((ARROW_BP, ARROW_BP, InfixGroup::Other)),
        _ => infix_binding_power(kind),
    }
}
