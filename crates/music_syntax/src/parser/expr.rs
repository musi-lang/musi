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
            let Some((left_bp, right_bp, group)) =
                infix_binding_power_without_colon_eq(self.peek_kind())
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
        if self.at_answer_lit_start() {
            return self.parse_answer_lit_expr();
        }
        if self.at_given_decl_start() {
            return self.parse_given_expr(Vec::new());
        }
        if self.at_any(&[
            TokenKind::Minus,
            TokenKind::KwComptime,
            TokenKind::KwGiven,
            TokenKind::KwAnswer,
            TokenKind::KwNot,
            TokenKind::KwMut,
            TokenKind::DotDot,
            TokenKind::DotDotLt,
        ]) {
            let op = self.advance_element();
            let operand = self.parse_expr(PREFIX_BP)?;
            return Ok(self.builder.push_node_from_children(
                SyntaxNodeKind::PrefixExpr,
                vec![op, SyntaxElementId::Node(operand)],
            ));
        }
        self.parse_atom_expr()
    }

    fn parse_type_prefix_expr(&mut self) -> SyntaxNodeParseResult {
        if self.at_array_type_prefix() {
            return self.parse_array_type_expr();
        }
        if self.at(TokenKind::KwAnswer) {
            return self.parse_answer_type_expr();
        }
        if self.at(TokenKind::KwMut) {
            let op = self.advance_element();
            let operand = self.parse_type_expr(PREFIX_BP)?;
            return Ok(self.builder.push_node_from_children(
                SyntaxNodeKind::PrefixExpr,
                vec![op, SyntaxElementId::Node(operand)],
            ));
        }
        if self.at_any(&[TokenKind::KwAny, TokenKind::KwSome]) {
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
        if self.at_any(&[TokenKind::Dot, TokenKind::QuestionDot, TokenKind::BangDot]) {
            return self.parse_field_expr(left).map(Some);
        }
        if self.at(TokenKind::ColonQuestion) {
            return self.parse_type_test_expr(left).map(Some);
        }
        if self.at(TokenKind::ColonQuestionGt) {
            return self.parse_type_cast_expr(left).map(Some);
        }
        if self.at_partial_range_from_postfix() {
            let op = self.advance_element();
            return Ok(Some(self.builder.push_node_from_children(
                SyntaxNodeKind::PostfixExpr,
                vec![SyntaxElementId::Node(left), op],
            )));
        }
        Ok(None)
    }

    fn at_partial_range_from_postfix(&self) -> bool {
        self.at_any(&[TokenKind::DotDot, TokenKind::LtDotDot])
            && !Self::starts_expr(self.nth_kind(1))
    }

    fn at_answer_lit_start(&self) -> bool {
        self.at(TokenKind::KwAnswer)
            && self.nth_kind(1) == TokenKind::Ident
            && self.nth_kind(2) == TokenKind::LBrace
    }

    fn at_given_decl_start(&self) -> bool {
        if !self.at(TokenKind::KwGiven) {
            return false;
        }
        let mut depth = 0usize;
        let mut offset = 1usize;
        loop {
            match self.nth_kind(offset) {
                TokenKind::LParen | TokenKind::LBracket => depth += 1,
                TokenKind::RParen | TokenKind::RBracket => depth = depth.saturating_sub(1),
                TokenKind::LBrace if depth == 0 => return true,
                TokenKind::Semicolon | TokenKind::Eof if depth == 0 => return false,
                _ => {}
            }
            offset += 1;
        }
    }

    const fn starts_expr(kind: TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Int
                | TokenKind::Float
                | TokenKind::String
                | TokenKind::Rune
                | TokenKind::TemplateNoSubst
                | TokenKind::TemplateHead
                | TokenKind::Ident
                | TokenKind::OpIdent
                | TokenKind::Hash
                | TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::LBrace
                | TokenKind::Dot
                | TokenKind::KwMatch
                | TokenKind::KwLet
                | TokenKind::KwResume
                | TokenKind::KwImport
                | TokenKind::KwData
                | TokenKind::KwEffect
                | TokenKind::KwShape
                | TokenKind::KwAsk
                | TokenKind::KwHandle
                | TokenKind::KwNative
                | TokenKind::KwQuote
                | TokenKind::KwUnsafe
                | TokenKind::KwComptime
                | TokenKind::KwGiven
                | TokenKind::KwAnswer
                | TokenKind::At
                | TokenKind::KwExport
                | TokenKind::KwPartial
                | TokenKind::Minus
                | TokenKind::KwNot
                | TokenKind::KwMut
                | TokenKind::DotDot
                | TokenKind::DotDotLt
        )
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
                    | TokenKind::KwAny
                    | TokenKind::KwSome
            )
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

    fn parse_answer_type_expr(&mut self) -> SyntaxNodeParseResult {
        let answer = self.expect_token(TokenKind::KwAnswer)?;
        let effect = self.parse_answer_effect_type_expr()?;
        let open = self.expect_token(TokenKind::LParen)?;
        let input = self.parse_type_expr(ARROW_BP + 1)?;
        let arrow = self.expect_token(TokenKind::MinusGt)?;
        let output = self.parse_type_expr(0)?;
        let close = self.expect_token(TokenKind::RParen)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::AnswerTy,
            vec![
                answer,
                SyntaxElementId::Node(effect),
                open,
                SyntaxElementId::Node(input),
                arrow,
                SyntaxElementId::Node(output),
                close,
            ],
        ))
    }

    fn parse_answer_effect_type_expr(&mut self) -> SyntaxNodeParseResult {
        let mut effect = if self.at_array_type_prefix() {
            self.parse_array_type_expr()?
        } else if self.at(TokenKind::KwMut) {
            let op = self.advance_element();
            let operand = self.parse_type_expr(PREFIX_BP)?;
            self.builder.push_node_from_children(
                SyntaxNodeKind::PrefixExpr,
                vec![op, SyntaxElementId::Node(operand)],
            )
        } else {
            self.parse_atom_expr()?
        };
        while self.at(TokenKind::LBracket) {
            effect = self.parse_apply_expr(effect)?;
        }
        Ok(effect)
    }
}

const fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8, InfixGroup)> {
    match kind {
        TokenKind::ColonEq => Some((1, ASSIGN_BP, InfixGroup::Other)),
        TokenKind::PipeGt => Some((PIPE_BP, PIPE_BP + 1, InfixGroup::Other)),
        TokenKind::MinusGt | TokenKind::TildeGt => Some((ARROW_BP, ARROW_BP, InfixGroup::Other)),
        TokenKind::KwOr | TokenKind::KwCatch | TokenKind::QuestionQuestion => {
            Some((OR_BP, OR_BP + 1, InfixGroup::Other))
        }
        TokenKind::KwXor => Some((XOR_BP, XOR_BP + 1, InfixGroup::Other)),
        TokenKind::KwAnd => Some((AND_BP, AND_BP + 1, InfixGroup::Other)),
        TokenKind::DotDot | TokenKind::DotDotLt | TokenKind::LtDotDot | TokenKind::LtDotDotLt => {
            Some((COMPARE_BP, COMPARE_BP + 1, InfixGroup::Comparison))
        }
        TokenKind::Eq
        | TokenKind::TildeEq
        | TokenKind::SlashEq
        | TokenKind::Lt
        | TokenKind::Gt
        | TokenKind::LtEq
        | TokenKind::GtEq
        | TokenKind::KwIn => Some((COMPARE_BP, COMPARE_BP + 1, InfixGroup::Comparison)),
        TokenKind::KwShl | TokenKind::KwShr => Some((SHIFT_BP, SHIFT_BP + 1, InfixGroup::Other)),
        TokenKind::Plus | TokenKind::Minus => Some((ADD_BP, ADD_BP + 1, InfixGroup::Other)),
        TokenKind::Star | TokenKind::Slash | TokenKind::Percent => {
            Some((MUL_BP, MUL_BP + 1, InfixGroup::Other))
        }
        _ => None,
    }
}

const fn infix_binding_power_without_colon_eq(kind: TokenKind) -> Option<(u8, u8, InfixGroup)> {
    if matches!(kind, TokenKind::ColonEq) {
        return None;
    }
    infix_binding_power(kind)
}
