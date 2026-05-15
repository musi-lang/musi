use music_syntax::TokenKind;

use crate::{
    OperatorBreak,
    token_class::{is_operator, is_word_like},
};

use super::helpers::is_closing;
use super::{CstFormatter, CstLeafRole, ParenKind};

impl CstFormatter<'_> {
    pub(super) fn needs_space_before(&self, current: TokenKind) -> bool {
        self.needs_space_before_with_role(current, CstLeafRole::Regular)
    }

    pub(super) fn needs_space_before_with_role(
        &self,
        current: TokenKind,
        role: CstLeafRole,
    ) -> bool {
        let Some(previous) = self.previous else {
            return false;
        };
        if self.at_line_start || Self::spacing_forbidden_before(previous, current) {
            return false;
        }
        if Self::requires_space_before_dot(previous, current) || Self::is_keyword_spacing(current) {
            return true;
        }
        if Self::requires_space_before_group(previous, current, role) {
            return true;
        }
        if Self::spacing_forbidden_for_member_or_apply(current) {
            return false;
        }
        if current == TokenKind::LParen {
            return Self::requires_space_before_lparen(previous);
        }
        if matches!(previous, TokenKind::LParen | TokenKind::LBracket) {
            return false;
        }
        if self.spacing_forbidden_after_unary_minus(previous, current) {
            return false;
        }
        if previous == TokenKind::Colon || current == TokenKind::Colon {
            return true;
        }
        if is_operator(previous) || is_operator(current) {
            return true;
        }
        is_word_like(previous) && is_word_like(current)
    }

    const fn spacing_forbidden_before(previous: TokenKind, current: TokenKind) -> bool {
        is_closing(current)
            || matches!(current, TokenKind::Comma | TokenKind::Semicolon)
            || matches!(
                previous,
                TokenKind::Dot | TokenKind::At | TokenKind::Hash | TokenKind::Backslash
            )
    }

    fn requires_space_before_dot(previous: TokenKind, current: TokenKind) -> bool {
        current == TokenKind::Dot
            && (matches!(
                previous,
                TokenKind::ColonEq
                    | TokenKind::KwElse
                    | TokenKind::KwLet
                    | TokenKind::KwThen
                    | TokenKind::Pipe
            ) || is_operator(previous))
    }

    const fn is_keyword_spacing(current: TokenKind) -> bool {
        matches!(current, TokenKind::KwElse | TokenKind::KwThen)
    }

    fn requires_space_before_group(
        previous: TokenKind,
        current: TokenKind,
        role: CstLeafRole,
    ) -> bool {
        (current == TokenKind::LBrace
            && (is_word_like(previous)
                || matches!(previous, TokenKind::RBracket | TokenKind::RParen)))
            || (current == TokenKind::LBracket
                && matches!(
                    role,
                    CstLeafRole::TypeParamBracket | CstLeafRole::ArrayTypeBracket
                ))
            || (current == TokenKind::LBracket && previous == TokenKind::Pipe)
    }

    const fn spacing_forbidden_for_member_or_apply(current: TokenKind) -> bool {
        matches!(current, TokenKind::Dot | TokenKind::LBracket)
    }

    fn requires_space_before_lparen(previous: TokenKind) -> bool {
        matches!(
            previous,
            TokenKind::KwElse
                | TokenKind::KwMatch
                | TokenKind::KwThen
                | TokenKind::KwUnsafe
                | TokenKind::Colon
        ) || is_operator(previous)
    }

    fn spacing_forbidden_after_unary_minus(&self, previous: TokenKind, current: TokenKind) -> bool {
        previous == TokenKind::Minus
            && matches!(current, TokenKind::Int | TokenKind::Float)
            && self
                .before_previous
                .is_none_or(Self::token_allows_unary_expression)
    }

    const fn token_allows_unary_expression(token: TokenKind) -> bool {
        matches!(
            token,
            TokenKind::ColonEq
                | TokenKind::Comma
                | TokenKind::EqGt
                | TokenKind::LParen
                | TokenKind::LBracket
                | TokenKind::KwElse
                | TokenKind::KwThen
                | TokenKind::Semicolon
        )
    }
}

impl CstFormatter<'_> {
    pub(super) const fn should_break_after_comma() -> bool {
        false
    }

    pub(super) fn maybe_break_before_token(
        &mut self,
        kind: TokenKind,
        text: &str,
        role: CstLeafRole,
    ) {
        if self.options.line_width == 0
            || self.at_line_start
            || !self.can_break_before_token(kind)
            || is_closing(kind)
        {
            return;
        }
        let space_len = usize::from(self.needs_space_before_with_role(kind, role));
        if self
            .line_len
            .saturating_add(space_len)
            .saturating_add(text.len())
            <= self.options.line_width
        {
            return;
        }
        if let Some(frame) = self.parens.last_mut() {
            frame.broke = true;
        }
        if self.parens.is_empty()
            || self
                .parens
                .last()
                .is_some_and(|frame| frame.kind != ParenKind::Regular)
        {
            self.continuation_indent = self.continuation_indent.max(1);
        }
        self.newline();
    }

    fn can_break_before_token(&self, current: TokenKind) -> bool {
        self.previous == Some(TokenKind::Comma)
            && self.parens.last().is_some_and(|frame| {
                frame.allows_trailing_comma
                    && matches!(frame.kind, ParenKind::Regular | ParenKind::Bracket)
            })
            || self.previous == Some(TokenKind::ColonEq)
            || (self.options.operator_break == OperatorBreak::After
                && self.previous.is_some_and(is_operator))
            || (self.options.operator_break == OperatorBreak::Before
                && is_operator(current)
                && self.parens.is_empty())
    }

    pub(super) fn write_indent_if_needed(&mut self) {
        if !self.at_line_start {
            return;
        }
        let unit = self.options.indent_unit();
        for _ in 0..self.indent.saturating_add(self.continuation_indent) {
            self.out.push_str(&unit);
            self.line_len = self.line_len.saturating_add(unit.len());
        }
        self.at_line_start = false;
    }

    pub(super) fn projected_line_len(&self) -> usize {
        if self.at_line_start {
            return self
                .indent
                .saturating_add(self.continuation_indent)
                .saturating_mul(self.options.indent_unit().len());
        }
        self.line_len
    }

    pub(super) fn push_space(&mut self) {
        if self.at_line_start || self.out.ends_with(' ') || self.out.ends_with('\n') {
            return;
        }
        self.out.push(' ');
        self.line_len = self.line_len.saturating_add(1);
    }

    pub(super) fn newline(&mut self) {
        self.trim_trailing_spaces();
        if !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.line_len = 0;
        self.at_line_start = true;
        self.line_start_paren_depth = self.parens.len();
    }

    pub(super) fn blank_line(&mut self) {
        self.trim_trailing_spaces();
        if self.out.is_empty() || self.out.ends_with("\n\n") {
            self.line_len = 0;
            self.at_line_start = true;
            self.line_start_paren_depth = self.parens.len();
            self.continuation_indent = 0;
            return;
        }
        if self.out.ends_with('\n') {
            self.out.push('\n');
        } else {
            self.out.push_str("\n\n");
        }
        self.line_len = 0;
        self.at_line_start = true;
        self.line_start_paren_depth = self.parens.len();
        self.continuation_indent = 0;
    }

    pub(super) fn trim_trailing_spaces(&mut self) {
        while self.out.ends_with(' ') || self.out.ends_with('\t') {
            let _ = self.out.pop();
        }
    }
}
