use music_base::Span;
use music_syntax::{LexedSource, TokenKind};

use crate::{
    BracePosition, GroupLayout, MatchArmIndent, OperatorBreak, TrailingCommas,
    line_width::{
        declaration_tail_flat_len, group_flat_len, regular_group_next_segment_len,
        rhs_block_header_len, rhs_field_flat_len, rhs_flat_len,
    },
    token_class::{is_operator, is_word_like},
};

use super::{
    BraceFrame, BraceKind, DeclarationState, ParenFrame, ParenKind, PendingAttachment,
    SourceFormatter, TokenFormatRole, TokenWriteOptions, next_non_comma_token_kind,
};

impl SourceFormatter<'_> {
    pub(super) fn write_token(
        &mut self,
        kind: TokenKind,
        text: &str,
        role: TokenFormatRole,
        span: Span,
        options: TokenWriteOptions,
    ) {
        if self.write_ignored_token(kind, text, span) || self.skip_current_token(span, options) {
            return;
        }
        if options.break_before_operator() {
            self.continuation_indent = self.continuation_indent.max(1);
            self.newline();
        }
        self.write_token_body(kind, text, role, options);
        self.finish_token_write(kind, role, span, options);
    }

    fn write_ignored_token(&mut self, kind: TokenKind, text: &str, span: Span) -> bool {
        if !self.ignore_next {
            return false;
        }
        self.write_original_token(kind, text);
        self.ignore_next = false;
        self.previous = Some(kind);
        self.set_last_token_end(span);
        true
    }

    fn skip_current_token(&mut self, span: Span, options: TokenWriteOptions) -> bool {
        if !options.skip_current_comma() {
            return false;
        }
        self.set_last_token_end(span);
        true
    }

    fn write_token_body(
        &mut self,
        kind: TokenKind,
        text: &str,
        role: TokenFormatRole,
        options: TokenWriteOptions,
    ) {
        match kind {
            TokenKind::RBrace | TokenKind::RParen | TokenKind::RBracket => {
                self.write_closing_group(kind, text);
            }
            TokenKind::LBrace => self.write_open_brace(text, role),
            TokenKind::Semicolon => self.write_semicolon(),
            TokenKind::Comma => self.write_comma(options.break_after_comma()),
            TokenKind::Pipe => self.write_pipe(text),
            TokenKind::LParen => {
                self.write_open_paren(text, role, options.break_after_open_group());
            }
            TokenKind::LBracket => {
                self.write_open_bracket(text, role, options.break_after_open_group());
            }
            _ => self.write_regular(kind, text, role),
        }
    }

    fn write_closing_group(&mut self, kind: TokenKind, text: &str) {
        let paren = matches!(kind, TokenKind::RParen | TokenKind::RBracket)
            .then(|| self.parens.pop())
            .flatten();
        let brace = (kind == TokenKind::RBrace)
            .then(|| self.braces.pop())
            .flatten();
        self.write_closing_trailing_comma(paren, brace);
        self.update_closing_indent(kind, paren, brace);
        self.write_closing_newline(kind, paren);
        if matches!(kind, TokenKind::RParen | TokenKind::RBracket) && self.out.ends_with(", ") {
            self.trim_trailing_spaces();
        }
        self.write_punct(kind, text);
        if paren.is_some_and(|frame| frame.kind.is_sequence()) {
            self.indent = self.indent.saturating_sub(1);
        }
    }

    fn write_closing_trailing_comma(
        &mut self,
        paren: Option<ParenFrame>,
        brace: Option<BraceFrame>,
    ) {
        if self.should_insert_trailing_comma(paren) {
            self.write_trailing_comma();
        }
        if self.should_insert_brace_trailing_comma(brace) {
            self.write_trailing_comma();
        }
    }

    fn update_closing_indent(
        &mut self,
        kind: TokenKind,
        paren: Option<ParenFrame>,
        brace: Option<BraceFrame>,
    ) {
        if paren.is_some_and(|frame| matches!(frame.kind, ParenKind::Regular | ParenKind::Bracket))
        {
            self.indent = self.indent.saturating_sub(1);
        }
        if let Some(frame) = brace {
            self.continuation_indent = frame.continuation_indent;
        }
        if kind == TokenKind::RBrace
            || paren
                .is_some_and(|frame| frame.kind.is_multiline() && frame.kind.closes_body_indent())
        {
            self.indent = self.indent.saturating_sub(1);
        }
    }

    fn write_closing_newline(&mut self, kind: TokenKind, paren: Option<ParenFrame>) {
        let multiline_close = paren.is_some_and(|frame| frame.kind.is_multiline());
        let broken_regular_close = paren.is_some_and(|frame| frame.broke);
        if (kind == TokenKind::RBrace || multiline_close || broken_regular_close)
            && !self.at_line_start
        {
            self.newline();
        }
    }

    fn finish_token_write(
        &mut self,
        kind: TokenKind,
        role: TokenFormatRole,
        span: Span,
        options: TokenWriteOptions,
    ) {
        if options.break_after_colon_eq() {
            self.continuation_indent = self.continuation_indent.max(1);
            self.newline();
        }
        self.update_state(kind);
        self.previous = Some(kind);
        self.update_pending_attachment(role);
        self.set_last_token_end(span);
    }

    fn update_pending_attachment(&mut self, role: TokenFormatRole) {
        if role == TokenFormatRole::AttributeEnd {
            self.newline();
            self.pending_attachment = PendingAttachment::ItemDoc;
        } else if role != TokenFormatRole::Attribute {
            self.pending_attachment = PendingAttachment::None;
        }
    }
}

impl SourceFormatter<'_> {
    pub(super) fn should_break_after_current_comma(
        &self,
        lexed: &LexedSource,
        token_index: usize,
    ) -> bool {
        if self.options.line_width == 0
            || self.options.trailing_commas != TrailingCommas::MultiLine
            || !self
                .parens
                .last()
                .is_some_and(|frame| matches!(frame.kind, ParenKind::Regular | ParenKind::Bracket))
        {
            return false;
        }
        let next_segment_len = regular_group_next_segment_len(lexed, token_index.saturating_add(1));
        next_segment_len > 0
            && self
                .line_len
                .saturating_add(1)
                .saturating_add(next_segment_len)
                > self.options.line_width
    }

    pub(super) fn should_skip_current_comma(
        &self,
        lexed: &LexedSource,
        token_index: usize,
    ) -> bool {
        self.options.trailing_commas == TrailingCommas::Never
            && lexed.tokens()[token_index].kind == TokenKind::Comma
            && next_non_comma_token_kind(lexed, token_index.saturating_add(1)).is_some_and(|kind| {
                matches!(
                    kind,
                    TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace
                )
            })
            && (self
                .parens
                .last()
                .is_some_and(|frame| matches!(frame.kind, ParenKind::Regular | ParenKind::Bracket))
                || self
                    .braces
                    .last()
                    .is_some_and(|frame| frame.kind == BraceKind::CommaList))
    }

    pub(super) fn should_break_before_current_operator(
        &self,
        lexed: &LexedSource,
        token_index: usize,
    ) -> bool {
        let token = lexed.tokens()[token_index];
        if self.options.line_width == 0
            || self.options.operator_break != OperatorBreak::Before
            || self.at_line_start
            || !self.parens.is_empty()
            || !is_operator(token.kind)
        {
            return false;
        }
        let Some(operator_text) = lexed.token_text(token_index) else {
            return false;
        };
        let next_len = lexed
            .tokens()
            .get(token_index.saturating_add(1))
            .and_then(|next| {
                (next.kind != TokenKind::Eof).then(|| lexed.token_text(token_index + 1))
            })
            .flatten()
            .map_or(0, str::len);
        self.line_len
            .saturating_add(1)
            .saturating_add(operator_text.len())
            .saturating_add(usize::from(next_len > 0))
            .saturating_add(next_len)
            > self.options.line_width
    }

    pub(super) fn should_break_after_current_colon_eq(
        &self,
        lexed: &LexedSource,
        token_index: usize,
    ) -> bool {
        if self.options.line_width == 0
            || self.at_line_start
            || self.previous == Some(TokenKind::Colon)
            || !self.parens.is_empty()
            || lexed.tokens()[token_index].kind != TokenKind::ColonEq
        {
            return false;
        }
        let Some(text) = lexed.token_text(token_index) else {
            return false;
        };
        let space_len = usize::from(self.needs_space_before(TokenKind::ColonEq));
        if let Some(header_len) = rhs_block_header_len(lexed, token_index.saturating_add(1))
            && self
                .line_len
                .saturating_add(space_len)
                .saturating_add(text.len())
                .saturating_add(1)
                .saturating_add(header_len)
                <= self.options.line_width
        {
            return false;
        }
        let rhs_len = if self
            .braces
            .last()
            .is_some_and(|frame| frame.kind == BraceKind::CommaList)
            && self.parens.is_empty()
        {
            rhs_field_flat_len(lexed, token_index.saturating_add(1))
        } else {
            rhs_flat_len(lexed, token_index.saturating_add(1))
        };
        rhs_len > 0
            && self
                .line_len
                .saturating_add(space_len)
                .saturating_add(text.len())
                .saturating_add(1)
                .saturating_add(rhs_len)
                > self.options.line_width
    }

    pub(super) fn should_break_after_current_open_group(
        &self,
        lexed: &LexedSource,
        token_index: usize,
        role: TokenFormatRole,
    ) -> bool {
        if self.options.line_width == 0 {
            return false;
        }
        let token = lexed.tokens()[token_index];
        if !matches!(token.kind, TokenKind::LParen | TokenKind::LBracket) {
            return false;
        }
        if token.kind == TokenKind::LBracket && self.previous.is_some_and(is_word_like) {
            return false;
        }
        let group_len = group_flat_len(lexed, token_index);
        if group_len <= 2 {
            return false;
        }
        let Some(text) = lexed.token_text(token_index) else {
            return false;
        };
        let space_len = usize::from(self.needs_space_before_with_role(token.kind, role));
        let line_len = self.projected_line_len();
        if self.should_force_block_group(role) {
            return line_len
                .saturating_add(space_len)
                .saturating_add(text.len())
                <= self.options.line_width;
        }
        let flat_len = if matches!(
            role,
            TokenFormatRole::ParamParen | TokenFormatRole::MemberParamParen
        ) || self.declaration_head_active
        {
            let tail_len = declaration_tail_flat_len(lexed, token_index);
            if group_len > self.options.line_width / 2 {
                tail_len.max(self.options.line_width.saturating_add(1))
            } else {
                tail_len
            }
        } else {
            group_len
        };
        line_len.saturating_add(space_len).saturating_add(flat_len) > self.options.line_width
            && line_len
                .saturating_add(space_len)
                .saturating_add(text.len())
                <= self.options.line_width
    }

    fn should_force_block_group(&self, role: TokenFormatRole) -> bool {
        match role {
            TokenFormatRole::CallParen => self.options.call_argument_layout == GroupLayout::Block,
            TokenFormatRole::ParamParen => {
                self.options.declaration_parameter_layout == GroupLayout::Block
            }
            TokenFormatRole::MemberParamParen => {
                self.options.effect_member_parameter_layout == GroupLayout::Block
            }
            _ => false,
        }
    }
}

impl SourceFormatter<'_> {
    fn write_original_token(&mut self, _kind: TokenKind, text: &str) {
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
    }

    fn write_open_brace(&mut self, text: &str, role: TokenFormatRole) {
        if self.previous != Some(TokenKind::ColonEq) {
            self.continuation_indent = 0;
        }
        if self.options.brace_position == BracePosition::NextLine && !self.at_line_start {
            self.newline();
        } else if self.needs_space_before(TokenKind::LBrace) {
            self.push_space();
        }
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
        self.braces.push(BraceFrame::new(
            if role == TokenFormatRole::CommaListBrace {
                BraceKind::CommaList
            } else {
                BraceKind::Block
            },
            self.continuation_indent,
        ));
        self.indent = self.indent.saturating_add(1);
        self.newline();
    }

    fn write_open_paren(&mut self, text: &str, role: TokenFormatRole, break_after_open: bool) {
        let paren = match role {
            TokenFormatRole::SequenceParen => Some(ParenKind::Sequence),
            TokenFormatRole::MatchParen
                if self.options.match_arm_indent == MatchArmIndent::PipeAligned =>
            {
                Some(ParenKind::MatchAligned)
            }
            TokenFormatRole::MatchParen => Some(ParenKind::Match),
            TokenFormatRole::ForeignGroupParen => Some(ParenKind::ForeignGroup),
            _ => None,
        };
        if let Some(paren) = paren {
            if matches!(paren, ParenKind::Sequence) {
                self.continuation_indent = 0;
                if !self.at_line_start {
                    self.newline();
                }
                self.indent = self.indent.saturating_add(1);
            } else if !self.at_line_start {
                self.push_space();
            }
            self.write_indent_if_needed();
            self.out.push_str(text);
            self.line_len = self.line_len.saturating_add(text.len());
            self.parens.push(ParenFrame::new(paren));
            self.declaration_state = DeclarationState::None;
            if paren.closes_body_indent() {
                self.indent = self.indent.saturating_add(1);
            }
            self.newline();
            return;
        }
        if (matches!(
            role,
            TokenFormatRole::ParamParen | TokenFormatRole::MemberParamParen
        ) && self.previous != Some(TokenKind::Backslash))
            || self.declaration_state == DeclarationState::NameBeforeParams
            || self.needs_space_before(TokenKind::LParen)
        {
            self.push_space();
        }
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
        self.parens.push(ParenFrame::with_trailing_commas(
            ParenKind::Regular,
            !matches!(
                role,
                TokenFormatRole::ParamParen | TokenFormatRole::MemberParamParen
            ),
        ));
        self.indent = self.indent.saturating_add(1);
        self.declaration_state = DeclarationState::None;
        if break_after_open {
            if let Some(frame) = self.parens.last_mut() {
                frame.broke = true;
            }
            self.newline();
        }
    }

    fn write_open_bracket(&mut self, text: &str, role: TokenFormatRole, break_after_open: bool) {
        if matches!(
            role,
            TokenFormatRole::TypeParamBracket | TokenFormatRole::ArrayTypeBracket
        ) || self.previous == Some(TokenKind::ColonEq)
            || self.previous == Some(TokenKind::Pipe)
        {
            self.push_space();
        }
        self.write_punct(TokenKind::LBracket, text);
        self.parens.push(ParenFrame::with_trailing_commas(
            ParenKind::Bracket,
            role != TokenFormatRole::TypeParamBracket,
        ));
        self.indent = self.indent.saturating_add(1);
        if break_after_open {
            if let Some(frame) = self.parens.last_mut() {
                frame.broke = true;
            }
            self.newline();
        }
    }

    fn write_semicolon(&mut self) {
        self.continuation_indent = 0;
        self.write_indent_if_needed();
        self.out.push(';');
        self.newline();
    }

    fn write_pipe(&mut self, text: &str) {
        if !self.at_line_start {
            self.newline();
        }
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
    }

    fn write_comma(&mut self, break_after_comma: bool) {
        if let Some(frame) = self.parens.last_mut()
            && matches!(frame.kind, ParenKind::Regular | ParenKind::Bracket)
        {
            frame.saw_comma = true;
        }
        let in_brace_comma_list = self
            .braces
            .last()
            .is_some_and(|frame| frame.kind == BraceKind::CommaList)
            && self.parens.is_empty();
        if in_brace_comma_list && let Some(frame) = self.braces.last_mut() {
            frame.saw_comma = true;
        }
        self.write_indent_if_needed();
        self.out.push(',');
        self.line_len = self.line_len.saturating_add(1);
        if in_brace_comma_list
            || break_after_comma
            || Self::should_break_after_comma()
            || self.group_already_broke()
        {
            if in_brace_comma_list {
                self.continuation_indent = self
                    .braces
                    .last()
                    .map_or(0, |frame| frame.continuation_indent);
            }
            if let Some(frame) = self.parens.last_mut() {
                frame.broke = true;
            }
            self.newline();
        } else {
            self.push_space();
        }
    }
}

impl SourceFormatter<'_> {
    fn write_trailing_comma(&mut self) {
        if self.previous == Some(TokenKind::Comma) {
            return;
        }
        self.trim_trailing_spaces();
        self.write_indent_if_needed();
        self.out.push(',');
        self.line_len = self.line_len.saturating_add(1);
    }

    const fn should_insert_trailing_comma(&self, paren: Option<ParenFrame>) -> bool {
        let Some(paren) = paren else {
            return false;
        };
        if !matches!(paren.kind, ParenKind::Regular | ParenKind::Bracket)
            || !paren.saw_comma
            || !paren.allows_trailing_comma
        {
            return false;
        }
        if self.options.line_width > 0 && self.line_len.saturating_add(1) > self.options.line_width
        {
            return false;
        }
        match self.options.trailing_commas {
            TrailingCommas::Always => true,
            TrailingCommas::MultiLine => paren.broke,
            TrailingCommas::Never => false,
        }
    }

    fn should_insert_brace_trailing_comma(&self, brace: Option<BraceFrame>) -> bool {
        let Some(brace) = brace else {
            return false;
        };
        if brace.kind != BraceKind::CommaList || !brace.saw_comma {
            return false;
        }
        if self.options.line_width > 0 && self.line_len.saturating_add(1) > self.options.line_width
        {
            return false;
        }
        match self.options.trailing_commas {
            TrailingCommas::Always | TrailingCommas::MultiLine => true,
            TrailingCommas::Never => false,
        }
    }

    fn write_punct(&mut self, _kind: TokenKind, text: &str) {
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
    }

    fn group_already_broke(&self) -> bool {
        self.parens.last().is_some_and(|frame| {
            frame.broke && matches!(frame.kind, ParenKind::Regular | ParenKind::Bracket)
        })
    }

    fn write_regular(&mut self, kind: TokenKind, text: &str, role: TokenFormatRole) {
        if kind == TokenKind::KwExport
            && self.indent == 0
            && !self.out.is_empty()
            && !self.out.ends_with("\n\n")
            && !self.pending_attachment.is_pending()
            && !self.out_ends_with_attachment_line()
        {
            self.blank_line();
        }
        self.maybe_break_before_token(kind, text, role);
        if self.needs_space_before_with_role(kind, role) {
            self.push_space();
        }
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
    }
}

impl SourceFormatter<'_> {
    fn update_state(&mut self, kind: TokenKind) {
        match kind {
            TokenKind::KwLet => {
                self.declaration_state = DeclarationState::WaitingName;
                self.declaration_head_active = true;
            }
            TokenKind::Ident if self.declaration_state == DeclarationState::WaitingName => {
                self.declaration_state = DeclarationState::NameBeforeParams;
            }
            TokenKind::KwExport | TokenKind::KwRec | TokenKind::KwPartial => {}
            _ if kind != TokenKind::LParen
                && !matches!(kind, TokenKind::LBracket | TokenKind::RBracket) =>
            {
                self.declaration_state = DeclarationState::None;
            }
            _ => {}
        }
        if matches!(kind, TokenKind::ColonEq | TokenKind::Semicolon) {
            self.declaration_head_active = false;
        }
    }
}
