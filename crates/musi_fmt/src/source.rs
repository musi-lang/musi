use std::ops::Range;

use music_base::Span;
use music_syntax::{LexedSource, Lexer, TokenKind, TriviaKind, parse};

use crate::{
    BracePosition, FormatError, FormatOptions, FormatResultOf, GroupLayout, MatchArmArrowAlignment,
    MatchArmIndent, OperatorBreak, TrailingCommas,
    imports::organize_imports,
    line_width::{
        declaration_tail_flat_len, group_flat_len, regular_group_next_segment_len,
        rhs_block_header_len, rhs_field_flat_len, rhs_flat_len,
    },
    protected::protected_line_ranges,
    roles::collect_token_roles,
    token_class::{is_operator, is_word_like},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FormatResult {
    pub text: String,
    pub changed: bool,
}

fn starts_with_item_doc_block_comment(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'-', ..])
}

fn starts_with_module_doc_block_comment(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'!', ..])
}

/// Formats one Musi source string.
///
/// # Errors
///
/// Returns [`FormatError::SyntaxErrors`] when lexing or parsing fails.
pub fn format_source(source: &str, options: &FormatOptions) -> FormatResultOf {
    let original_source = source;
    if has_ignore_file(source) {
        return Ok(FormatResult {
            text: ensure_final_newline(source),
            changed: source != ensure_final_newline(source),
        });
    }
    let organized = organize_imports(source);
    let source = organized.as_deref().unwrap_or(source);
    let lexed = Lexer::new(source).lex();
    let parsed = parse(lexed.clone());
    if !lexed.errors().is_empty() || !parsed.errors().is_empty() {
        return Err(FormatError::SyntaxErrors);
    }

    let tree = parsed.tree();
    let protected_ranges = protected_line_ranges(source, tree);
    let token_roles = collect_token_roles(tree);
    let mut formatter = SourceFormatter::new(source, options, protected_ranges);
    for (index, token) in lexed.tokens().iter().enumerate() {
        if token.kind == TokenKind::Eof {
            break;
        }
        for trivia in lexed.token_trivia(index) {
            if formatter.write_protected_if_needed(trivia.span) {
                continue;
            }
            if trivia.kind.is_comment() {
                let Some(text) = lexed.slice_span(trivia.span) else {
                    continue;
                };
                formatter.write_comment(text, trivia.kind, trivia.span);
            }
        }
        if formatter.write_protected_if_needed(token.span) {
            continue;
        }
        let Some(text) = lexed.token_text(index) else {
            continue;
        };
        let role = token_roles.get(index).copied().unwrap_or_default();
        formatter.preserve_blank_separator_if_needed(token.span);
        let break_after_comma = formatter.should_break_after_current_comma(&lexed, index);
        let break_before_operator = formatter.should_break_before_current_operator(&lexed, index);
        let break_after_colon_eq = formatter.should_break_after_current_colon_eq(&lexed, index);
        let break_after_open_group =
            formatter.should_break_after_current_open_group(&lexed, index, role);
        let skip_current_comma = formatter.should_skip_current_comma(&lexed, index);
        formatter.write_token(
            token.kind,
            text,
            role,
            token.span,
            break_after_comma,
            skip_current_comma,
            break_before_operator,
            break_after_colon_eq,
            break_after_open_group,
        );
    }
    let formatted_text = enforce_bind_line_width(formatter.finish(), options);
    let formatted_text = compact_record_fields(formatted_text, options);
    let formatted_text = align_match_arrows(formatted_text, options);
    Ok(FormatResult {
        changed: formatted_text != original_source,
        text: formatted_text,
    })
}

struct SourceFormatter<'a> {
    original: &'a str,
    options: &'a FormatOptions,
    protected_ranges: Vec<Range<usize>>,
    protected_index: usize,
    protected_until: usize,
    out: String,
    indent: usize,
    line_len: usize,
    at_line_start: bool,
    previous: Option<TokenKind>,
    ignore_next: bool,
    declaration_state: DeclarationState,
    declaration_head_active: bool,
    parens: ParenFrameList,
    braces: BraceFrameList,
    last_token_end: usize,
    pending_attachment: bool,
    line_start_paren_depth: usize,
    continuation_indent: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DeclarationState {
    None,
    WaitingName,
    NameBeforeParams,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ParenKind {
    Regular,
    Bracket,
    Sequence,
    Match,
    MatchAligned,
    ForeignGroup,
}

impl ParenKind {
    const fn is_sequence(self) -> bool {
        matches!(self, Self::Sequence)
    }

    const fn is_multiline(self) -> bool {
        matches!(
            self,
            Self::Sequence | Self::Match | Self::MatchAligned | Self::ForeignGroup
        )
    }

    const fn closes_body_indent(self) -> bool {
        !matches!(self, Self::MatchAligned)
    }
}

type ParenFrameList = Vec<ParenFrame>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BraceKind {
    Block,
    CommaList,
}

type BraceFrameList = Vec<BraceFrame>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BraceFrame {
    kind: BraceKind,
    continuation_indent: usize,
    saw_comma: bool,
}

impl BraceFrame {
    const fn new(kind: BraceKind, continuation_indent: usize) -> Self {
        Self {
            kind,
            continuation_indent,
            saw_comma: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ParenFrame {
    kind: ParenKind,
    broke: bool,
    saw_comma: bool,
    allows_trailing_comma: bool,
}

impl ParenFrame {
    const fn new(kind: ParenKind) -> Self {
        Self {
            kind,
            broke: false,
            saw_comma: false,
            allows_trailing_comma: false,
        }
    }

    const fn with_trailing_commas(kind: ParenKind, allows_trailing_comma: bool) -> Self {
        Self {
            kind,
            broke: false,
            saw_comma: false,
            allows_trailing_comma,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TokenFormatRole {
    #[default]
    Regular,
    CallParen,
    SequenceParen,
    MatchParen,
    ForeignGroupParen,
    ParamParen,
    MemberParamParen,
    TypeParamBracket,
    ArrayTypeBracket,
    CommaListBrace,
    Attribute,
    AttributeEnd,
}

impl<'a> SourceFormatter<'a> {
    const fn new(
        original: &'a str,
        options: &'a FormatOptions,
        protected_ranges: Vec<Range<usize>>,
    ) -> Self {
        Self {
            original,
            options,
            protected_ranges,
            protected_index: 0,
            protected_until: 0,
            out: String::new(),
            indent: 0,
            line_len: 0,
            at_line_start: true,
            previous: None,
            ignore_next: false,
            declaration_state: DeclarationState::None,
            declaration_head_active: false,
            parens: Vec::new(),
            braces: Vec::new(),
            last_token_end: 0,
            pending_attachment: false,
            line_start_paren_depth: 0,
            continuation_indent: 0,
        }
    }

    fn finish(mut self) -> String {
        self.trim_trailing_spaces();
        if !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.out
    }

    fn write_comment(&mut self, text: &str, kind: TriviaKind, span: Span) {
        let trimmed_text = text.trim_start();
        let is_item_doc = matches!(
            kind,
            TriviaKind::LineDocComment | TriviaKind::BlockDocComment
        ) || trimmed_text.starts_with("---")
            || starts_with_item_doc_block_comment(trimmed_text);
        let is_module_doc = matches!(
            kind,
            TriviaKind::LineModuleDocComment | TriviaKind::BlockModuleDocComment
        ) || trimmed_text.starts_with("--!")
            || starts_with_module_doc_block_comment(trimmed_text);
        let is_doc = is_item_doc || is_module_doc;
        let is_line = kind.is_line_comment();
        let is_same_line = self.trivia_starts_on_previous_token_line(span);
        if !is_same_line {
            self.preserve_blank_separator_if_needed(span);
        }
        if is_line && is_same_line && self.out.ends_with('\n') {
            let _ = self.out.pop();
            self.at_line_start = false;
        }
        if !self.at_line_start {
            self.push_space();
        }
        self.write_indent_if_needed();
        self.out.push_str(text.trim_end());
        if is_line || is_doc {
            self.newline();
        } else {
            self.push_space();
        }
        self.pending_attachment = is_item_doc && !is_same_line;
        self.set_last_token_end(span);
    }

    fn write_protected_if_needed(&mut self, span: Span) -> bool {
        let Some(start) = usize::try_from(span.start).ok() else {
            return false;
        };
        if start < self.protected_until {
            return true;
        }
        while let Some(range) = self.protected_ranges.get(self.protected_index) {
            if range.end <= start {
                self.protected_index = self.protected_index.saturating_add(1);
                continue;
            }
            if range.start > start {
                return false;
            }
            self.protected_until = range.end;
            self.write_protected_range(range.clone());
            self.protected_index = self.protected_index.saturating_add(1);
            return true;
        }
        false
    }

    fn write_protected_range(&mut self, range: Range<usize>) {
        if !self.at_line_start {
            self.newline();
        }
        let end = range.end;
        let Some(text) = self.original.get(range) else {
            return;
        };
        self.out.push_str(text.trim_end_matches([' ', '\t']));
        if !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.at_line_start = true;
        self.line_len = 0;
        self.previous = None;
        self.last_token_end = end;
        self.line_start_paren_depth = self.parens.len();
    }

    fn preserve_blank_separator_if_needed(&mut self, span: Span) {
        if !self.can_preserve_blank_separator() {
            return;
        }
        let Some(start) = usize::try_from(span.start).ok() else {
            return;
        };
        if start <= self.last_token_end {
            return;
        }
        let Some(between) = self.original.get(self.last_token_end..start) else {
            return;
        };
        if self.pending_attachment || self.out_ends_with_attachment_line() {
            return;
        }
        if newline_count(between) >= 2 {
            self.blank_line();
        }
    }

    fn can_preserve_blank_separator(&self) -> bool {
        self.indent == 0 && self.parens.is_empty() && self.out.ends_with('\n')
    }
}

impl SourceFormatter<'_> {
    fn write_token(
        &mut self,
        kind: TokenKind,
        text: &str,
        role: TokenFormatRole,
        span: Span,
        break_after_comma: bool,
        skip_current_comma: bool,
        break_before_operator: bool,
        break_after_colon_eq: bool,
        break_after_open_group: bool,
    ) {
        if self.ignore_next {
            self.write_original_token(kind, text);
            self.ignore_next = false;
            self.previous = Some(kind);
            self.set_last_token_end(span);
            return;
        }
        if break_before_operator {
            self.continuation_indent = self.continuation_indent.max(1);
            self.newline();
        }
        if skip_current_comma {
            self.set_last_token_end(span);
            return;
        }

        match kind {
            TokenKind::RBrace | TokenKind::RParen | TokenKind::RBracket => {
                let paren = matches!(kind, TokenKind::RParen | TokenKind::RBracket)
                    .then(|| self.parens.pop())
                    .flatten();
                let brace = (kind == TokenKind::RBrace)
                    .then(|| self.braces.pop())
                    .flatten();
                if self.should_insert_trailing_comma(paren) {
                    self.write_trailing_comma();
                }
                if self.should_insert_brace_trailing_comma(brace) {
                    self.write_trailing_comma();
                }
                if paren.is_some_and(|frame| {
                    matches!(frame.kind, ParenKind::Regular | ParenKind::Bracket)
                }) {
                    self.indent = self.indent.saturating_sub(1);
                }
                let multiline_close = paren.is_some_and(|frame| frame.kind.is_multiline());
                let broken_regular_close = paren.is_some_and(|frame| frame.broke);
                if let Some(frame) = brace {
                    self.continuation_indent = frame.continuation_indent;
                }
                if kind == TokenKind::RBrace
                    || paren.is_some_and(|frame| {
                        frame.kind.is_multiline() && frame.kind.closes_body_indent()
                    })
                {
                    self.indent = self.indent.saturating_sub(1);
                }
                if (kind == TokenKind::RBrace || multiline_close || broken_regular_close)
                    && !self.at_line_start
                {
                    self.newline();
                }
                if matches!(kind, TokenKind::RParen | TokenKind::RBracket)
                    && self.out.ends_with(", ")
                {
                    self.trim_trailing_spaces();
                }
                self.write_punct(kind, text);
                if paren.is_some_and(|frame| frame.kind.is_sequence()) {
                    self.indent = self.indent.saturating_sub(1);
                }
            }
            TokenKind::LBrace => self.write_open_brace(text, role),
            TokenKind::Semicolon => self.write_semicolon(),
            TokenKind::Comma => self.write_comma(break_after_comma),
            TokenKind::Pipe => self.write_pipe(text),
            TokenKind::LParen => self.write_open_paren(text, role, break_after_open_group),
            TokenKind::LBracket => self.write_open_bracket(text, role, break_after_open_group),
            _ => self.write_regular(kind, text, role),
        }
        if break_after_colon_eq {
            self.continuation_indent = self.continuation_indent.max(1);
            self.newline();
        }
        self.update_state(kind);
        self.previous = Some(kind);
        if role == TokenFormatRole::AttributeEnd {
            self.newline();
            self.pending_attachment = true;
        } else if role != TokenFormatRole::Attribute {
            self.pending_attachment = false;
        }
        self.set_last_token_end(span);
    }

    fn should_break_after_current_comma(&self, lexed: &LexedSource, token_index: usize) -> bool {
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

    fn should_skip_current_comma(&self, lexed: &LexedSource, token_index: usize) -> bool {
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

    fn should_break_before_current_operator(
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

    fn should_break_after_current_colon_eq(&self, lexed: &LexedSource, token_index: usize) -> bool {
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

    fn should_break_after_current_open_group(
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
            || self.should_break_after_comma()
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

    fn write_trailing_comma(&mut self) {
        if self.previous == Some(TokenKind::Comma) {
            return;
        }
        self.trim_trailing_spaces();
        self.write_indent_if_needed();
        self.out.push(',');
        self.line_len = self.line_len.saturating_add(1);
    }

    fn should_insert_trailing_comma(&self, paren: Option<ParenFrame>) -> bool {
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
            && !self.pending_attachment
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
            _ if kind != TokenKind::LParen => {
                if !matches!(kind, TokenKind::LBracket | TokenKind::RBracket) {
                    self.declaration_state = DeclarationState::None;
                }
            }
            _ => {}
        }
        if matches!(kind, TokenKind::ColonEq | TokenKind::Semicolon) {
            self.declaration_head_active = false;
        }
    }
}

impl SourceFormatter<'_> {
    fn needs_space_before(&self, current: TokenKind) -> bool {
        self.needs_space_before_with_role(current, TokenFormatRole::Regular)
    }

    fn needs_space_before_with_role(&self, current: TokenKind, role: TokenFormatRole) -> bool {
        let Some(previous) = self.previous else {
            return false;
        };
        if self.at_line_start {
            return false;
        }
        if is_closing(current) || matches!(current, TokenKind::Comma | TokenKind::Semicolon) {
            return false;
        }
        if matches!(
            previous,
            TokenKind::Dot | TokenKind::At | TokenKind::Hash | TokenKind::Backslash
        ) {
            return false;
        }
        if current == TokenKind::Dot && matches!(previous, TokenKind::ColonEq | TokenKind::Pipe) {
            return true;
        }
        if current == TokenKind::LBrace
            && (is_word_like(previous)
                || matches!(previous, TokenKind::RBracket | TokenKind::RParen))
        {
            return true;
        }
        if current == TokenKind::LBracket
            && matches!(
                role,
                TokenFormatRole::TypeParamBracket | TokenFormatRole::ArrayTypeBracket
            )
        {
            return true;
        }
        if current == TokenKind::LBracket && previous == TokenKind::Pipe {
            return true;
        }
        if matches!(current, TokenKind::Dot | TokenKind::LBracket) {
            return false;
        }
        if current == TokenKind::LParen && previous == TokenKind::KwMatch {
            return true;
        }
        if current == TokenKind::LParen {
            return false;
        }
        if matches!(previous, TokenKind::LParen | TokenKind::LBracket) {
            return false;
        }
        if matches!(previous, TokenKind::Colon) {
            return true;
        }
        if matches!(current, TokenKind::Colon) {
            return true;
        }
        if is_operator(previous) || is_operator(current) {
            return true;
        }
        is_word_like(previous) && is_word_like(current)
    }
}

impl SourceFormatter<'_> {
    const fn should_break_after_comma(&self) -> bool {
        false
    }

    fn maybe_break_before_token(&mut self, kind: TokenKind, text: &str, role: TokenFormatRole) {
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
            && self
                .parens
                .last()
                .is_some_and(|frame| matches!(frame.kind, ParenKind::Regular | ParenKind::Bracket))
            || self.previous == Some(TokenKind::ColonEq)
            || (self.options.operator_break == OperatorBreak::After
                && self.previous.is_some_and(is_operator))
            || (self.options.operator_break == OperatorBreak::Before
                && is_operator(current)
                && self.parens.is_empty())
    }

    fn write_indent_if_needed(&mut self) {
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

    fn projected_line_len(&self) -> usize {
        if self.at_line_start {
            return self
                .indent
                .saturating_add(self.continuation_indent)
                .saturating_mul(self.options.indent_unit().len());
        }
        self.line_len
    }

    fn push_space(&mut self) {
        if self.at_line_start || self.out.ends_with(' ') || self.out.ends_with('\n') {
            return;
        }
        self.out.push(' ');
        self.line_len = self.line_len.saturating_add(1);
    }

    fn newline(&mut self) {
        self.trim_trailing_spaces();
        if !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.line_len = 0;
        self.at_line_start = true;
        self.line_start_paren_depth = self.parens.len();
    }

    fn blank_line(&mut self) {
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

    fn trim_trailing_spaces(&mut self) {
        while self.out.ends_with(' ') || self.out.ends_with('\t') {
            let _ = self.out.pop();
        }
    }

    fn out_ends_with_attachment_line(&self) -> bool {
        let line = self.out.trim_end_matches('\n').lines().next_back();
        line.is_some_and(|line| {
            let trimmed = line.trim_start();
            trimmed.starts_with("---")
                || starts_with_item_doc_block_comment(trimmed)
                || trimmed.starts_with('@')
        })
    }

    fn trivia_starts_on_previous_token_line(&self, span: Span) -> bool {
        let Some(start) = usize::try_from(span.start).ok() else {
            return false;
        };
        if start < self.last_token_end {
            return false;
        }
        self.original
            .get(self.last_token_end..start)
            .is_some_and(|between| !between.contains('\n'))
    }

    fn set_last_token_end(&mut self, span: Span) {
        if let Ok(end) = usize::try_from(span.end) {
            self.last_token_end = end;
        }
    }
}

fn has_ignore_file(source: &str) -> bool {
    source
        .lines()
        .take(5)
        .any(|line| line.contains("musi-fmt-ignore-file"))
}

fn ensure_final_newline(source: &str) -> String {
    let mut text = source.trim_end_matches(['\r', '\n']).to_owned();
    text.push('\n');
    text
}

fn enforce_bind_line_width(text: String, options: &FormatOptions) -> String {
    if options.line_width == 0 {
        return text;
    }
    let mut out = String::with_capacity(text.len());
    for line in text.lines() {
        if line.chars().count() > options.line_width
            && is_let_line(line)
            && let Some(broken) = break_long_let_signature_line(line, options)
        {
            out.push_str(&broken);
            continue;
        }
        if line.chars().count() > options.line_width
            && is_let_line(line)
            && let Some(index) = line.find(" := ")
        {
            let rhs_start = index.saturating_add(" := ".len());
            let indent = line
                .chars()
                .take_while(|ch| ch.is_whitespace())
                .collect::<String>();
            out.push_str(line[..index + " :=".len()].trim_end());
            out.push('\n');
            out.push_str(&indent);
            out.push_str(&options.indent_unit());
            out.push_str(line[rhs_start..].trim_start());
            out.push('\n');
            continue;
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

fn break_long_let_signature_line(line: &str, options: &FormatOptions) -> Option<String> {
    let open = line.rfind(" (")?.saturating_add(1);
    let close = matching_close_paren(line, open)?;
    let inner = line.get(open + 1..close)?;
    if inner.trim().is_empty() || !inner.contains(',') {
        return None;
    }
    let indent = line
        .chars()
        .take_while(|ch| ch.is_whitespace())
        .collect::<String>();
    let item_indent = format!("{indent}{}", options.indent_unit());
    let mut out = String::new();
    out.push_str(line.get(..=open)?.trim_end());
    out.push('\n');
    for item in split_top_level_commas(inner) {
        out.push_str(&item_indent);
        out.push_str(item.trim());
        out.push_str(",\n");
    }
    if out.ends_with(",\n") {
        out.truncate(out.len().saturating_sub(2));
        out.push('\n');
    }
    out.push_str(&indent);
    out.push(')');
    out.push_str(line.get(close + 1..)?.trim_end());
    out.push('\n');
    Some(out)
}

fn matching_close_paren(line: &str, open: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (index, ch) in line.char_indices().skip_while(|(index, _)| *index < open) {
        match ch {
            '(' => depth = depth.saturating_add(1),
            ')' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Some(index);
                }
            }
            _ => {}
        }
    }
    None
}

fn split_top_level_commas(text: &str) -> Vec<&str> {
    let mut items = Vec::new();
    let mut start = 0usize;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    for (index, ch) in text.char_indices() {
        match ch {
            '(' => paren_depth = paren_depth.saturating_add(1),
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '[' => bracket_depth = bracket_depth.saturating_add(1),
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            ',' if paren_depth == 0 && bracket_depth == 0 => {
                if let Some(item) = text.get(start..index) {
                    items.push(item);
                }
                start = index.saturating_add(1);
            }
            _ => {}
        }
    }
    if let Some(item) = text.get(start..) {
        items.push(item);
    }
    items
}

fn compact_record_fields(text: String, options: &FormatOptions) -> String {
    if options.record_field_layout == GroupLayout::Block || options.line_width == 0 {
        return text;
    }
    let mut out = Vec::new();
    let lines: Vec<&str> = text.lines().collect();
    let mut index = 0usize;
    while index < lines.len() {
        let line = lines[index];
        if !line.trim_end().ends_with('{') {
            out.push(line.to_owned());
            index = index.saturating_add(1);
            continue;
        }
        let mut cursor = index.saturating_add(1);
        let mut fields = Vec::new();
        while let Some(field_line) = lines.get(cursor).copied() {
            let trimmed = field_line.trim();
            if trimmed == "};" {
                break;
            }
            if !trimmed.ends_with(',') || trimmed.contains('{') || trimmed.contains('}') {
                fields.clear();
                break;
            }
            fields.push(trimmed.trim_end_matches(',').to_owned());
            cursor = cursor.saturating_add(1);
        }
        if fields.is_empty() || lines.get(cursor).copied() != Some("};") {
            out.push(line.to_owned());
            index = index.saturating_add(1);
            continue;
        }
        let prefix = line.trim_end().trim_end_matches('{').trim_end();
        let candidate = format!("{prefix} {{ {} }};", fields.join(", "));
        if candidate.chars().count() > options.line_width {
            out.push(line.to_owned());
            index = index.saturating_add(1);
            continue;
        }
        out.push(candidate);
        index = cursor.saturating_add(1);
    }
    let mut formatted = out.join("\n");
    if text.ends_with('\n') {
        formatted.push('\n');
    }
    formatted
}

fn align_match_arrows(text: String, options: &FormatOptions) -> String {
    match options.match_arm_arrow_alignment {
        MatchArmArrowAlignment::None => text,
        MatchArmArrowAlignment::Consecutive => {
            align_match_arrow_runs(text, options, MatchArmArrowAlignment::Consecutive)
        }
        MatchArmArrowAlignment::Block => {
            align_match_arrow_runs(text, options, MatchArmArrowAlignment::Block)
        }
    }
}

fn align_match_arrow_runs(
    text: String,
    options: &FormatOptions,
    alignment: MatchArmArrowAlignment,
) -> String {
    let mut lines: Vec<String> = text.lines().map(str::to_owned).collect();
    let mut run = Vec::new();
    let mut in_match = false;
    for index in 0..lines.len() {
        let trimmed = lines[index].trim_start();
        if trimmed.starts_with("match ") && trimmed.ends_with('(') {
            in_match = true;
            run.clear();
            continue;
        }
        if in_match && trimmed == ");" {
            align_match_arrow_run(&mut lines, &run, options);
            run.clear();
            in_match = false;
            continue;
        }
        if !in_match {
            continue;
        }
        if is_match_arm_line(trimmed) {
            run.push(index);
        } else if alignment == MatchArmArrowAlignment::Consecutive {
            align_match_arrow_run(&mut lines, &run, options);
            run.clear();
        }
    }
    align_match_arrow_run(&mut lines, &run, options);
    let mut out = lines.join("\n");
    if text.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn align_match_arrow_run(lines: &mut [String], run: &[usize], options: &FormatOptions) {
    if run.len() < 2 {
        return;
    }
    let Some(target) = run
        .iter()
        .filter_map(|index| match_arrow_index(lines.get(*index)?))
        .max()
    else {
        return;
    };
    let mut aligned = Vec::with_capacity(run.len());
    for index in run {
        let Some(line) = lines.get(*index) else {
            return;
        };
        let Some(arrow) = match_arrow_index(line) else {
            return;
        };
        let padding = target.saturating_sub(arrow);
        let mut next = String::with_capacity(line.len().saturating_add(padding));
        next.push_str(line.get(..arrow).unwrap_or_default().trim_end());
        for _ in 0..=padding {
            next.push(' ');
        }
        next.push_str(line.get(arrow..).unwrap_or_default());
        if options.line_width > 0 && next.chars().count() > options.line_width {
            return;
        }
        aligned.push((*index, next));
    }
    for (index, line) in aligned {
        if let Some(target_line) = lines.get_mut(index) {
            *target_line = line;
        }
    }
}

fn is_match_arm_line(trimmed: &str) -> bool {
    trimmed.starts_with("| ") && trimmed.contains("=>")
}

fn match_arrow_index(line: &str) -> Option<usize> {
    line.find("=>")
}

fn next_non_comma_token_kind(lexed: &LexedSource, start_index: usize) -> Option<TokenKind> {
    lexed
        .tokens()
        .iter()
        .skip(start_index)
        .find(|token| token.kind != TokenKind::Comma)
        .map(|token| token.kind)
}

fn is_let_line(line: &str) -> bool {
    let trimmed = line.trim_start();
    trimmed.starts_with("let ")
        || trimmed.starts_with("let(")
        || trimmed.starts_with("export let ")
        || trimmed.starts_with("export let(")
        || trimmed.starts_with("foreign let ")
        || trimmed.starts_with("foreign let(")
        || trimmed.starts_with("export foreign ")
}

fn newline_count(text: &str) -> usize {
    text.bytes().filter(|byte| *byte == b'\n').count()
}

const fn is_closing(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::RBrace | TokenKind::RParen | TokenKind::RBracket
    )
}
