use std::ops::Range;

use music_base::Span;
use music_syntax::{LexedSource, Lexer, TokenKind, TriviaKind, parse};

use crate::{
    BracePosition, FormatError, FormatOptions, FormatResultOf, TrailingCommas,
    imports::organize_imports,
    line_width::regular_group_next_segment_len,
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
        formatter.write_token(token.kind, text, role, token.span, break_after_comma);
    }
    let formatted_text = formatter.finish();
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
    parens: ParenFrameList,
    last_token_end: usize,
    pending_attachment: bool,
    line_start_paren_depth: usize,
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
    ForeignGroup,
}

impl ParenKind {
    const fn is_sequence(self) -> bool {
        matches!(self, Self::Sequence)
    }

    const fn is_multiline(self) -> bool {
        matches!(self, Self::Sequence | Self::Match | Self::ForeignGroup)
    }
}

type ParenFrameList = Vec<ParenFrame>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ParenFrame {
    kind: ParenKind,
    broke: bool,
}

impl ParenFrame {
    const fn new(kind: ParenKind) -> Self {
        Self { kind, broke: false }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TokenFormatRole {
    #[default]
    Regular,
    SequenceParen,
    MatchParen,
    ForeignGroupParen,
    ParamParen,
    TypeParamBracket,
    ArrayTypeBracket,
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
            parens: Vec::new(),
            last_token_end: 0,
            pending_attachment: false,
            line_start_paren_depth: 0,
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
    ) {
        if self.ignore_next {
            self.write_original_token(kind, text);
            self.ignore_next = false;
            self.previous = Some(kind);
            self.set_last_token_end(span);
            return;
        }

        match kind {
            TokenKind::RBrace | TokenKind::RParen | TokenKind::RBracket => {
                let paren = matches!(kind, TokenKind::RParen | TokenKind::RBracket)
                    .then(|| self.parens.pop())
                    .flatten();
                if paren.is_some_and(|frame| {
                    matches!(frame.kind, ParenKind::Regular | ParenKind::Bracket)
                }) {
                    self.indent = self.indent.saturating_sub(1);
                }
                let multiline_close = paren.is_some_and(|frame| frame.kind.is_multiline());
                let broken_regular_close = paren.is_some_and(|frame| frame.broke);
                if kind == TokenKind::RBrace || multiline_close {
                    self.indent = self.indent.saturating_sub(1);
                }
                if (kind == TokenKind::RBrace || multiline_close || broken_regular_close)
                    && !self.at_line_start
                {
                    self.newline();
                }
                self.write_punct(kind, text);
                if paren.is_some_and(|frame| frame.kind.is_sequence()) {
                    self.indent = self.indent.saturating_sub(1);
                }
            }
            TokenKind::LBrace => self.write_open_brace(text),
            TokenKind::Semicolon => self.write_semicolon(),
            TokenKind::Comma => self.write_comma(break_after_comma),
            TokenKind::Pipe => self.write_pipe(text),
            TokenKind::LParen => self.write_open_paren(text, role),
            TokenKind::LBracket => self.write_open_bracket(text, role),
            _ => self.write_regular(kind, text, role),
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
                .is_some_and(|frame| frame.kind == ParenKind::Regular)
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

    fn write_original_token(&mut self, _kind: TokenKind, text: &str) {
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
    }

    fn write_open_brace(&mut self, text: &str) {
        if self.options.brace_position == BracePosition::NextLine && !self.at_line_start {
            self.newline();
        } else if self.needs_space_before(TokenKind::LBrace) {
            self.push_space();
        }
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
        self.indent = self.indent.saturating_add(1);
        self.newline();
    }

    fn write_open_paren(&mut self, text: &str, role: TokenFormatRole) {
        let paren = match role {
            TokenFormatRole::SequenceParen => Some(ParenKind::Sequence),
            TokenFormatRole::MatchParen => Some(ParenKind::Match),
            TokenFormatRole::ForeignGroupParen => Some(ParenKind::ForeignGroup),
            _ => None,
        };
        if let Some(paren) = paren {
            if matches!(paren, ParenKind::Sequence) {
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
            self.indent = self.indent.saturating_add(1);
            self.newline();
            return;
        }
        if role == TokenFormatRole::ParamParen
            || self.declaration_state == DeclarationState::NameBeforeParams
            || self.needs_space_before(TokenKind::LParen)
        {
            self.push_space();
        }
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
        self.parens.push(ParenFrame::new(ParenKind::Regular));
        self.indent = self.indent.saturating_add(1);
        self.declaration_state = DeclarationState::None;
    }

    fn write_open_bracket(&mut self, text: &str, role: TokenFormatRole) {
        if matches!(
            role,
            TokenFormatRole::TypeParamBracket | TokenFormatRole::ArrayTypeBracket
        ) || self.previous == Some(TokenKind::ColonEq)
        {
            self.push_space();
        }
        self.write_punct(TokenKind::LBracket, text);
        self.parens.push(ParenFrame::new(ParenKind::Bracket));
        self.indent = self.indent.saturating_add(1);
    }

    fn write_semicolon(&mut self) {
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
        self.write_indent_if_needed();
        self.out.push(',');
        self.line_len = self.line_len.saturating_add(1);
        if break_after_comma || self.should_break_after_comma() {
            if let Some(frame) = self.parens.last_mut() {
                frame.broke = true;
            }
            self.newline();
        } else {
            self.push_space();
        }
    }

    fn write_punct(&mut self, _kind: TokenKind, text: &str) {
        self.write_indent_if_needed();
        self.out.push_str(text);
        self.line_len = self.line_len.saturating_add(text.len());
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
        if current == TokenKind::LBrace && is_word_like(previous) {
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
        match self.options.trailing_commas {
            TrailingCommas::Always => true,
            TrailingCommas::Never | TrailingCommas::MultiLine => false,
        }
    }

    fn maybe_break_before_token(&mut self, kind: TokenKind, text: &str, role: TokenFormatRole) {
        if self.options.line_width == 0
            || self.at_line_start
            || !self.can_break_before_token()
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
        self.newline();
    }

    fn can_break_before_token(&self) -> bool {
        self.previous == Some(TokenKind::Comma)
            && self
                .parens
                .last()
                .is_some_and(|frame| frame.kind == ParenKind::Regular)
    }

    fn write_indent_if_needed(&mut self) {
        if !self.at_line_start {
            return;
        }
        let unit = self.options.indent_unit();
        for _ in 0..self.indent {
            self.out.push_str(&unit);
            self.line_len = self.line_len.saturating_add(unit.len());
        }
        self.at_line_start = false;
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

fn newline_count(text: &str) -> usize {
    text.bytes().filter(|byte| *byte == b'\n').count()
}

const fn is_closing(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::RBrace | TokenKind::RParen | TokenKind::RBracket
    )
}
