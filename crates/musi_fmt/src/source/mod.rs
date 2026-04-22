use std::ops::Range;

use music_base::Span;
use music_syntax::{LexedSource, Lexer, TokenKind, TriviaKind, parse};

use crate::{
    FormatError, FormatOptions, FormatResultOf, OperatorBreak,
    imports::organize_imports,
    protected::protected_line_ranges,
    roles::collect_token_roles,
    token_class::{is_operator, is_word_like},
};

mod postprocess;
mod token;

use postprocess::{align_match_arrows, compact_record_fields, enforce_bind_line_width};
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
        let token_options = TokenWriteOptions::empty()
            .with_break_after_comma(formatter.should_break_after_current_comma(&lexed, index))
            .with_skip_current_comma(formatter.should_skip_current_comma(&lexed, index))
            .with_break_before_operator(
                formatter.should_break_before_current_operator(&lexed, index),
            )
            .with_break_after_colon_eq(formatter.should_break_after_current_colon_eq(&lexed, index))
            .with_break_after_open_group(
                formatter.should_break_after_current_open_group(&lexed, index, role),
            );
        formatter.write_token(token.kind, text, role, token.span, token_options);
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
    pending_attachment: PendingAttachment,
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
enum PendingAttachment {
    None,
    ItemDoc,
}

impl PendingAttachment {
    const fn from_item_doc(is_item_doc: bool) -> Self {
        if is_item_doc {
            Self::ItemDoc
        } else {
            Self::None
        }
    }

    const fn is_pending(self) -> bool {
        matches!(self, Self::ItemDoc)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TokenWriteOptions(u8);

impl TokenWriteOptions {
    const BREAK_AFTER_COMMA: u8 = 1 << 0;
    const SKIP_CURRENT_COMMA: u8 = 1 << 1;
    const BREAK_BEFORE_OPERATOR: u8 = 1 << 2;
    const BREAK_AFTER_COLON_EQ: u8 = 1 << 3;
    const BREAK_AFTER_OPEN_GROUP: u8 = 1 << 4;

    const fn empty() -> Self {
        Self(0)
    }

    const fn with_break_after_comma(self, enabled: bool) -> Self {
        self.with_flag(Self::BREAK_AFTER_COMMA, enabled)
    }

    const fn with_skip_current_comma(self, enabled: bool) -> Self {
        self.with_flag(Self::SKIP_CURRENT_COMMA, enabled)
    }

    const fn with_break_before_operator(self, enabled: bool) -> Self {
        self.with_flag(Self::BREAK_BEFORE_OPERATOR, enabled)
    }

    const fn with_break_after_colon_eq(self, enabled: bool) -> Self {
        self.with_flag(Self::BREAK_AFTER_COLON_EQ, enabled)
    }

    const fn with_break_after_open_group(self, enabled: bool) -> Self {
        self.with_flag(Self::BREAK_AFTER_OPEN_GROUP, enabled)
    }

    const fn break_after_comma(self) -> bool {
        self.has_flag(Self::BREAK_AFTER_COMMA)
    }

    const fn skip_current_comma(self) -> bool {
        self.has_flag(Self::SKIP_CURRENT_COMMA)
    }

    const fn break_before_operator(self) -> bool {
        self.has_flag(Self::BREAK_BEFORE_OPERATOR)
    }

    const fn break_after_colon_eq(self) -> bool {
        self.has_flag(Self::BREAK_AFTER_COLON_EQ)
    }

    const fn break_after_open_group(self) -> bool {
        self.has_flag(Self::BREAK_AFTER_OPEN_GROUP)
    }

    const fn with_flag(self, flag: u8, enabled: bool) -> Self {
        if enabled {
            Self(self.0 | flag)
        } else {
            Self(self.0 & !flag)
        }
    }

    const fn has_flag(self, flag: u8) -> bool {
        self.0 & flag != 0
    }
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
            pending_attachment: PendingAttachment::None,
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
        self.pending_attachment = PendingAttachment::from_item_doc(is_item_doc && !is_same_line);
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
        if self.pending_attachment.is_pending() || self.out_ends_with_attachment_line() {
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
    const fn should_break_after_comma() -> bool {
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
        || trimmed.starts_with("native let ")
        || trimmed.starts_with("native let(")
        || trimmed.starts_with("export native ")
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
