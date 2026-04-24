use std::ops::Range;

use music_base::Span;
use music_syntax::{
    LexedSource, SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree, TokenKind,
    TriviaKind,
};

use crate::{
    FormatOptions, OperatorBreak,
    token_class::{is_operator, is_word_like},
};

mod layout;
mod token;

use layout::{format_bind_layout, format_match_arrow_layout, format_record_layout};

pub(super) fn format_cst_source(
    source: &str,
    tree: &SyntaxTree,
    options: &FormatOptions,
    protected_ranges: Vec<Range<usize>>,
) -> String {
    let lexed = tree.lexed();
    let mut formatter = CstFormatter::new(source, options, protected_ranges);
    let mut token_index = 0usize;
    formatter.write_node(tree.root(), lexed, &mut token_index, None);
    formatter.write_lexed_leaf_tail(lexed, &mut token_index);
    let formatted_text = format_bind_layout(formatter.finish(), options);
    let formatted_text = format_record_layout(formatted_text, options);
    format_match_arrow_layout(formatted_text, options)
}

impl CstFormatter<'_> {
    fn write_node(
        &mut self,
        node: SyntaxNode<'_, '_>,
        lexed: &LexedSource,
        token_index: &mut usize,
        attr_last_token: Option<(TokenKind, Span)>,
    ) {
        let attr_last_token = if node.kind() == SyntaxNodeKind::Attr {
            last_node_token(node).map(|token| (token.kind(), token.span()))
        } else {
            attr_last_token
        };
        for child in node.children() {
            match child {
                SyntaxElement::Node(child_node) => {
                    self.write_node(child_node, lexed, token_index, attr_last_token);
                }
                SyntaxElement::Token(token) => {
                    self.write_syntax_token(node, token, lexed, token_index, attr_last_token);
                }
            }
        }
    }

    fn write_syntax_token(
        &mut self,
        parent: SyntaxNode<'_, '_>,
        token: SyntaxToken<'_, '_>,
        lexed: &LexedSource,
        token_index: &mut usize,
        attr_last_token: Option<(TokenKind, Span)>,
    ) {
        if token.kind() == TokenKind::Eof {
            return;
        }
        let current_index = self.write_tokens_before(token, lexed, token_index);
        let role = attr_last_token.map_or_else(
            || leaf_role_for(parent, token.kind()),
            |(last_kind, last_span)| {
                if last_kind == token.kind() && last_span == token.span() {
                    CstLeafRole::AttributeEnd
                } else {
                    CstLeafRole::Attribute
                }
            },
        );
        self.write_lexed_leaf(lexed, current_index, role);
        *token_index = current_index.saturating_add(1);
    }

    fn write_tokens_before(
        &mut self,
        token: SyntaxToken<'_, '_>,
        lexed: &LexedSource,
        token_index: &mut usize,
    ) -> usize {
        while let Some(lexed_token) = lexed.tokens().get(*token_index) {
            if lexed_token.kind == token.kind() && lexed_token.span == token.span() {
                return *token_index;
            }
            self.write_lexed_leaf(lexed, *token_index, CstLeafRole::Regular);
            *token_index = token_index.saturating_add(1);
        }
        token_index.saturating_sub(1)
    }

    fn write_lexed_leaf_tail(&mut self, lexed: &LexedSource, token_index: &mut usize) {
        while let Some(token) = lexed.tokens().get(*token_index) {
            if token.kind == TokenKind::Eof {
                break;
            }
            self.write_lexed_leaf(lexed, *token_index, CstLeafRole::Regular);
            *token_index = token_index.saturating_add(1);
        }
    }

    fn write_lexed_leaf(&mut self, lexed: &LexedSource, token_index: usize, role: CstLeafRole) {
        let Some(token) = lexed.tokens().get(token_index) else {
            return;
        };
        if token.kind == TokenKind::Eof {
            return;
        }
        for trivia in lexed.token_trivia(token_index) {
            if self.write_protected_if_needed(trivia.span) {
                continue;
            }
            if trivia.kind.is_comment() {
                let Some(text) = self.original.get(
                    usize::try_from(trivia.span.start).unwrap_or(usize::MAX)
                        ..usize::try_from(trivia.span.end).unwrap_or(usize::MAX),
                ) else {
                    continue;
                };
                self.write_comment(text, trivia.kind, trivia.span);
            }
        }
        if self.write_protected_if_needed(token.span) {
            return;
        }
        let Some(text) = lexed.token_text(token_index) else {
            return;
        };
        self.preserve_blank_separator_if_needed(token.span);
        let token_options = TokenWriteOptions::empty()
            .with_break_after_comma(self.should_break_after_current_comma(lexed, token_index))
            .with_skip_current_comma(self.should_skip_current_comma(lexed, token_index))
            .with_break_before_operator(
                self.should_break_before_current_operator(lexed, token_index),
            )
            .with_break_after_colon_eq(self.should_break_after_current_colon_eq(lexed, token_index))
            .with_break_after_open_group(self.should_break_after_current_open_group(
                lexed,
                token_index,
                role,
            ));
        self.write_token(token.kind, text, role, token.span, token_options);
    }
}

fn last_node_token<'tree, 'src>(node: SyntaxNode<'tree, 'src>) -> Option<SyntaxToken<'tree, 'src>> {
    node.children().filter_map(last_element_token).last()
}

fn last_element_token<'tree, 'src>(
    element: SyntaxElement<'tree, 'src>,
) -> Option<SyntaxToken<'tree, 'src>> {
    match element {
        SyntaxElement::Node(node) => last_node_token(node),
        SyntaxElement::Token(token) => Some(token),
    }
}

fn leaf_role_for(parent: SyntaxNode<'_, '_>, token: TokenKind) -> CstLeafRole {
    match (parent.kind(), token) {
        (SyntaxNodeKind::SequenceExpr, TokenKind::LParen | TokenKind::RParen) => {
            CstLeafRole::SequenceParen
        }
        (SyntaxNodeKind::MatchExpr, TokenKind::LParen | TokenKind::RParen) => {
            CstLeafRole::MatchParen
        }
        (SyntaxNodeKind::MemberList, TokenKind::LParen | TokenKind::RParen) => {
            CstLeafRole::ForeignGroupParen
        }
        (SyntaxNodeKind::CallExpr, TokenKind::LParen | TokenKind::RParen) => CstLeafRole::CallParen,
        (SyntaxNodeKind::ParamList, TokenKind::LParen | TokenKind::RParen)
            if parent
                .parent()
                .is_some_and(|node| node.kind() == SyntaxNodeKind::Member) =>
        {
            CstLeafRole::MemberParamParen
        }
        (SyntaxNodeKind::ParamList, TokenKind::LParen | TokenKind::RParen) => {
            CstLeafRole::ParamParen
        }
        (SyntaxNodeKind::TypeParamList, TokenKind::LBracket | TokenKind::RBracket) => {
            CstLeafRole::TypeParamBracket
        }
        (SyntaxNodeKind::ArrayTy, TokenKind::LBracket | TokenKind::RBracket) => {
            CstLeafRole::ArrayTypeBracket
        }
        (
            SyntaxNodeKind::EffectSet | SyntaxNodeKind::RecordExpr | SyntaxNodeKind::RecordPat,
            TokenKind::LBrace | TokenKind::RBrace,
        ) => CstLeafRole::CommaListBrace,
        (SyntaxNodeKind::Attr, _) => CstLeafRole::Attribute,
        _ => CstLeafRole::Regular,
    }
}

fn starts_with_item_doc_block_comment(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'-', ..])
}

fn starts_with_module_doc_block_comment(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'!', ..])
}

struct CstFormatter<'a> {
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
enum CstLeafRole {
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

impl<'a> CstFormatter<'a> {
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

impl CstFormatter<'_> {
    fn needs_space_before(&self, current: TokenKind) -> bool {
        self.needs_space_before_with_role(current, CstLeafRole::Regular)
    }

    fn needs_space_before_with_role(&self, current: TokenKind, role: CstLeafRole) -> bool {
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
                CstLeafRole::TypeParamBracket | CstLeafRole::ArrayTypeBracket
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

impl CstFormatter<'_> {
    const fn should_break_after_comma() -> bool {
        false
    }

    fn maybe_break_before_token(&mut self, kind: TokenKind, text: &str, role: CstLeafRole) {
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
