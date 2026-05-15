use std::ops::Range;

use music_base::Span;
use music_syntax::{TokenKind, TriviaKind};

use crate::FormatOptions;

use super::helpers::{
    newline_count, starts_with_item_doc_block_comment, starts_with_module_doc_block_comment,
};

pub(super) struct CstFormatter<'a> {
    pub(super) original: &'a str,
    pub(super) options: &'a FormatOptions,
    pub(super) protected_ranges: Vec<Range<usize>>,
    pub(super) protected_index: usize,
    pub(super) protected_until: usize,
    pub(super) out: String,
    pub(super) indent: usize,
    pub(super) line_len: usize,
    pub(super) at_line_start: bool,
    pub(super) before_previous: Option<TokenKind>,
    pub(super) previous: Option<TokenKind>,
    pub(super) ignore_next: bool,
    pub(super) declaration_state: DeclarationState,
    pub(super) declaration_head_active: bool,
    pub(super) parens: ParenFrameList,
    pub(super) braces: BraceFrameList,
    pub(super) last_token_end: usize,
    pub(super) pending_attachment: PendingAttachment,
    pub(super) line_start_paren_depth: usize,
    pub(super) continuation_indent: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum DeclarationState {
    None,
    WaitingName,
    NameBeforeParams,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PendingAttachment {
    None,
    ItemDoc,
}

impl PendingAttachment {
    pub(super) const fn from_item_doc(is_item_doc: bool) -> Self {
        if is_item_doc {
            Self::ItemDoc
        } else {
            Self::None
        }
    }

    pub(super) const fn is_pending(self) -> bool {
        matches!(self, Self::ItemDoc)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct TokenWriteOptions(u8);

impl TokenWriteOptions {
    const BREAK_AFTER_COMMA: u8 = 1 << 0;
    const SKIP_CURRENT_COMMA: u8 = 1 << 1;
    const BREAK_BEFORE_OPERATOR: u8 = 1 << 2;
    const BREAK_AFTER_COLON_EQ: u8 = 1 << 3;
    const BREAK_AFTER_OPEN_GROUP: u8 = 1 << 4;
    const BREAK_BEFORE_ELSE: u8 = 1 << 5;

    pub(super) const fn empty() -> Self {
        Self(0)
    }

    pub(super) const fn with_break_after_comma(self, enabled: bool) -> Self {
        self.with_flag(Self::BREAK_AFTER_COMMA, enabled)
    }

    pub(super) const fn with_skip_current_comma(self, enabled: bool) -> Self {
        self.with_flag(Self::SKIP_CURRENT_COMMA, enabled)
    }

    pub(super) const fn with_break_before_operator(self, enabled: bool) -> Self {
        self.with_flag(Self::BREAK_BEFORE_OPERATOR, enabled)
    }

    pub(super) const fn with_break_before_else(self, enabled: bool) -> Self {
        self.with_flag(Self::BREAK_BEFORE_ELSE, enabled)
    }

    pub(super) const fn with_break_after_colon_eq(self, enabled: bool) -> Self {
        self.with_flag(Self::BREAK_AFTER_COLON_EQ, enabled)
    }

    pub(super) const fn with_break_after_open_group(self, enabled: bool) -> Self {
        self.with_flag(Self::BREAK_AFTER_OPEN_GROUP, enabled)
    }

    pub(super) const fn break_after_comma(self) -> bool {
        self.has_flag(Self::BREAK_AFTER_COMMA)
    }

    pub(super) const fn skip_current_comma(self) -> bool {
        self.has_flag(Self::SKIP_CURRENT_COMMA)
    }

    pub(super) const fn break_before_operator(self) -> bool {
        self.has_flag(Self::BREAK_BEFORE_OPERATOR)
    }

    pub(super) const fn break_before_else(self) -> bool {
        self.has_flag(Self::BREAK_BEFORE_ELSE)
    }

    pub(super) const fn break_after_colon_eq(self) -> bool {
        self.has_flag(Self::BREAK_AFTER_COLON_EQ)
    }

    pub(super) const fn break_after_open_group(self) -> bool {
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
pub(super) enum ParenKind {
    Regular,
    Bracket,
    Sequence,
    UnsafeSequence,
    Match,
    MatchAligned,
    ForeignGroup,
}

impl ParenKind {
    pub(super) const fn is_sequence(self) -> bool {
        matches!(self, Self::Sequence)
    }

    pub(super) const fn is_multiline(self) -> bool {
        matches!(
            self,
            Self::Sequence
                | Self::UnsafeSequence
                | Self::Match
                | Self::MatchAligned
                | Self::ForeignGroup
        )
    }

    pub(super) const fn closes_body_indent(self) -> bool {
        !matches!(self, Self::MatchAligned)
    }
}

type ParenFrameList = Vec<ParenFrame>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum BraceKind {
    Block,
    CommaList,
}

type BraceFrameList = Vec<BraceFrame>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct BraceFrame {
    pub(super) kind: BraceKind,
    pub(super) continuation_indent: usize,
    pub(super) saw_comma: bool,
}

impl BraceFrame {
    pub(super) const fn new(kind: BraceKind, continuation_indent: usize) -> Self {
        Self {
            kind,
            continuation_indent,
            saw_comma: false,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) struct ParenFrame {
    pub(super) kind: ParenKind,
    pub(super) broke: bool,
    pub(super) saw_comma: bool,
    pub(super) allows_trailing_comma: bool,
}

impl ParenFrame {
    pub(super) const fn new(kind: ParenKind) -> Self {
        Self {
            kind,
            broke: false,
            saw_comma: false,
            allows_trailing_comma: false,
        }
    }

    pub(super) const fn with_trailing_commas(kind: ParenKind, allows_trailing_comma: bool) -> Self {
        Self {
            kind,
            broke: false,
            saw_comma: false,
            allows_trailing_comma,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub(super) enum CstLeafRole {
    #[default]
    Regular,
    CallParen,
    SequenceParen,
    MatchParen,
    ForeignGroupParen,
    ParamParen,
    MemberParamParen,
    TypeParamBracket,
    ApplyBracket,
    ArrayTypeBracket,
    CommaListBrace,
    Attribute,
    AttributeEnd,
}

impl<'a> CstFormatter<'a> {
    pub(super) const fn new(
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
            before_previous: None,
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

    pub(super) fn finish(mut self) -> String {
        self.trim_trailing_spaces();
        if !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.out
    }

    pub(super) fn write_comment(&mut self, text: &str, kind: TriviaKind, span: Span) {
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
        let is_leading_line = self.at_line_start;
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
        if is_line || is_doc || is_leading_line {
            self.newline();
        } else {
            self.push_space();
        }
        self.pending_attachment = PendingAttachment::from_item_doc(is_item_doc && !is_same_line);
        self.set_last_token_end(span);
    }

    pub(super) fn write_protected_if_needed(&mut self, span: Span) -> bool {
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
        self.before_previous = None;
        self.previous = None;
        self.last_token_end = end;
        self.line_start_paren_depth = self.parens.len();
    }

    pub(super) fn preserve_blank_separator_if_needed(&mut self, span: Span) {
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

    pub(super) fn out_ends_with_attachment_line(&self) -> bool {
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

    pub(super) fn set_last_token_end(&mut self, span: Span) {
        if let Ok(end) = usize::try_from(span.end) {
            self.last_token_end = end;
        }
    }
}
