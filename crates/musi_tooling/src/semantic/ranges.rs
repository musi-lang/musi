use std::cmp::Reverse;
use std::collections::BTreeMap;

use music_base::{Source, Span};

use super::model::{
    SemanticTokenSink, ToolSemanticModifierList, ToolSemanticToken, ToolSemanticTokenKind,
    ToolSemanticTokenList,
};
use crate::analysis::{ToolRange, tool_range};

type SemanticRangeKey = (usize, usize, usize, usize);

pub(super) fn token_range_priorities(
    tokens: &[ToolSemanticToken],
) -> BTreeMap<SemanticRangeKey, u8> {
    let mut priorities: BTreeMap<SemanticRangeKey, u8> = BTreeMap::new();
    for token in tokens {
        let key = tool_range_key(token.range);
        let priority = token_priority(token.kind);
        let _ = priorities
            .entry(key)
            .and_modify(|stored| *stored = (*stored).max(priority))
            .or_insert(priority);
    }
    priorities
}

pub(super) fn range_key(source: &Source, span: Span) -> SemanticRangeKey {
    tool_range_key(tool_range(source, span))
}

pub(super) const fn tool_range_key(range: ToolRange) -> SemanticRangeKey {
    (
        range.start_line,
        range.start_col,
        range.end_line,
        range.end_col,
    )
}

pub(super) fn push_span_tokens(
    source: &Source,
    out: SemanticTokenSink<'_>,
    span: Span,
    kind: ToolSemanticTokenKind,
    modifiers: ToolSemanticModifierList,
) {
    let range = tool_range(source, span);
    if range.start_line == range.end_line {
        if range.start_col < range.end_col {
            out.push(ToolSemanticToken::new(range, kind, modifiers));
        }
        return;
    }
    for line in range.start_line..=range.end_line {
        let Some(line_text) = source.line_text(line) else {
            continue;
        };
        let start_col = if line == range.start_line {
            range.start_col
        } else {
            1
        };
        let end_col = if line == range.end_line {
            range.end_col
        } else {
            line_text.chars().count().saturating_add(1)
        };
        if start_col < end_col {
            out.push(ToolSemanticToken::new(
                ToolRange::new(line, start_col, line, end_col),
                kind,
                modifiers.clone(),
            ));
        }
    }
}
pub(super) const fn token_priority(kind: ToolSemanticTokenKind) -> u8 {
    match kind {
        ToolSemanticTokenKind::Function | ToolSemanticTokenKind::Procedure => 80,
        ToolSemanticTokenKind::Parameter => 70,
        ToolSemanticTokenKind::EnumMember | ToolSemanticTokenKind::Decorator => 60,
        ToolSemanticTokenKind::TypeParameter => 50,
        ToolSemanticTokenKind::Type => 40,
        ToolSemanticTokenKind::Namespace | ToolSemanticTokenKind::Property => 30,
        ToolSemanticTokenKind::Variable => 10,
        _ => 0,
    }
}

pub(super) fn normalize_tokens(mut tokens: ToolSemanticTokenList) -> ToolSemanticTokenList {
    tokens.sort_by_key(|token| {
        (
            token.range.start_line,
            token.range.start_col,
            token.range.end_line,
            token.range.end_col,
            Reverse(token_priority(token.kind)),
        )
    });
    let mut out = Vec::new();
    let mut last_line = 0usize;
    let mut last_col = 0usize;
    for token in tokens {
        if token.range.start_line < last_line
            || (token.range.start_line == last_line && token.range.start_col < last_col)
        {
            continue;
        }
        last_line = token.range.end_line;
        last_col = token.range.end_col;
        out.push(token);
    }
    out
}
