use std::fs;
use std::path::Path;

use music_base::{Source, SourceMap, Span};
use music_syntax::{Lexer, SyntaxElement, SyntaxNode, parse};

use crate::analysis::{ToolPosition, ToolRange, tool_range};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolSelectionRange {
    pub range: ToolRange,
    pub parent: Option<Box<Self>>,
}

#[must_use]
pub fn selection_ranges_for_project_file(
    path: &Path,
    positions: &[ToolPosition],
) -> Vec<Option<ToolSelectionRange>> {
    selection_ranges_for_project_file_with_overlay(path, None, positions)
}

#[must_use]
pub fn selection_ranges_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    positions: &[ToolPosition],
) -> Vec<Option<ToolSelectionRange>> {
    let source_text = overlay_text
        .map(str::to_owned)
        .or_else(|| fs::read_to_string(path).ok())
        .unwrap_or_default();
    let mut sources = SourceMap::new();
    let Ok(source_id) = sources.add(path.to_path_buf(), source_text) else {
        return positions.iter().map(|_| None).collect();
    };
    let Some(source) = sources.get(source_id) else {
        return positions.iter().map(|_| None).collect();
    };
    let lexed = Lexer::new(source.text()).lex();
    let parsed = parse(lexed);
    let root = parsed.tree().root();
    positions
        .iter()
        .map(|position| selection_range_at_position(source, root, *position))
        .collect()
}

fn selection_range_at_position(
    source: &Source,
    root: SyntaxNode<'_, '_>,
    position: ToolPosition,
) -> Option<ToolSelectionRange> {
    let offset = source.offset(position.line, position.col)?;
    let element = deepest_element_at_offset(root, offset)?;
    let mut spans = Vec::new();
    spans.push(element.span());
    let mut parent = match element {
        SyntaxElement::Node(node) => node.parent(),
        SyntaxElement::Token(token) => token.parent(),
    };
    while let Some(node) = parent {
        spans.push(node.span());
        parent = node.parent();
    }
    selection_range_from_spans(source, spans)
}

fn deepest_element_at_offset<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
    offset: u32,
) -> Option<SyntaxElement<'tree, 'src>> {
    if !contains_offset(node.span(), offset) {
        return None;
    }
    for child in node.children() {
        if contains_offset(child.span(), offset) {
            if let SyntaxElement::Node(child_node) = child
                && let Some(deepest) = deepest_element_at_offset(child_node, offset)
            {
                return Some(deepest);
            }
            return Some(child);
        }
    }
    Some(SyntaxElement::Node(node))
}

fn selection_range_from_spans(source: &Source, spans: Vec<Span>) -> Option<ToolSelectionRange> {
    let mut ranges = Vec::new();
    for span in spans {
        if span.is_empty() {
            continue;
        }
        let range = tool_range(source, span);
        if ranges.last().is_none_or(|previous| previous != &range) {
            ranges.push(range);
        }
    }
    let mut parent = None;
    for range in ranges.into_iter().rev() {
        parent = Some(Box::new(ToolSelectionRange { range, parent }));
    }
    parent.map(|selection| *selection)
}

const fn contains_offset(span: Span, offset: u32) -> bool {
    span.start <= offset && offset < span.end
}
