use std::fs;
use std::path::Path;

use music_base::{Source, SourceMap, Span};
use music_syntax::{
    LexedSource, Lexer, SyntaxElement, SyntaxNode, SyntaxNodeKind, TriviaKind, parse,
};

use crate::analysis::{ToolRange, tool_range};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolFoldingRangeKind {
    Comment,
    Imports,
    Region,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolFoldingRange {
    pub range: ToolRange,
    pub kind: Option<ToolFoldingRangeKind>,
}

#[must_use]
pub fn folding_ranges_for_project_file(path: &Path) -> Vec<ToolFoldingRange> {
    folding_ranges_for_project_file_with_overlay(path, None)
}

#[must_use]
pub fn folding_ranges_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> Vec<ToolFoldingRange> {
    let source_text = overlay_text
        .map(str::to_owned)
        .or_else(|| fs::read_to_string(path).ok())
        .unwrap_or_default();
    let mut sources = SourceMap::new();
    let Ok(source_id) = sources.add(path.to_path_buf(), source_text) else {
        return Vec::new();
    };
    let Some(source) = sources.get(source_id) else {
        return Vec::new();
    };
    let lexed = Lexer::new(source.text()).lex();
    let parsed = parse(lexed.clone());
    let mut ranges = Vec::new();
    collect_node_folds(source, parsed.tree().root(), &mut ranges);
    collect_comment_folds(source, &lexed, &mut ranges);
    ranges.sort_by_key(|fold| {
        (
            fold.range.start_line,
            fold.range.start_col,
            fold.range.end_line,
            fold.range.end_col,
        )
    });
    ranges.dedup_by_key(|fold| {
        (
            fold.range.start_line,
            fold.range.start_col,
            fold.range.end_line,
            fold.range.end_col,
        )
    });
    ranges
}

fn collect_node_folds(source: &Source, node: SyntaxNode<'_, '_>, out: &mut Vec<ToolFoldingRange>) {
    if is_foldable_node(node.kind()) {
        push_fold(source, node.span(), None, out);
    }
    for child in node.children() {
        if let SyntaxElement::Node(child_node) = child {
            collect_node_folds(source, child_node, out);
        }
    }
}

fn collect_comment_folds(source: &Source, lexed: &LexedSource, out: &mut Vec<ToolFoldingRange>) {
    for trivia in lexed.trivia() {
        if matches!(
            trivia.kind,
            TriviaKind::BlockComment
                | TriviaKind::BlockDocComment
                | TriviaKind::BlockModuleDocComment
        ) {
            push_fold(
                source,
                trivia.span,
                Some(ToolFoldingRangeKind::Comment),
                out,
            );
        }
    }
}

fn push_fold(
    source: &Source,
    span: Span,
    kind: Option<ToolFoldingRangeKind>,
    out: &mut Vec<ToolFoldingRange>,
) {
    let range = tool_range(source, span);
    if range.start_line < range.end_line {
        out.push(ToolFoldingRange { range, kind });
    }
}

const fn is_foldable_node(kind: SyntaxNodeKind) -> bool {
    matches!(
        kind,
        SyntaxNodeKind::SequenceExpr
            | SyntaxNodeKind::DataExpr
            | SyntaxNodeKind::ShapeExpr
            | SyntaxNodeKind::ForeignBlockExpr
            | SyntaxNodeKind::MatchExpr
            | SyntaxNodeKind::MatchArm
            | SyntaxNodeKind::TupleExpr
            | SyntaxNodeKind::ArrayExpr
            | SyntaxNodeKind::RecordExpr
            | SyntaxNodeKind::RecordUpdateExpr
            | SyntaxNodeKind::VariantPayloadList
            | SyntaxNodeKind::VariantFieldDef
            | SyntaxNodeKind::ParamList
            | SyntaxNodeKind::FieldList
            | SyntaxNodeKind::VariantList
            | SyntaxNodeKind::TypeParamList
            | SyntaxNodeKind::ConstraintList
            | SyntaxNodeKind::MemberList
    )
}
