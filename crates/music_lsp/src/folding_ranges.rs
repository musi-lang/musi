//! Folding ranges: code folding for multi-line constructs.

use lsp_types::{FoldingRange, FoldingRangeKind};
use music_ast::Expr;
use music_lex::TriviaKind;
use music_shared::Idx;

use crate::analysis::{AnalyzedDoc, offset_to_position};

pub fn folding_ranges(doc: &AnalyzedDoc) -> Vec<FoldingRange> {
    let mut ranges = vec![];

    for idx in 0..doc.module.arenas.exprs.len() {
        let idx = Idx::from_raw(u32::try_from(idx).unwrap_or(0));
        let span = match &doc.module.arenas.exprs[idx] {
            Expr::Block { span, .. }
            | Expr::Fn { span, .. }
            | Expr::Match { span, .. }
            | Expr::Handle { span, .. }
            | Expr::Class { span, .. }
            | Expr::Effect { span, .. }
            | Expr::Instance { span, .. }
            | Expr::Choice { span, .. } => *span,
            _ => continue,
        };

        let start = offset_to_position(doc.file_id, span.start, &doc.source_db);
        let end = offset_to_position(doc.file_id, span.end(), &doc.source_db);

        if end.line > start.line {
            ranges.push(FoldingRange {
                start_line: start.line,
                start_character: Some(start.character),
                end_line: end.line,
                end_character: Some(end.character),
                kind: Some(FoldingRangeKind::Region),
                collapsed_text: None,
            });
        }
    }

    // Consecutive doc-comment (///) trivia lines fold as a comment block.
    let mut comment_start: Option<u32> = None;
    let mut comment_end: Option<u32> = None;

    for trivia in &doc.lexed.trivia {
        if matches!(trivia.kind, TriviaKind::LineComment { doc_style: true }) {
            let pos = offset_to_position(doc.file_id, trivia.span.start, &doc.source_db);
            let end_pos = offset_to_position(doc.file_id, trivia.span.end(), &doc.source_db);
            if comment_start.is_none() {
                comment_start = Some(pos.line);
            }
            comment_end = Some(end_pos.line);
        } else {
            if let (Some(start_line), Some(end_line)) = (comment_start, comment_end)
                && end_line > start_line
            {
                ranges.push(FoldingRange {
                    start_line,
                    start_character: None,
                    end_line,
                    end_character: None,
                    kind: Some(FoldingRangeKind::Comment),
                    collapsed_text: None,
                });
            }
            comment_start = None;
            comment_end = None;
        }
    }

    if let (Some(start_line), Some(end_line)) = (comment_start, comment_end)
        && end_line > start_line
    {
        ranges.push(FoldingRange {
            start_line,
            start_character: None,
            end_line,
            end_character: None,
            kind: Some(FoldingRangeKind::Comment),
            collapsed_text: None,
        });
    }

    ranges
}
