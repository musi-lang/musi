use lsp_types::{FoldingRange, FoldingRangeKind};
use musi_ast::{
    Expr, ExprKind, Program, Stmt, StmtKind,
    visitor::{Visitor, walk_expr},
};
use musi_basic::{source::SourceFile, span::Span};

use crate::types::FoldingRangeList;

pub fn collect_folding_ranges(source: &SourceFile, program: &Program) -> FoldingRangeList {
    let mut collector = FoldingCollector {
        source,
        ranges: vec![],
    };
    collector.visit_program(program);
    collector.ranges
}

struct FoldingCollector<'a> {
    source: &'a SourceFile,
    ranges: FoldingRangeList,
}

impl FoldingCollector<'_> {
    fn add_range(&mut self, span: Span, kind: Option<FoldingRangeKind>) {
        let (start_line, _) = self.source.location_at(span.lo);
        let (end_line, _) = self.source.location_at(span.hi);
        if end_line > start_line {
            self.ranges.push(FoldingRange {
                start_line: u32::try_from(start_line.saturating_sub(1)).unwrap_or(0),
                start_character: None,
                end_line: u32::try_from(end_line.saturating_sub(1)).unwrap_or(0),
                end_character: None,
                kind,
                collapsed_text: None,
            });
        }
    }
}

impl Visitor for FoldingCollector<'_> {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        let StmtKind::Expr(expr) = &stmt.kind;
        self.visit_expr(expr);
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match &expr.kind {
            ExprKind::Block { .. }
            | ExprKind::Fn { .. }
            | ExprKind::RecordDef { .. }
            | ExprKind::SumDef { .. }
            | ExprKind::Match { .. }
            | ExprKind::If { .. }
            | ExprKind::While { .. }
            | ExprKind::For { .. } => {
                self.add_range(expr.span, Some(FoldingRangeKind::Region));
            }
            _ => {}
        }
        walk_expr(self, expr);
    }
}
