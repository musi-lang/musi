use lsp_types::{FoldingRange, FoldingRangeKind};
use musi_ast::{AstArena, ExprId, ExprKind, Prog, StmtKind};
use musi_basic::{source::SourceFile, span::Span};

use crate::types::FoldingRangeList;

pub fn collect_folding_ranges(
    source: &SourceFile,
    prog: &Prog,
    arena: &AstArena,
) -> FoldingRangeList {
    let mut collector = FoldingCollector {
        source,
        arena,
        ranges: vec![],
    };
    collector.visit_prog(prog);
    collector.ranges
}

struct FoldingCollector<'a> {
    source: &'a SourceFile,
    arena: &'a AstArena,
    ranges: FoldingRangeList,
}

impl FoldingCollector<'_> {
    fn add_range(&mut self, span: Span, kind: Option<FoldingRangeKind>) {
        let (start_line, _) = self.source.location_at(span.lo);
        let (end_line, _) = self.source.location_at(span.hi);
        if end_line > start_line {
            self.ranges.push(FoldingRange {
                start_line: u32::try_from(start_line - 1).unwrap_or(0),
                start_character: None,
                end_line: u32::try_from(end_line - 1).unwrap_or(0),
                end_character: None,
                kind,
                collapsed_text: None,
            });
        }
    }

    fn visit_prog(&mut self, prog: &Prog) {
        for &stmt_id in &prog.stmts {
            let stmt = self.arena.stmts.get(stmt_id);
            let StmtKind::Expr(expr_id) = stmt.kind;
            self.visit_expr_id(expr_id);
        }
    }

    fn visit_expr_id(&mut self, expr_id: ExprId) {
        let expr = self.arena.exprs.get(expr_id);
        match &expr.kind {
            ExprKind::Block {
                stmts,
                expr: opt_expr,
                ..
            } => {
                self.add_range(expr.span, Some(FoldingRangeKind::Region));
                for &stmt_id in stmts {
                    let stmt = self.arena.stmts.get(stmt_id);
                    let StmtKind::Expr(inner_id) = stmt.kind;
                    self.visit_expr_id(inner_id);
                }
                if let Some(inner_id) = opt_expr {
                    self.visit_expr_id(*inner_id);
                }
            }
            ExprKind::Fn { body, .. } => {
                self.add_range(expr.span, Some(FoldingRangeKind::Region));
                self.visit_expr_id(*body);
            }
            ExprKind::RecordDef { .. } | ExprKind::SumDef { .. } => {
                self.add_range(expr.span, Some(FoldingRangeKind::Region));
            }
            ExprKind::Match { scrutinee, cases } => {
                self.add_range(expr.span, Some(FoldingRangeKind::Region));
                self.visit_expr_id(*scrutinee);
                for case in cases {
                    self.visit_expr_id(case.body);
                }
            }
            ExprKind::If {
                cond,
                then_br,
                else_br,
            } => {
                self.add_range(expr.span, Some(FoldingRangeKind::Region));
                let cond_node = self.arena.conds.get(*cond);
                if let musi_ast::CondKind::Expr(inner) = &cond_node.kind {
                    self.visit_expr_id(*inner);
                }
                self.visit_expr_id(*then_br);
                if let Some(else_id) = else_br {
                    self.visit_expr_id(*else_id);
                }
            }
            ExprKind::While { cond, body } => {
                self.add_range(expr.span, Some(FoldingRangeKind::Region));
                let cond_node = self.arena.conds.get(*cond);
                if let musi_ast::CondKind::Expr(inner) = &cond_node.kind {
                    self.visit_expr_id(*inner);
                }
                self.visit_expr_id(*body);
            }
            ExprKind::For { iter, body, .. } => {
                self.add_range(expr.span, Some(FoldingRangeKind::Region));
                self.visit_expr_id(*iter);
                self.visit_expr_id(*body);
            }
            _ => {}
        }
    }
}
