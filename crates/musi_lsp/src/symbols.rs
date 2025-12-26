use lsp_types::{DocumentSymbol, SymbolKind};
use musi_ast::{AstArena, ExprKind, FnSig, PatKind, Prog, StmtKind, SumCase};
use musi_basic::{interner::Interner, source::SourceFile, span::Span};

use crate::types::DocumentSymbolList;

pub fn collect_symbols(
    source: &SourceFile,
    prog: &Prog,
    arena: &AstArena,
    interner: &Interner,
) -> DocumentSymbolList {
    let mut collector = SymbolCollector {
        source,
        arena,
        interner,
        symbols: Vec::new(),
    };
    collector.visit_prog(prog);
    collector.symbols
}

struct SymbolCollector<'a> {
    source: &'a SourceFile,
    arena: &'a AstArena,
    interner: &'a Interner,
    symbols: DocumentSymbolList,
}

impl SymbolCollector<'_> {
    fn span_to_range(&self, span: Span) -> lsp_types::Range {
        let (start_line, start_col) = self.source.location_at(span.lo);
        let (end_line, end_col) = self.source.location_at(span.hi);
        lsp_types::Range {
            start: lsp_types::Position {
                line: u32::try_from(start_line.saturating_sub(1)).unwrap_or(0),
                character: u32::try_from(start_col.saturating_sub(1)).unwrap_or(0),
            },
            end: lsp_types::Position {
                line: u32::try_from(end_line.saturating_sub(1)).unwrap_or(0),
                character: u32::try_from(end_col.saturating_sub(1)).unwrap_or(0),
            },
        }
    }

    fn resolve_name(&self, id: u32) -> String {
        self.interner.lookup(id).unwrap_or("<unknown>").to_owned()
    }

    fn make_symbol(
        &self,
        name: String,
        kind: SymbolKind,
        span: Span,
        children: Option<DocumentSymbolList>,
    ) -> DocumentSymbol {
        let range = self.span_to_range(span);
        DocumentSymbol {
            name,
            detail: None,
            kind,
            tags: None,
            #[allow(deprecated)]
            deprecated: None,
            range,
            selection_range: range,
            children,
        }
    }

    fn collect_fn(&mut self, sig: &FnSig, span: Span) {
        if let Some(name_id) = sig.name {
            let name = self.resolve_name(name_id);
            let sym = self.make_symbol(name, SymbolKind::FUNCTION, span, None);
            self.symbols.push(sym);
        }
    }

    fn collect_sum_cases(&self, cases: &[SumCase], parent_span: Span) -> DocumentSymbolList {
        cases
            .iter()
            .map(|c| {
                let name = self.resolve_name(c.name);
                self.make_symbol(name, SymbolKind::ENUM_MEMBER, parent_span, None)
            })
            .collect()
    }

    fn visit_prog(&mut self, prog: &Prog) {
        for &stmt_id in &prog.stmts {
            let stmt = self.arena.stmts.get(stmt_id);
            let StmtKind::Expr(expr_id) = stmt.kind;
            self.visit_expr_id(expr_id);
        }
    }

    fn visit_expr_id(&mut self, expr_id: musi_ast::ExprId) {
        let expr = self.arena.exprs.get(expr_id);
        match &expr.kind {
            ExprKind::Fn { sig, .. } => {
                self.collect_fn(sig, expr.span);
            }
            ExprKind::Bind { pat, .. } => {
                let pat_node = self.arena.pats.get(*pat);
                if let PatKind::Ident(ident_id) = &pat_node.kind {
                    let name = self.resolve_name(*ident_id);
                    let sym = self.make_symbol(name, SymbolKind::VARIABLE, pat_node.span, None);
                    self.symbols.push(sym);
                }
            }
            ExprKind::RecordDef { name: Some(n), .. } => {
                let name_str = self.resolve_name(*n);
                let sym = self.make_symbol(name_str, SymbolKind::STRUCT, expr.span, None);
                self.symbols.push(sym);
            }
            ExprKind::SumDef {
                name: Some(n),
                cases,
                ..
            } => {
                let name_str = self.resolve_name(*n);
                let children = self.collect_sum_cases(cases, expr.span);
                let sym = self.make_symbol(name_str, SymbolKind::ENUM, expr.span, Some(children));
                self.symbols.push(sym);
            }
            ExprKind::Alias { name, .. } => {
                let name_str = self.resolve_name(*name);
                let sym = self.make_symbol(name_str, SymbolKind::TYPE_PARAMETER, expr.span, None);
                self.symbols.push(sym);
            }
            ExprKind::Block {
                stmts,
                expr: opt_expr,
            } => {
                for &stmt_id in stmts {
                    let stmt = self.arena.stmts.get(stmt_id);
                    let StmtKind::Expr(inner_expr_id) = stmt.kind;
                    self.visit_expr_id(inner_expr_id);
                }
                if let Some(inner) = opt_expr {
                    self.visit_expr_id(*inner);
                }
            }
            _ => {}
        }
    }
}
