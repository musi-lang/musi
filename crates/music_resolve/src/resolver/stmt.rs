use super::*;

use music_hir::HirExprId;
use music_syntax::SyntaxNodeKind;

use super::util::stmt_inner_expr;
use crate::diag::ResolveDiagKind;
use music_base::diag::DiagContext;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_source_file(&mut self) -> HirExprId {
        let root = self.tree.root();
        if root.kind() != SyntaxNodeKind::SourceFile {
            return self.alloc_expr(self.origin_node(root), HirExprKind::Error);
        }

        let mut exprs = Vec::new();
        for child in root.child_nodes() {
            let Some(inner) = stmt_inner_expr(child) else {
                self.diags.push(resolve_diag(
                    self.source_id,
                    child.span(),
                    ResolveDiagKind::InvalidStmt,
                    DiagContext::new(),
                ));
                continue;
            };
            let expr = self.lower_expr(inner);
            self.inject_anonymous_imports(expr);
            exprs.push(expr);
        }
        let range = self.store.alloc_expr_list(exprs);
        self.alloc_expr(
            self.origin_node(root),
            HirExprKind::Sequence { exprs: range },
        )
    }
    pub(super) fn inject_anonymous_imports(&mut self, expr: HirExprId) {
        if let HirExprKind::Import { arg } = self.store.exprs.get(expr).kind.clone() {
            let import_span = self.store.exprs.get(expr).origin.span;
            self.inject_import_arg_exports(arg, import_span);
        }
    }

    fn inject_import_arg_exports(&mut self, arg: HirExprId, import_span: Span) {
        match self.store.exprs.get(arg).kind.clone() {
            HirExprKind::Tuple { items } | HirExprKind::Sequence { exprs: items } => {
                for item in self.store.expr_ids.get(items).to_vec() {
                    self.inject_import_arg_exports(item, import_span);
                }
            }
            _ => self.inject_import_site_exports(arg, import_span),
        }
    }

    fn inject_import_site_exports(&mut self, arg: HirExprId, import_span: Span) {
        let span = self.store.exprs.get(arg).origin.span;
        let Some(target) = self
            .import_targets
            .get(&span)
            .or_else(|| self.import_targets.get(&import_span))
            .cloned()
        else {
            return;
        };
        let Some(summary) = self.import_export_summaries.get(&target).cloned() else {
            return;
        };
        for name in summary.exports() {
            let symbol = self.interner.intern(name);
            let _binding = self.insert_import_binding(symbol, span, target.clone());
        }
    }
}
