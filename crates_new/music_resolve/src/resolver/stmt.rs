use super::*;

use super::util::stmt_inner_expr;

impl Resolver<'_, '_, '_, '_> {
    pub(super) fn lower_source_file(&mut self) -> music_hir::HirExprId {
        let root = self.tree.root();
        if root.kind() != music_syntax::SyntaxNodeKind::SourceFile {
            return self.alloc_expr(self.origin_node(root), HirExprKind::Error);
        }

        let mut exprs = Vec::new();
        for child in root.child_nodes() {
            let Some(inner) = stmt_inner_expr(child) else {
                self.diags.push(Diag::error("invalid stmt").with_label(
                    child.span(),
                    self.source_id,
                    "expected `expr;`",
                ));
                continue;
            };
            exprs.push(self.lower_expr(inner));
        }
        let range = self.store.alloc_expr_list(exprs);
        self.alloc_expr(
            self.origin_node(root),
            HirExprKind::Sequence { exprs: range },
        )
    }
}
