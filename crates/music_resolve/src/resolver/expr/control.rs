use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_case_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let scrutinee = self.lower_opt_expr(origin, node.child_nodes().next());

        let mut arms = Vec::<HirCaseArm>::new();
        for arm in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::CaseArm)
        {
            arms.push(self.lower_case_arm(arm));
        }
        let arms = self.store.case_arms.alloc_from_iter(arms);
        self.alloc_expr(origin, HirExprKind::Case { scrutinee, arms })
    }

    pub(super) fn lower_case_arm(&mut self, node: SyntaxNode<'tree, 'src>) -> HirCaseArm {
        self.push_scope();

        let attrs = self.lower_attrs(node);
        let pat_node = node.child_nodes().find(|n| n.kind().is_pat());
        let pat_node = pat_node.unwrap_or(node);
        let binders = if pat_node.kind().is_pat() {
            self.collect_pat_binders(pat_node)
        } else {
            Vec::new()
        };
        for b in binders {
            let _ = self.insert_binding(b, NameBindingKind::PatternBind);
        }
        let pat = if pat_node.kind().is_pat() {
            self.lower_pat(pat_node)
        } else {
            self.store
                .alloc_pat(HirPat::new(self.origin_node(node), HirPatKind::Error))
        };

        let mut exprs = node.child_nodes().filter(|child| child.kind().is_expr());
        let guard = self.lower_optional_expr_clause(node, TokenKind::KwIf, &mut exprs);
        let expr = match exprs.next() {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };

        self.pop_scope();
        HirCaseArm::new(attrs, pat, guard, expr)
    }
}
