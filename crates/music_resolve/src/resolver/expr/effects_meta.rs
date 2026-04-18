use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_request_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let expr = self.lower_opt_expr(origin, node.child_nodes().next());
        self.alloc_expr(origin, HirExprKind::Request { expr })
    }

    pub(super) fn lower_handler_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let effect_tok = node
            .child_tokens()
            .find(|t| Self::is_ident_token_kind(t.kind()));
        let effect = self.intern_ident_token_or_placeholder(effect_tok, node.span());
        self.record_use(effect);

        let mut clauses = Vec::<HirHandleClause>::new();
        for clause in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::HandlerClause)
        {
            clauses.push(self.lower_handle_clause(clause));
        }
        let clauses = self.store.handle_clauses.alloc_from_iter(clauses);
        self.alloc_expr(origin, HirExprKind::HandlerLit { effect, clauses })
    }

    pub(super) fn lower_handle_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let expr = self.lower_opt_expr(origin, nodes.next());
        let handler = self.lower_opt_expr(origin, nodes.next());
        self.alloc_expr(origin, HirExprKind::Handle { expr, handler })
    }

    pub(super) fn lower_handle_clause(&mut self, node: SyntaxNode<'tree, 'src>) -> HirHandleClause {
        let op_tok = node
            .child_tokens()
            .find(|t| Self::is_ident_token_kind(t.kind()));
        let op = self.intern_ident_token_or_placeholder(op_tok, node.span());

        let mut params = Vec::<Ident>::new();
        for token in node
            .child_tokens()
            .filter(|t| Self::is_ident_token_kind(t.kind()))
            .skip(1)
        {
            if let Some(ident) = self.intern_ident_token(token) {
                params.push(ident);
            }
        }

        self.push_scope();
        if params.is_empty() && self.interner.resolve(op.name) == "value" {
            // `value => ...` has an implicit `value` binder in its body scope.
            let _ = self.insert_binding(op, NameBindingKind::HandleClauseParam);
        }
        for p in &params {
            let _ = self.insert_binding(*p, NameBindingKind::HandleClauseParam);
        }
        let body = match node.child_nodes().find(|n| n.kind().is_expr()) {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };
        self.pop_scope();

        let params = self.store.idents.alloc_from_iter(params);
        HirHandleClause::new(op, params, body)
    }

    pub(super) fn lower_resume_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let expr = node.child_nodes().next().map(|n| self.lower_expr(n));
        self.alloc_expr(origin, HirExprKind::Resume { expr })
    }

    pub(super) fn lower_quote_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let raw = self
            .tree
            .lexed()
            .slice_span(node.span())
            .unwrap_or_default()
            .into();
        if node.child_tokens().any(|t| t.kind() == TokenKind::LParen) {
            let expr = self.lower_opt_expr(origin, node.child_nodes().find(|n| n.kind().is_expr()));
            return self.alloc_expr(
                origin,
                HirExprKind::Quote {
                    kind: HirQuoteKind::Expr { expr, raw },
                },
            );
        }

        let mut exprs = Vec::<HirExprId>::new();
        for stmt in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::SequenceExpr)
        {
            if let Some(inner) = stmt_inner_expr(stmt) {
                exprs.push(self.lower_expr(inner));
            }
        }
        let exprs = self.store.alloc_expr_list(exprs);
        self.alloc_expr(
            origin,
            HirExprKind::Quote {
                kind: HirQuoteKind::Block { exprs, raw },
            },
        )
    }

    pub(super) fn lower_splice_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let raw = self
            .tree
            .lexed()
            .slice_span(node.span())
            .unwrap_or_default()
            .into();
        if let Some(tok) = node
            .child_tokens()
            .find(|t| Self::is_ident_token_kind(t.kind()))
        {
            let name = self.intern_ident_token_or_placeholder(Some(tok), tok.span());
            self.record_use(name);
            return self.alloc_expr(
                origin,
                HirExprKind::Splice {
                    kind: HirSpliceKind::Name { name, raw },
                },
            );
        }
        if let Some(expr) = node.child_nodes().find(|n| n.kind().is_expr()) {
            let expr = self.lower_expr(expr);
            return self.alloc_expr(
                origin,
                HirExprKind::Splice {
                    kind: HirSpliceKind::Expr { expr, raw },
                },
            );
        }
        let exprs: Vec<_> = node.child_nodes().map(|n| self.lower_expr(n)).collect();
        let exprs = self.store.alloc_expr_list(exprs);
        self.alloc_expr(
            origin,
            HirExprKind::Splice {
                kind: HirSpliceKind::Exprs { exprs, raw },
            },
        )
    }
}
