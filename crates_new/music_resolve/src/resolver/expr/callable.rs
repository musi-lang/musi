use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_pi_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let binder_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let binder = self.intern_ident_token_or_placeholder(binder_tok, node.span());

        self.push_scope();
        let _ = self.insert_binding(binder, NameBindingKind::PiBinder);

        let is_effectful = node.child_tokens().any(|t| t.kind() == TokenKind::TildeGt);
        let mut exprs = node.child_nodes();
        let binder_ty = self.lower_opt_expr(origin, exprs.next());
        let ret = self.lower_opt_expr(origin, exprs.next());

        self.pop_scope();
        self.alloc_expr(
            origin,
            HirExprKind::Pi {
                binder,
                binder_ty,
                ret,
                is_effectful,
            },
        )
    }

    pub(super) fn lower_lambda_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let param_list = node
            .child_nodes()
            .find(|child| child.kind() == SyntaxNodeKind::ParamList);
        self.push_scope();
        let params = param_list.map_or(SliceRange::EMPTY, |list| self.lower_param_list(list));

        let mut exprs = node
            .child_nodes()
            .filter(|child| child.kind() != SyntaxNodeKind::ParamList);
        let ret_ty = self.lower_optional_expr_clause(node, TokenKind::Colon, &mut exprs);
        let body = match exprs.next() {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(origin),
        };
        self.pop_scope();

        self.alloc_expr(
            origin,
            HirExprKind::Lambda {
                params,
                ret_ty,
                body,
            },
        )
    }

    pub(super) fn lower_call_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let callee = self.lower_opt_expr(origin, nodes.next());

        let mut args = Vec::<HirArg>::new();
        for arg_node in nodes.filter(|n| n.kind() == SyntaxNodeKind::Arg) {
            let spread = arg_node
                .child_tokens()
                .any(|t| t.kind() == TokenKind::DotDotDot);
            let expr = self.lower_opt_expr(origin, arg_node.child_nodes().next());
            args.push(HirArg { spread, expr });
        }
        let args = self.store.args.alloc_from_iter(args);
        self.alloc_expr(origin, HirExprKind::Call { callee, args })
    }

    pub(super) fn lower_apply_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        self.lower_apply_like_expr(node, |callee, args| HirExprKind::Apply { callee, args })
    }

    pub(super) fn lower_index_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        self.lower_apply_like_expr(node, |base, args| HirExprKind::Index { base, args })
    }

    pub(super) fn lower_record_update_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());
        let items = self.lower_record_items(nodes);
        let items = self.store.record_items.alloc_from_iter(items);
        self.alloc_expr(origin, HirExprKind::RecordUpdate { base, items })
    }

    fn lower_apply_like_expr<F>(&mut self, node: SyntaxNode<'tree, 'src>, build: F) -> HirExprId
    where
        F: FnOnce(HirExprId, SliceRange<HirExprId>) -> HirExprKind,
    {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());
        let args: Vec<_> = nodes.map(|child| self.lower_expr(child)).collect();
        let args = self.store.expr_ids.alloc_from_iter(args);
        self.alloc_expr(origin, build(base, args))
    }

    pub(super) fn lower_field_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());

        let access = HirAccessKind::Direct;

        let name_tok = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::Int));
        let name = name_tok
            .and_then(|tok| {
                tok.text()
                    .map(|raw| self.intern_ident_text(tok.kind(), raw, tok.span()))
            })
            .unwrap_or_else(|| self.placeholder_ident(node.span()));

        self.alloc_expr(origin, HirExprKind::Field { base, access, name })
    }

    pub(super) fn lower_type_test_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());
        let ty = self.lower_opt_expr(origin, nodes.next());

        let as_name = node
            .child_tokens()
            .find(|t| t.kind() == TokenKind::Ident)
            .and_then(|t| self.intern_ident_token(t));
        self.alloc_expr(origin, HirExprKind::TypeTest { base, ty, as_name })
    }

    pub(super) fn lower_type_cast_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());
        let ty = self.lower_opt_expr(origin, nodes.next());
        self.alloc_expr(origin, HirExprKind::TypeCast { base, ty })
    }
}
