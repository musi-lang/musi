use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_attributed_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);
        let expr = self.lower_opt_expr(origin, node.child_nodes().find(|n| n.kind().is_expr()));
        self.alloc_expr(origin, HirExprKind::Attributed { attrs, expr })
    }

    pub(super) fn lower_attrs(&mut self, node: SyntaxNode<'tree, 'src>) -> SliceRange<HirAttr> {
        let attrs: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Attr)
            .map(|n| self.lower_attr(n))
            .collect();
        self.store.attrs.alloc_from_iter(attrs)
    }

    pub(super) fn lower_attr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirAttr {
        let origin = self.origin_node(node);
        let path: Vec<_> = node
            .child_tokens()
            .filter(|t| t.kind() == TokenKind::Ident)
            .filter_map(|t| self.intern_ident_token(t))
            .collect();
        let path = self.store.idents.alloc_from_iter(path);

        let args: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::AttrArg)
            .map(|n| self.lower_attr_arg(n))
            .collect();
        let args = self.store.attr_args.alloc_from_iter(args);

        HirAttr { origin, path, args }
    }

    pub(super) fn lower_attr_arg(&mut self, node: SyntaxNode<'tree, 'src>) -> HirAttrArg {
        let name = if node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
            node.child_tokens()
                .find(|t| t.kind() == TokenKind::Ident)
                .and_then(|t| self.intern_ident_token(t))
        } else {
            None
        };
        let value = match node.child_nodes().find(|n| n.kind().is_expr()) {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };
        HirAttrArg { name, value }
    }
}

