use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_tuple_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let items: Vec<_> = node.child_nodes().map(|n| self.lower_expr(n)).collect();
        let items = self.store.alloc_expr_list(items);
        self.alloc_expr(origin, HirExprKind::Tuple { items })
    }

    pub(super) fn lower_array_expr_or_ty(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        if node
            .child_nodes()
            .any(|n| n.kind() == SyntaxNodeKind::ArrayItem)
        {
            return self.lower_array_expr(node);
        }
        self.lower_array_ty_expr(node)
    }

    pub(super) fn lower_array_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut items = Vec::<HirArrayItem>::new();
        for item in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::ArrayItem)
        {
            let spread = item
                .child_tokens()
                .any(|t| t.kind() == TokenKind::DotDotDot);
            let expr = self.lower_opt_expr(origin, item.child_nodes().next());
            items.push(HirArrayItem { spread, expr });
        }
        let items = self.store.array_items.alloc_from_iter(items);
        self.alloc_expr(origin, HirExprKind::Array { items })
    }

    pub(super) fn lower_array_ty_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut dims = Vec::<HirDim>::new();
        let mut item_expr = None::<HirExprId>;
        for child in node.children() {
            match child {
                SyntaxElement::Token(tok) => match tok.kind() {
                    TokenKind::Underscore => dims.push(HirDim::Unknown),
                    TokenKind::Int => {
                        if let Some(raw) = tok.text() {
                            if let Some(v) = parse_u32_lit(raw) {
                                dims.push(HirDim::Int(v));
                            } else {
                                dims.push(HirDim::Unknown);
                            }
                        }
                    }
                    TokenKind::Ident => {
                        if let Some(raw) = tok.text() {
                            let ident = self.intern_ident_text(tok.kind(), raw, tok.span());
                            self.record_use(ident);
                            dims.push(HirDim::Name(ident));
                        }
                    }
                    _ => {}
                },
                SyntaxElement::Node(expr) => {
                    item_expr = Some(self.lower_expr(expr));
                }
            }
        }
        let item = item_expr.unwrap_or_else(|| self.error_expr(origin));
        let dims = self.store.dims.alloc_from_iter(dims);
        self.alloc_expr(origin, HirExprKind::ArrayTy { dims, item })
    }

    pub(super) fn lower_record_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let items = self.lower_record_items(node.child_nodes());
        let items = self.store.record_items.alloc_from_iter(items);
        self.alloc_expr(origin, HirExprKind::Record { items })
    }

    pub(super) fn lower_record_item(&mut self, node: SyntaxNode<'tree, 'src>) -> HirRecordItem {
        let origin = self.origin_node(node);
        if node
            .child_tokens()
            .any(|t| t.kind() == TokenKind::DotDotDot)
        {
            let value = self.lower_opt_expr(origin, node.child_nodes().next());
            return HirRecordItem {
                spread: true,
                name: None,
                value,
            };
        }

        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());

        let has_bind = node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq);
        let value = if has_bind {
            self.lower_opt_expr(origin, node.child_nodes().next())
        } else {
            self.record_use(name);
            let name_origin = name_tok.map_or(origin, |tok| self.origin_token(tok));
            self.alloc_expr(name_origin, HirExprKind::Name { name })
        };

        HirRecordItem {
            spread: false,
            name: Some(name),
            value,
        }
    }

    pub(super) fn lower_variant_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let tag_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let tag = self.intern_ident_token_or_placeholder(tag_tok, node.span());
        let args: Vec<_> = node.child_nodes().map(|n| self.lower_expr(n)).collect();
        let args = self.store.alloc_expr_list(args);
        self.alloc_expr(origin, HirExprKind::Variant { tag, args })
    }

    pub(super) fn lower_record_items<I>(&mut self, nodes: I) -> Vec<HirRecordItem>
    where
        I: Iterator<Item = SyntaxNode<'tree, 'src>>,
    {
        nodes
            .filter(|node| node.kind() == SyntaxNodeKind::RecordItem)
            .map(|node| self.lower_record_item(node))
            .collect()
    }
}
