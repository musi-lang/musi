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
            items.push(HirArrayItem::new(spread, expr));
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

    pub(super) fn lower_handler_ty_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut exprs = node.child_nodes();
        let effect = self.lower_opt_expr(origin, exprs.next());
        let input = self.lower_opt_expr(origin, exprs.next());
        let output = self.lower_opt_expr(origin, exprs.next());
        self.alloc_expr(
            origin,
            HirExprKind::HandlerTy {
                effect,
                input,
                output,
            },
        )
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
            return HirRecordItem::new(true, None, value);
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

        HirRecordItem::new(false, Some(name), value)
    }

    pub(super) fn lower_variant_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let tag_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let tag = self.intern_ident_token_or_placeholder(tag_tok, node.span());
        let args = node
            .child_nodes()
            .find(|child| child.kind() == SyntaxNodeKind::VariantPayloadList)
            .map_or_else(Vec::new, |list| self.lower_variant_args(list));
        let args = self.store.args.alloc_from_iter(args);
        self.alloc_expr(origin, HirExprKind::Variant { tag, args })
    }

    fn lower_variant_args(&mut self, node: SyntaxNode<'tree, 'src>) -> Vec<HirArg> {
        let mut out = Vec::new();
        for child in node
            .child_nodes()
            .filter(|inner| inner.kind() == SyntaxNodeKind::VariantArg)
        {
            let name_tok = child.child_tokens().find(|t| t.kind() == TokenKind::Ident);
            let name = if child.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
                name_tok.and_then(|tok| self.intern_ident_token(tok))
            } else {
                None
            };
            let expr = self.lower_opt_expr(self.origin_node(child), child.child_nodes().next());
            out.push(HirArg::new(false, name, expr));
        }
        out
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
