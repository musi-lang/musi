use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_sequence_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let exprs: Vec<_> = node.child_nodes().map(|n| self.lower_expr(n)).collect();
        let exprs = self.store.alloc_expr_list(exprs);
        self.alloc_expr(origin, HirExprKind::Sequence { exprs })
    }

    pub(super) fn lower_name_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let Some(tok) = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::OpIdent))
        else {
            self.diags.push(Diag::error("expected name").with_label(
                node.span(),
                self.source_id,
                "identifier missing",
            ));
            return self.error_expr(origin);
        };
        let Some(ident) = self.intern_ident_token(tok) else {
            return self.error_expr(origin);
        };
        self.record_use(ident);
        self.alloc_expr(origin, HirExprKind::Name { name: ident })
    }

    pub(super) fn lower_literal_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let Some(tok) = node.child_tokens().next() else {
            return self.error_expr(origin);
        };
        let Some(lit) = self.alloc_lit_from_token(tok) else {
            return self.error_expr(origin);
        };
        self.alloc_expr(origin, HirExprKind::Lit { lit })
    }

    pub(super) fn lower_template_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut parts = Vec::<HirTemplatePart>::new();
        for child in node.children() {
            match child {
                SyntaxElement::Token(tok) => {
                    let Some(raw) = tok.text() else {
                        continue;
                    };
                    let decoded = match tok.kind() {
                        TokenKind::TemplateNoSubst => decode_template_no_subst(raw),
                        TokenKind::TemplateHead => decode_template_head(raw),
                        TokenKind::TemplateMiddle => decode_template_middle(raw),
                        TokenKind::TemplateTail => decode_template_tail(raw),
                        _ => continue,
                    };
                    if let Ok(text) = decoded {
                        parts.push(HirTemplatePart::Text { value: text.into() });
                    }
                }
                SyntaxElement::Node(expr) => {
                    let hir = self.lower_expr(expr);
                    parts.push(HirTemplatePart::Expr { expr: hir });
                }
            }
        }
        let parts = self.store.template_parts.alloc_from_iter(parts);
        self.alloc_expr(origin, HirExprKind::Template { parts })
    }
}

