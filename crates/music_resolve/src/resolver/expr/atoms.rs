use super::*;
use crate::diag::ResolveDiagKind;
use music_base::diag::DiagContext;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_sequence_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let exprs: Vec<_> = node
            .child_nodes()
            .map(|n| {
                let expr = self.lower_expr(n);
                self.inject_anonymous_imports(expr);
                expr
            })
            .collect();
        let exprs = self.store.alloc_expr_list(exprs);
        self.alloc_expr(origin, HirExprKind::Sequence { exprs })
    }

    pub(super) fn lower_unsafe_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let exprs = node
            .child_nodes()
            .map(|child| {
                let expr = self.lower_sequence_or_stmt_expr(child);
                self.inject_anonymous_imports(expr);
                expr
            })
            .collect::<Vec<_>>();
        let exprs = self.store.alloc_expr_list(exprs);
        let body = self.alloc_expr(origin, HirExprKind::Sequence { exprs });
        self.alloc_expr(origin, HirExprKind::Unsafe { body })
    }

    pub(super) fn lower_pin_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut child_nodes = node.child_nodes();
        let Some(value_node) = child_nodes.next() else {
            return self.error_expr(origin);
        };
        let Some(body_node) = child_nodes.next() else {
            return self.error_expr(origin);
        };
        let Some(name_tok) = node
            .child_tokens()
            .find(|tok| tok.kind() == TokenKind::Ident)
        else {
            return self.error_expr(origin);
        };
        let Some(name) = self.intern_ident_token(name_tok) else {
            return self.error_expr(origin);
        };

        let pinned_expr = self.lower_expr(value_node);
        self.push_scope();
        let _binding = self.insert_binding(name, NameBindingKind::Pin);
        let body = self.lower_expr(body_node);
        self.pop_scope();

        self.alloc_expr(
            origin,
            HirExprKind::Pin {
                value: pinned_expr,
                name,
                body,
            },
        )
    }

    pub(super) fn lower_name_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let Some(tok) = node
            .child_tokens()
            .find(|t| Self::is_name_token_kind(t.kind()))
        else {
            self.diags.push(resolve_diag(
                self.source_id,
                node.span(),
                ResolveDiagKind::ExpectedName,
                DiagContext::new(),
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
        let mut pieces = Vec::<HirExprId>::new();
        for child in node.children() {
            match child {
                SyntaxElement::Token(tok)
                    if matches!(
                        tok.kind(),
                        TokenKind::TemplateNoSubst
                            | TokenKind::TemplateHead
                            | TokenKind::TemplateMiddle
                            | TokenKind::TemplateTail
                    ) =>
                {
                    let Some(lit) = self.alloc_lit_from_token(tok) else {
                        continue;
                    };
                    let lit_origin = self.origin_token(tok);
                    pieces.push(self.alloc_expr(lit_origin, HirExprKind::Lit { lit }));
                }
                SyntaxElement::Node(expr) => {
                    pieces.push(self.lower_expr(expr));
                }
                SyntaxElement::Token(_) => {}
            }
        }

        let mut iter = pieces.into_iter();
        let Some(first) = iter.next() else {
            return self.error_expr(origin);
        };
        iter.fold(first, |left, right| {
            self.alloc_expr(
                origin,
                HirExprKind::Binary {
                    op: HirBinaryOp::Add,
                    left,
                    right,
                },
            )
        })
    }
}
