use super::*;
use crate::diag::ResolveDiagKind;

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
            self.diags.push(
                Diag::error(ResolveDiagKind::ExpectedName.message())
                    .with_code(ResolveDiagKind::ExpectedName.code())
                    .with_label(
                        node.span(),
                        self.source_id,
                        ResolveDiagKind::ExpectedName.label(),
                    ),
            );
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
