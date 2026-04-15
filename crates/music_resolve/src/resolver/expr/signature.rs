use super::*;
use crate::resolver::util::is_expr_or_ty;
use music_hir::HirBinder;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(in crate::resolver) fn lower_type_param_list(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirBinder> {
        let params: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::TypeParam)
            .filter_map(|n| {
                let name = n
                    .child_tokens()
                    .find(|t| t.kind() == TokenKind::Ident)
                    .and_then(|t| self.intern_ident_token(t))?;
                let mut exprs = n.child_nodes().filter(|child| is_expr_or_ty(child.kind()));
                let ty = self.lower_optional_expr_clause(n, TokenKind::Colon, &mut exprs);
                Some(HirBinder::new(name, ty))
            })
            .collect();
        for p in &params {
            let _ = self.insert_binding(p.name, NameBindingKind::TypeParam);
        }
        self.store.binders.alloc_from_iter(params)
    }

    pub(in crate::resolver) fn lower_param_list(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirParam> {
        let params: Vec<_> = node
            .child_nodes()
            .filter(|child| child.kind() == SyntaxNodeKind::Param)
            .map(|child| self.lower_param(child))
            .collect();
        self.store.params.alloc_from_iter(params)
    }

    fn lower_param(&mut self, node: SyntaxNode<'tree, 'src>) -> HirParam {
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());
        let _ = self.insert_binding(name, NameBindingKind::Param);

        let mut exprs = node
            .child_nodes()
            .filter(|child| is_expr_or_ty(child.kind()));
        let ty = self.lower_optional_expr_clause(node, TokenKind::Colon, &mut exprs);
        let default = self.lower_optional_expr_clause(node, TokenKind::ColonEq, &mut exprs);

        HirParam::new(name, ty, default)
    }

    pub(in crate::resolver) fn lower_constraint_list(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirConstraint> {
        let constraints: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Constraint)
            .map(|n| self.lower_constraint(n))
            .collect();
        self.store.constraints.alloc_from_iter(constraints)
    }

    fn lower_constraint(&mut self, node: SyntaxNode<'tree, 'src>) -> HirConstraint {
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());
        self.record_use(name);

        let kind = if node.child_tokens().any(|t| t.kind() == TokenKind::LtColon) {
            HirConstraintKind::Subtype
        } else if node.child_tokens().any(|t| t.kind() == TokenKind::TildeEq) {
            HirConstraintKind::TypeEq
        } else {
            HirConstraintKind::Implements
        };
        let value = match node.child_nodes().find(|n| is_expr_or_ty(n.kind())) {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };
        HirConstraint::new(name, kind, value)
    }

    pub(super) fn lower_effect_set(&mut self, node: SyntaxNode<'tree, 'src>) -> HirEffectSet {
        let mut items = Vec::<HirEffectItem>::new();
        let mut open = None::<Ident>;
        let mut saw_dots = false;
        for child in node.children() {
            match child {
                SyntaxElement::Token(tok) => match tok.kind() {
                    TokenKind::DotDotDot => saw_dots = true,
                    TokenKind::Ident if saw_dots => {
                        if let Some(id) = self.intern_ident_token(tok) {
                            self.record_use(id);
                            open = Some(id);
                        }
                        saw_dots = false;
                    }
                    _ => {}
                },
                SyntaxElement::Node(n) if n.kind() == SyntaxNodeKind::EffectItem => {
                    items.push(self.lower_effect_item(n));
                }
                SyntaxElement::Node(_) => {}
            }
        }
        let items = self.store.effect_items.alloc_from_iter(items);
        HirEffectSet::new(items, open)
    }

    fn lower_effect_item(&mut self, node: SyntaxNode<'tree, 'src>) -> HirEffectItem {
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());
        self.record_use(name);

        let arg = node
            .child_nodes()
            .find(|n| is_expr_or_ty(n.kind()))
            .map(|n| self.lower_expr(n));
        HirEffectItem::new(name, arg)
    }
}
