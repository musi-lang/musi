use music_ast::{SyntaxElement, SyntaxNode, SyntaxNodeKind};
use music_hir::{
    HirCallableName, HirExprKind, HirMemberDef, HirMemberDefs, HirOrigin, HirParam, HirParams,
};
use music_lex::TokenKind;
use music_names::{Ident, NameBindingKind, Symbol};

use super::{Resolver, cursor::AstCursor};

impl<'tree> Resolver<'_, 'tree, '_> {
    pub(super) fn lower_member_defs(&mut self, node: SyntaxNode<'tree>) -> HirMemberDefs {
        let defs: Vec<HirMemberDef> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Member)
            .map(|member| self.lower_member_def(member))
            .collect();
        defs.into_boxed_slice()
    }

    fn lower_member_def(&mut self, node: SyntaxNode<'tree>) -> HirMemberDef {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let head = cursor
            .bump_token()
            .map_or(TokenKind::KwLet, |t| t.kind().clone());

        match head {
            TokenKind::KwLet => self.lower_let_member(origin, cursor),
            TokenKind::KwLaw => self.lower_law_member(origin, cursor),
            _ => {
                self.error(origin.span, "expected member");
                HirMemberDef::Law {
                    origin,
                    attrs: Box::new([]),
                    name: Ident::dummy(Symbol::synthetic(u32::MAX)),
                    params: Box::new([]),
                    value: self.alloc_expr(origin, HirExprKind::Error),
                }
            }
        }
    }

    fn lower_let_member(
        &mut self,
        origin: HirOrigin,
        mut cursor: AstCursor<'tree>,
    ) -> HirMemberDef {
        let name = self.lower_callable_name(&mut cursor);
        let params_node = cursor.eat_node(SyntaxNodeKind::ParamList);

        self.push_scope();
        let params = match params_node {
            Some(list) => self.lower_param_list(list),
            None => Box::new([]),
        };

        let ret = if cursor.at_token(&TokenKind::Colon) {
            let _ = cursor.bump_token();
            cursor
                .bump_node()
                .filter(|n| n.kind().is_ty())
                .map(|ty| self.lower_ty(ty))
        } else {
            None
        };

        let value = if cursor.at_token(&TokenKind::ColonEq) {
            let _ = cursor.bump_token();
            cursor
                .bump_node()
                .filter(|n| n.kind().is_expr())
                .map(|expr| self.lower_expr(expr))
        } else {
            None
        };

        self.pop_scope();

        HirMemberDef::Let {
            origin,
            attrs: Box::new([]),
            name,
            params,
            ret,
            value,
        }
    }

    fn lower_law_member(
        &mut self,
        origin: HirOrigin,
        mut cursor: AstCursor<'tree>,
    ) -> HirMemberDef {
        let Some(name_tok) = cursor.bump_token() else {
            self.error(origin.span, "expected law name");
            return HirMemberDef::Law {
                origin,
                attrs: Box::new([]),
                name: Ident::dummy(Symbol::synthetic(u32::MAX)),
                params: Box::new([]),
                value: self.alloc_expr(origin, HirExprKind::Error),
            };
        };

        let name = self.intern_ident_token(name_tok);
        let params_node = cursor.eat_node(SyntaxNodeKind::ParamList);

        self.push_scope();
        let params = match params_node {
            Some(list) => self.lower_param_list(list),
            None => Box::new([]),
        };

        // Skip ':=' token.
        let _ = cursor.eat_token(&TokenKind::ColonEq);

        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let value = cursor
            .bump_node()
            .filter(|n| n.kind().is_expr())
            .map_or(error_expr, |expr| self.lower_expr(expr));

        self.pop_scope();

        HirMemberDef::Law {
            origin,
            attrs: Box::new([]),
            name,
            params,
            value,
        }
    }

    fn lower_callable_name(&mut self, cursor: &mut AstCursor<'tree>) -> HirCallableName {
        if let Some(tok) = cursor
            .peek()
            .and_then(SyntaxElement::into_token)
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
        {
            let _ = cursor.bump_token();
            return HirCallableName {
                name: self.intern_ident_token(tok),
            };
        }

        // Operator form: '(' op ')'
        let _ = cursor.eat_token(&TokenKind::LParen);
        let op_tok = cursor.bump_token();
        let _ = cursor.eat_token(&TokenKind::RParen);

        let name = op_tok.map_or_else(
            || Ident::dummy(Symbol::synthetic(u32::MAX)),
            |t| self.intern_op_token(t),
        );

        HirCallableName { name }
    }

    pub(super) fn lower_param_list(&mut self, node: SyntaxNode<'tree>) -> HirParams {
        let mut params = Vec::new();
        for param_node in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Param)
        {
            params.push(self.lower_param(param_node));
        }
        params.into_boxed_slice()
    }

    fn lower_param(&mut self, node: SyntaxNode<'tree>) -> HirParam {
        let origin = Self::origin_node(node);
        let mutable = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::KwMut));

        let Some(name_tok) = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
        else {
            self.error(node.span(), "expected parameter name");
            return HirParam {
                origin,
                mutable,
                name: Ident::dummy(Symbol::synthetic(u32::MAX)),
                annot: None,
                default: None,
            };
        };

        let name = self.intern_ident_token(name_tok);
        self.define(NameBindingKind::Param, name);

        let annot = node
            .child_nodes()
            .find(|n| n.kind().is_ty())
            .map(|ty| self.lower_ty(ty));

        let default = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::ColonEq))
            .then(|| {
                node.child_nodes()
                    .find(|n| n.kind().is_expr())
                    .map(|expr| self.lower_expr(expr))
            })
            .flatten();

        HirParam {
            origin,
            mutable,
            name,
            annot,
            default,
        }
    }
}
