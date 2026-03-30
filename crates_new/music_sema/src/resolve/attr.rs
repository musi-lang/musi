use music_ast::{SyntaxElement, SyntaxNode, SyntaxNodeKind};
use music_hir::{HirAttr, HirAttrArg, HirAttrArgKind, HirAttrId, HirAttrPath};

use super::{Resolver, cursor::AstCursor};

impl<'a, 'tree> Resolver<'a, 'tree> {
    pub(super) fn lower_attr_prefix(&mut self, cursor: &mut AstCursor<'tree>) -> Vec<HirAttrId> {
        let mut out = Vec::new();
        while let Some(node) = cursor.peek().and_then(SyntaxElement::into_node) {
            if node.kind() != SyntaxNodeKind::Attr {
                break;
            }
            let _ = cursor.bump();
            out.push(self.lower_attr(node));
        }
        out
    }

    pub(super) fn lower_attr(&mut self, node: SyntaxNode<'tree>) -> HirAttrId {
        let mut segments = Vec::new();
        for token in node.child_tokens() {
            if matches!(token.kind(), music_lex::TokenKind::Ident | music_lex::TokenKind::EscapedIdent)
            {
                segments.push(self.intern_ident_token(token));
            }
        }

        let mut args = Vec::new();
        for arg_node in node.child_nodes() {
            if arg_node.kind() != SyntaxNodeKind::AttrArg {
                continue;
            }
            args.push(self.lower_attr_arg(arg_node));
        }

        self.store.attrs.alloc(HirAttr {
            origin: self.origin_node(node),
            path: HirAttrPath {
                segments: segments.into_boxed_slice(),
            },
            args: args.into_boxed_slice(),
        })
    }

    fn lower_attr_arg(&mut self, node: SyntaxNode<'tree>) -> HirAttrArg {
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);
        let first_token = cursor
            .peek()
            .and_then(SyntaxElement::into_token)
            .filter(|tok| matches!(tok.kind(), music_lex::TokenKind::Ident | music_lex::TokenKind::EscapedIdent));

        let kind = if let (Some(name_tok), Some(_colon_eq)) = (
            first_token,
            node.child_tokens()
                .find(|t| matches!(t.kind(), music_lex::TokenKind::ColonEq)),
        ) {
            // Consume the name token and the ':=' token if present.
            let _ = cursor.bump_token();
            let _ = cursor.bump_token();
            let value = node
                .child_nodes()
                .find(|n| n.kind().is_expr())
                .map(|expr| self.lower_expr(expr))
                .unwrap_or_else(|| {
                    self.error(node.span(), "expected attribute argument expression");
                    self.alloc_expr(self.origin_node(node), music_hir::HirExprKind::Error)
                });
            HirAttrArgKind::Named {
                name: self.intern_ident_token(name_tok),
                value,
            }
        } else {
            let value = node
                .child_nodes()
                .find(|n| n.kind().is_expr())
                .map(|expr| self.lower_expr(expr))
                .unwrap_or_else(|| {
                    self.error(node.span(), "expected attribute argument expression");
                    self.alloc_expr(self.origin_node(node), music_hir::HirExprKind::Error)
                });
            HirAttrArgKind::Positional { value }
        };

        HirAttrArg { origin, kind }
    }
}
