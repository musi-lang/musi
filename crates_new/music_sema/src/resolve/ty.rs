use music_ast::{SyntaxNode, SyntaxNodeKind, SyntaxToken};
use music_hir::{HirArrowFlavor, HirDim, HirTy, HirTyBinOp, HirTyId, HirTyKind};
use music_lex::TokenKind;

use super::Resolver;

impl<'a, 'tree> Resolver<'a, 'tree> {
    pub(super) fn lower_ty(&mut self, node: SyntaxNode<'tree>) -> HirTyId {
        let origin = self.origin_node(node);

        // `mut T` is represented by rewrapping the underlying node kind.
        if node
            .child_tokens()
            .next()
            .is_some_and(|t| matches!(t.kind(), TokenKind::KwMut))
        {
            let inner = node
                .child_nodes()
                .find(|n| n.kind().is_ty())
                .map(|n| self.lower_ty(n))
                .unwrap_or_else(|| {
                    self.error(node.span(), "expected type after 'mut'");
                    self.alloc_ty(origin, HirTyKind::Error)
                });
            return self.alloc_ty(origin, HirTyKind::Mut { base: inner });
        }

        match node.kind() {
            SyntaxNodeKind::NamedTy => self.lower_named_ty(node),
            SyntaxNodeKind::FunctionTy => self.lower_function_ty(node),
            SyntaxNodeKind::BinaryTy => self.lower_binary_ty(node),
            SyntaxNodeKind::PiTy => self.lower_pi_ty(node),
            SyntaxNodeKind::TupleTy => self.lower_tuple_ty(node),
            SyntaxNodeKind::ArrayTy => self.lower_array_ty(node),
            _ => {
                self.error(node.span(), "expected type");
                self.alloc_ty(origin, HirTyKind::Error)
            }
        }
    }

    fn alloc_ty(&mut self, origin: music_hir::HirOrigin, kind: HirTyKind) -> HirTyId {
        self.store.tys.alloc(HirTy { origin, kind })
    }

    fn lower_named_ty(&mut self, node: SyntaxNode<'tree>) -> HirTyId {
        let origin = self.origin_node(node);
        let Some(name_tok) = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
        else {
            return self.alloc_ty(origin, HirTyKind::Error);
        };

        let name = self.intern_ident_token(name_tok);
        let args: Vec<HirTyId> = node
            .child_nodes()
            .filter(|n| n.kind().is_ty())
            .map(|ty| self.lower_ty(ty))
            .collect();

        self.alloc_ty(
            origin,
            HirTyKind::Named {
                name,
                args: args.into_boxed_slice(),
            },
        )
    }

    fn lower_function_ty(&mut self, node: SyntaxNode<'tree>) -> HirTyId {
        let origin = self.origin_node(node);
        let mut tys = node.child_nodes().filter(|n| n.kind().is_ty());
        let input = tys
            .next()
            .map(|n| self.lower_ty(n))
            .unwrap_or_else(|| self.alloc_ty(origin, HirTyKind::Error));
        let output = tys
            .next()
            .map(|n| self.lower_ty(n))
            .unwrap_or_else(|| self.alloc_ty(origin, HirTyKind::Error));

        let flavor = node
            .child_tokens()
            .find_map(|t| match t.kind() {
                TokenKind::MinusGt => Some(HirArrowFlavor::Pure),
                TokenKind::TildeGt => Some(HirArrowFlavor::Effectful),
                _ => None,
            })
            .unwrap_or(HirArrowFlavor::Pure);

        self.alloc_ty(
            origin,
            HirTyKind::Arrow {
                flavor,
                input,
                output,
            },
        )
    }

    fn lower_binary_ty(&mut self, node: SyntaxNode<'tree>) -> HirTyId {
        let origin = self.origin_node(node);
        let mut tys = node.child_nodes().filter(|n| n.kind().is_ty());
        let left = tys
            .next()
            .map(|n| self.lower_ty(n))
            .unwrap_or_else(|| self.alloc_ty(origin, HirTyKind::Error));
        let right = tys
            .next()
            .map(|n| self.lower_ty(n))
            .unwrap_or_else(|| self.alloc_ty(origin, HirTyKind::Error));

        let op = node
            .child_tokens()
            .find_map(|t| match t.kind() {
                TokenKind::Plus => Some(HirTyBinOp::Sum),
                TokenKind::Star => Some(HirTyBinOp::Product),
                _ => None,
            })
            .unwrap_or(HirTyBinOp::Sum);

        self.alloc_ty(
            origin,
            HirTyKind::Binary {
                op,
                left,
                right,
            },
        )
    }

    fn lower_pi_ty(&mut self, node: SyntaxNode<'tree>) -> HirTyId {
        let origin = self.origin_node(node);
        let mut tokens = node.child_tokens();
        let binder_tok = tokens.find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let Some(binder_tok) = binder_tok else {
            return self.alloc_ty(origin, HirTyKind::Error);
        };
        let binder = self.intern_ident_token(binder_tok);

        let mut tys = node.child_nodes().filter(|n| n.kind().is_ty());
        let binder_ty = tys
            .next()
            .map(|n| self.lower_ty(n))
            .unwrap_or_else(|| self.alloc_ty(origin, HirTyKind::Error));

        self.push_scope();
        self.define(binder);
        let body = tys
            .next()
            .map(|n| self.lower_ty(n))
            .unwrap_or_else(|| self.alloc_ty(origin, HirTyKind::Error));
        self.pop_scope();

        self.alloc_ty(
            origin,
            HirTyKind::Pi {
                binder,
                binder_ty,
                body,
            },
        )
    }

    fn lower_tuple_ty(&mut self, node: SyntaxNode<'tree>) -> HirTyId {
        let origin = self.origin_node(node);
        let items: Vec<HirTyId> = node
            .child_nodes()
            .filter(|n| n.kind().is_ty())
            .map(|n| self.lower_ty(n))
            .collect();
        self.alloc_ty(origin, HirTyKind::Tuple {
            items: items.into_boxed_slice(),
        })
    }

    fn lower_array_ty(&mut self, node: SyntaxNode<'tree>) -> HirTyId {
        let origin = self.origin_node(node);
        let mut dims = Vec::new();
        for tok in node.child_tokens() {
            match tok.kind() {
                TokenKind::IntLit => dims.push(HirDim::IntLit { span: tok.span() }),
                TokenKind::Ident | TokenKind::EscapedIdent => {
                    dims.push(HirDim::Name {
                        name: self.intern_ident_token(tok),
                    });
                }
                TokenKind::LBracket | TokenKind::RBracket | TokenKind::Comma => {}
                _ => {}
            }
        }

        let elem = node
            .child_nodes()
            .filter(|n| n.kind().is_ty())
            .last()
            .map(|n| self.lower_ty(n))
            .unwrap_or_else(|| self.alloc_ty(origin, HirTyKind::Error));

        self.alloc_ty(
            origin,
            HirTyKind::Array {
                dims: dims.into_boxed_slice(),
                elem,
            },
        )
    }

    pub(super) fn string_lit_token(&mut self, token: SyntaxToken<'tree>) -> music_hir::HirStringLit {
        music_hir::HirStringLit::new(token.span(), Some(token.id()))
    }
}
