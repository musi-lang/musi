use core::mem;

use music_lex::TokenKind;

use crate::SyntaxNodeKind;
use crate::red::SyntaxNode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TyKindView {
    Named,
    Function,
    Binary,
    Pi,
    Tuple,
    Array,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionTyFlavor {
    Pure,
    Effectful,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryTyOp {
    Sum,
    Product,
}

#[derive(Debug, Clone, Copy)]
pub struct Ty<'tree> {
    syntax: SyntaxNode<'tree>,
}

impl<'tree> Ty<'tree> {
    #[must_use]
    pub fn cast(node: SyntaxNode<'tree>) -> Option<Self> {
        node.kind().is_ty().then_some(Self { syntax: node })
    }

    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree> {
        self.syntax
    }

    /// Classify this type node.
    ///
    /// # Panics
    ///
    /// Panics if the wrapper does not actually hold a type node.
    /// This indicates a broken internal invariant after a bad cast.
    #[must_use]
    pub fn kind(self) -> TyKindView {
        TyKindView::from_syntax(self.syntax.kind()).expect("Ty wrapper always holds type node")
    }

    #[must_use]
    pub fn is_mutable(self) -> bool {
        self.kind() == TyKindView::Named && has_token(self.syntax, &TokenKind::KwMut)
    }

    #[must_use]
    pub fn function_flavor(self) -> Option<FunctionTyFlavor> {
        if self.kind() != TyKindView::Function {
            return None;
        }

        self.syntax
            .child_tokens()
            .find_map(|token| match token.kind() {
                TokenKind::MinusGt => Some(FunctionTyFlavor::Pure),
                TokenKind::TildeGt => Some(FunctionTyFlavor::Effectful),
                _ => None,
            })
    }

    #[must_use]
    pub fn binary_op(self) -> Option<BinaryTyOp> {
        if self.kind() != TyKindView::Binary {
            return None;
        }

        self.syntax
            .child_tokens()
            .find_map(|token| match token.kind() {
                TokenKind::Plus => Some(BinaryTyOp::Sum),
                TokenKind::Star => Some(BinaryTyOp::Product),
                _ => None,
            })
    }
}

impl TyKindView {
    #[must_use]
    pub const fn from_syntax(kind: SyntaxNodeKind) -> Option<Self> {
        match kind {
            SyntaxNodeKind::NamedTy => Some(Self::Named),
            SyntaxNodeKind::FunctionTy => Some(Self::Function),
            SyntaxNodeKind::BinaryTy => Some(Self::Binary),
            SyntaxNodeKind::PiTy => Some(Self::Pi),
            SyntaxNodeKind::TupleTy => Some(Self::Tuple),
            SyntaxNodeKind::ArrayTy => Some(Self::Array),
            _ => None,
        }
    }
}

fn has_token(node: SyntaxNode<'_>, expected: &TokenKind) -> bool {
    node.child_tokens()
        .any(|token| mem::discriminant(token.kind()) == mem::discriminant(expected))
}
