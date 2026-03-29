use core::mem;

use music_lex::TokenKind;

use crate::{SyntaxNode, SyntaxNodeKind, SyntaxToken};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprKindView {
    Sequence,
    Let,
    Import,
    ForeignBlock,
    Data,
    Effect,
    Class,
    Instance,
    Name,
    Literal,
    Tuple,
    Array,
    Record,
    Variant,
    Lambda,
    Call,
    Field,
    Index,
    RecordUpdate,
    TypeTest,
    TypeCast,
    Prefix,
    Binary,
    Case,
    Perform,
    Handle,
    Resume,
    Quote,
    Splice,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryExprOp {
    Assign,
    Pipe,
    Or,
    Xor,
    And,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Symbolic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixExprOp {
    Negate,
    Not,
    Mut,
}

#[derive(Debug, Clone, Copy)]
pub struct SourceFile<'tree> {
    syntax: SyntaxNode<'tree>,
}

#[derive(Debug, Clone, Copy)]
pub struct Expr<'tree> {
    syntax: SyntaxNode<'tree>,
}

#[derive(Debug, Clone, Copy)]
pub struct DeclSurface<'tree> {
    syntax: SyntaxNode<'tree>,
}

impl<'tree> SourceFile<'tree> {
    #[must_use]
    pub fn cast(node: SyntaxNode<'tree>) -> Option<Self> {
        (node.kind() == SyntaxNodeKind::SourceFile).then_some(Self { syntax: node })
    }

    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree> {
        self.syntax
    }

    pub fn expressions(self) -> impl Iterator<Item = Expr<'tree>> + 'tree {
        self.syntax.child_nodes().filter_map(Expr::cast)
    }
}

impl<'tree> Expr<'tree> {
    #[must_use]
    pub fn cast(node: SyntaxNode<'tree>) -> Option<Self> {
        node.kind().is_expr().then_some(Self { syntax: node })
    }

    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree> {
        self.syntax
    }

    /// Classify this expression node.
    ///
    /// # Panics
    ///
    /// Panics if the wrapper does not actually hold an expression node.
    /// This indicates a broken internal invariant after a bad cast.
    #[must_use]
    pub fn kind(self) -> ExprKindView {
        ExprKindView::from_syntax(self.syntax.kind()).expect("Expr wrapper always holds expr node")
    }

    pub fn child_expressions(self) -> impl Iterator<Item = Expr<'tree>> + 'tree {
        self.syntax.child_nodes().filter_map(Expr::cast)
    }

    #[must_use]
    pub fn decl_surface(self) -> Option<DeclSurface<'tree>> {
        supports_decl_surface(self.kind()).then_some(DeclSurface {
            syntax: self.syntax,
        })
    }

    #[must_use]
    pub fn is_optional_chain(self) -> bool {
        self.kind() == ExprKindView::Field && has_token(self.syntax, &TokenKind::QuestionDot)
    }

    #[must_use]
    pub fn is_forced_chain(self) -> bool {
        self.kind() == ExprKindView::Field && has_token(self.syntax, &TokenKind::BangDot)
    }

    #[must_use]
    pub fn binary_op(self) -> Option<BinaryExprOp> {
        if self.kind() != ExprKindView::Binary {
            return None;
        }

        binary_token(self.syntax).map(|token| match token.kind() {
            TokenKind::LtMinus => BinaryExprOp::Assign,
            TokenKind::PipeGt => BinaryExprOp::Pipe,
            TokenKind::KwOr => BinaryExprOp::Or,
            TokenKind::KwXor => BinaryExprOp::Xor,
            TokenKind::KwAnd => BinaryExprOp::And,
            TokenKind::Eq => BinaryExprOp::Eq,
            TokenKind::SlashEq => BinaryExprOp::NotEq,
            TokenKind::Lt => BinaryExprOp::Lt,
            TokenKind::Gt => BinaryExprOp::Gt,
            TokenKind::LtEq => BinaryExprOp::LtEq,
            TokenKind::GtEq => BinaryExprOp::GtEq,
            TokenKind::KwShl => BinaryExprOp::Shl,
            TokenKind::KwShr => BinaryExprOp::Shr,
            TokenKind::Plus => BinaryExprOp::Add,
            TokenKind::Minus => BinaryExprOp::Sub,
            TokenKind::Star => BinaryExprOp::Mul,
            TokenKind::Slash => BinaryExprOp::Div,
            TokenKind::Percent => BinaryExprOp::Mod,
            _ => BinaryExprOp::Symbolic,
        })
    }

    #[must_use]
    pub fn prefix_op(self) -> Option<PrefixExprOp> {
        if self.kind() != ExprKindView::Prefix {
            return None;
        }

        self.syntax
            .child_tokens()
            .find_map(|token| match token.kind() {
                TokenKind::Minus => Some(PrefixExprOp::Negate),
                TokenKind::KwNot => Some(PrefixExprOp::Not),
                TokenKind::KwMut => Some(PrefixExprOp::Mut),
                _ => None,
            })
    }

    #[must_use]
    pub fn is_block_quote(self) -> bool {
        self.kind() == ExprKindView::Quote && has_token(self.syntax, &TokenKind::LBrace)
    }
}

impl<'tree> DeclSurface<'tree> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree> {
        self.syntax
    }

    #[must_use]
    pub fn is_exported(self) -> bool {
        has_token(self.syntax, &TokenKind::KwExport)
    }

    #[must_use]
    pub fn is_opaque(self) -> bool {
        has_token(self.syntax, &TokenKind::KwOpaque)
    }

    #[must_use]
    pub fn is_mutable(self) -> bool {
        has_token(self.syntax, &TokenKind::KwMut)
    }

    #[must_use]
    pub fn is_external(self) -> bool {
        has_token(self.syntax, &TokenKind::KwForeign)
    }

    #[must_use]
    pub fn external_abi_token(self) -> Option<SyntaxToken<'tree>> {
        self.syntax
            .child_tokens()
            .find(|token| matches!(token.kind(), TokenKind::StringLit))
    }
}

impl ExprKindView {
    #[must_use]
    pub const fn from_syntax(kind: SyntaxNodeKind) -> Option<Self> {
        match kind {
            SyntaxNodeKind::SequenceExpr => Some(Self::Sequence),
            SyntaxNodeKind::LetExpr => Some(Self::Let),
            SyntaxNodeKind::ImportExpr => Some(Self::Import),
            SyntaxNodeKind::ForeignBlockExpr => Some(Self::ForeignBlock),
            SyntaxNodeKind::DataExpr => Some(Self::Data),
            SyntaxNodeKind::EffectExpr => Some(Self::Effect),
            SyntaxNodeKind::ClassExpr => Some(Self::Class),
            SyntaxNodeKind::InstanceExpr => Some(Self::Instance),
            SyntaxNodeKind::NameExpr => Some(Self::Name),
            SyntaxNodeKind::LiteralExpr => Some(Self::Literal),
            SyntaxNodeKind::TupleExpr => Some(Self::Tuple),
            SyntaxNodeKind::ArrayExpr => Some(Self::Array),
            SyntaxNodeKind::RecordExpr => Some(Self::Record),
            SyntaxNodeKind::VariantExpr => Some(Self::Variant),
            SyntaxNodeKind::LambdaExpr => Some(Self::Lambda),
            SyntaxNodeKind::CallExpr => Some(Self::Call),
            SyntaxNodeKind::FieldExpr => Some(Self::Field),
            SyntaxNodeKind::IndexExpr => Some(Self::Index),
            SyntaxNodeKind::RecordUpdateExpr => Some(Self::RecordUpdate),
            SyntaxNodeKind::TypeTestExpr => Some(Self::TypeTest),
            SyntaxNodeKind::TypeCastExpr => Some(Self::TypeCast),
            SyntaxNodeKind::PrefixExpr => Some(Self::Prefix),
            SyntaxNodeKind::BinaryExpr => Some(Self::Binary),
            SyntaxNodeKind::CaseExpr => Some(Self::Case),
            SyntaxNodeKind::PerformExpr => Some(Self::Perform),
            SyntaxNodeKind::HandleExpr => Some(Self::Handle),
            SyntaxNodeKind::ResumeExpr => Some(Self::Resume),
            SyntaxNodeKind::QuoteExpr => Some(Self::Quote),
            SyntaxNodeKind::SpliceExpr => Some(Self::Splice),
            _ => None,
        }
    }
}

fn has_token(node: SyntaxNode<'_>, expected: &TokenKind) -> bool {
    node.child_tokens()
        .any(|token| same_token_kind(token.kind(), expected))
}

const fn supports_decl_surface(kind: ExprKindView) -> bool {
    matches!(
        kind,
        ExprKindView::Let | ExprKindView::Instance | ExprKindView::ForeignBlock
    )
}

fn same_token_kind(left: &TokenKind, right: &TokenKind) -> bool {
    mem::discriminant(left) == mem::discriminant(right)
}

fn binary_token(node: SyntaxNode<'_>) -> Option<SyntaxToken<'_>> {
    node.child_tokens().find(|token| {
        matches!(
            token.kind(),
            TokenKind::LtMinus
                | TokenKind::PipeGt
                | TokenKind::KwOr
                | TokenKind::KwXor
                | TokenKind::KwAnd
                | TokenKind::Eq
                | TokenKind::SlashEq
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::LtEq
                | TokenKind::GtEq
                | TokenKind::KwShl
                | TokenKind::KwShr
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::SymOp
                | TokenKind::Amp
                | TokenKind::Caret
                | TokenKind::Tilde
        )
    })
}
