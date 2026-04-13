use super::kinds::SyntaxNodeKind;
use super::syntax::SyntaxNode;

#[derive(Debug, Clone, Copy)]
pub struct Program<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct Stmt<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprKind {
    Literal,
    Template,
    Name,
    Pi,
    Lambda,
    Tuple,
    Sequence,
    Array,
    Record,
    Variant,
    Call,
    Apply,
    Index,
    RecordUpdate,
    FieldAccess,
    TypeTest,
    TypeCast,
    Prefix,
    Binary,
    Case,
    Attributed,
    Let,
    Resume,
    Import,
    Data,
    Effect,
    Class,
    Instance,
    Perform,
    Handler,
    Handle,
    Foreign,
    Quote,
    Splice,
    Other,
}

#[derive(Debug, Clone, Copy)]
pub enum Expr<'tree, 'src> {
    Array(ArrayExpr<'tree, 'src>),
    Binary(BinaryExpr<'tree, 'src>),
    Call(CallExpr<'tree, 'src>),
    Case(CaseExpr<'tree, 'src>),
    Handle(HandleExpr<'tree, 'src>),
    Handler(HandlerExpr<'tree, 'src>),
    Import(ImportExpr<'tree, 'src>),
    Instance(InstanceExpr<'tree, 'src>),
    Let(LetExpr<'tree, 'src>),
    Quote(QuoteExpr<'tree, 'src>),
    Other(SyntaxNode<'tree, 'src>),
}

#[derive(Debug, Clone, Copy)]
pub struct LetExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct BinaryExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct CallExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct ArrayExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct CaseExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct HandleExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct HandlerExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct ImportExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct InstanceExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

#[derive(Debug, Clone, Copy)]
pub struct QuoteExpr<'tree, 'src> {
    syntax: SyntaxNode<'tree, 'src>,
}

impl<'tree, 'src> Program<'tree, 'src> {
    #[must_use]
    pub fn cast(node: SyntaxNode<'tree, 'src>) -> Option<Self> {
        (node.kind() == SyntaxNodeKind::SourceFile).then_some(Self { syntax: node })
    }

    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }

    pub fn statements(self) -> impl Iterator<Item = Stmt<'tree, 'src>> + 'tree {
        self.syntax.child_nodes().filter_map(Stmt::cast)
    }
}

impl<'tree, 'src> Stmt<'tree, 'src> {
    #[must_use]
    pub fn cast(node: SyntaxNode<'tree, 'src>) -> Option<Self> {
        (node.kind() == SyntaxNodeKind::SequenceExpr).then_some(Self { syntax: node })
    }

    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }

    #[must_use]
    pub fn expression(self) -> Option<Expr<'tree, 'src>> {
        self.syntax.child_nodes().find_map(Expr::cast)
    }
}

impl<'tree, 'src> Expr<'tree, 'src> {
    #[must_use]
    pub fn cast(node: SyntaxNode<'tree, 'src>) -> Option<Self> {
        match node.kind() {
            SyntaxNodeKind::ArrayExpr => Some(Self::Array(ArrayExpr { syntax: node })),
            SyntaxNodeKind::BinaryExpr => Some(Self::Binary(BinaryExpr { syntax: node })),
            SyntaxNodeKind::CallExpr => Some(Self::Call(CallExpr { syntax: node })),
            SyntaxNodeKind::CaseExpr => Some(Self::Case(CaseExpr { syntax: node })),
            SyntaxNodeKind::HandlerExpr => Some(Self::Handler(HandlerExpr { syntax: node })),
            SyntaxNodeKind::HandleExpr => Some(Self::Handle(HandleExpr { syntax: node })),
            SyntaxNodeKind::ImportExpr => Some(Self::Import(ImportExpr { syntax: node })),
            SyntaxNodeKind::InstanceExpr => Some(Self::Instance(InstanceExpr { syntax: node })),
            SyntaxNodeKind::LetExpr => Some(Self::Let(LetExpr { syntax: node })),
            SyntaxNodeKind::QuoteExpr => Some(Self::Quote(QuoteExpr { syntax: node })),
            kind if kind.is_expr() => Some(Self::Other(node)),
            _ => None,
        }
    }

    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        match self {
            Self::Array(expr) => expr.syntax,
            Self::Binary(expr) => expr.syntax,
            Self::Call(expr) => expr.syntax,
            Self::Case(expr) => expr.syntax,
            Self::Handler(expr) => expr.syntax,
            Self::Handle(expr) => expr.syntax,
            Self::Import(expr) => expr.syntax,
            Self::Instance(expr) => expr.syntax,
            Self::Let(expr) => expr.syntax,
            Self::Quote(expr) => expr.syntax,
            Self::Other(node) => node,
        }
    }

    #[must_use]
    pub fn kind(self) -> ExprKind {
        let kind = self.syntax().kind();
        if !kind.is_expr() {
            return ExprKind::Other;
        }
        match kind {
            SyntaxNodeKind::LiteralExpr => ExprKind::Literal,
            SyntaxNodeKind::TemplateExpr => ExprKind::Template,
            SyntaxNodeKind::NameExpr => ExprKind::Name,
            SyntaxNodeKind::PiExpr => ExprKind::Pi,
            SyntaxNodeKind::LambdaExpr => ExprKind::Lambda,
            SyntaxNodeKind::TupleExpr => ExprKind::Tuple,
            SyntaxNodeKind::SequenceExpr => ExprKind::Sequence,
            SyntaxNodeKind::ArrayExpr => ExprKind::Array,
            SyntaxNodeKind::RecordExpr => ExprKind::Record,
            SyntaxNodeKind::VariantExpr => ExprKind::Variant,
            SyntaxNodeKind::CallExpr => ExprKind::Call,
            SyntaxNodeKind::ApplyExpr => ExprKind::Apply,
            SyntaxNodeKind::IndexExpr => ExprKind::Index,
            SyntaxNodeKind::RecordUpdateExpr => ExprKind::RecordUpdate,
            SyntaxNodeKind::FieldExpr => ExprKind::FieldAccess,
            SyntaxNodeKind::TypeTestExpr => ExprKind::TypeTest,
            SyntaxNodeKind::TypeCastExpr => ExprKind::TypeCast,
            SyntaxNodeKind::PrefixExpr | SyntaxNodeKind::PostfixExpr => ExprKind::Prefix,
            SyntaxNodeKind::BinaryExpr => ExprKind::Binary,
            SyntaxNodeKind::CaseExpr => ExprKind::Case,
            SyntaxNodeKind::AttributedExpr => ExprKind::Attributed,
            SyntaxNodeKind::LetExpr => ExprKind::Let,
            SyntaxNodeKind::ResumeExpr => ExprKind::Resume,
            SyntaxNodeKind::ImportExpr => ExprKind::Import,
            SyntaxNodeKind::DataExpr => ExprKind::Data,
            SyntaxNodeKind::EffectExpr => ExprKind::Effect,
            SyntaxNodeKind::ClassExpr => ExprKind::Class,
            SyntaxNodeKind::InstanceExpr => ExprKind::Instance,
            SyntaxNodeKind::PerformExpr => ExprKind::Perform,
            SyntaxNodeKind::HandlerExpr => ExprKind::Handler,
            SyntaxNodeKind::HandleExpr => ExprKind::Handle,
            SyntaxNodeKind::ForeignBlockExpr => ExprKind::Foreign,
            SyntaxNodeKind::QuoteExpr => ExprKind::Quote,
            SyntaxNodeKind::SpliceExpr => ExprKind::Splice,
            _ => ExprKind::Other,
        }
    }
}

impl<'tree, 'src> LetExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}

impl<'tree, 'src> BinaryExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}

impl<'tree, 'src> CallExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}

impl<'tree, 'src> ArrayExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}

impl<'tree, 'src> CaseExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}

impl<'tree, 'src> HandleExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}

impl<'tree, 'src> HandlerExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}

impl<'tree, 'src> ImportExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}

impl<'tree, 'src> InstanceExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}

impl<'tree, 'src> QuoteExpr<'tree, 'src> {
    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree, 'src> {
        self.syntax
    }
}
