//! AST type definitions for the Musi compiler.
//!
//! Every node carries a [`Span`] for source location tracking.
//! Recursive expression children are arena-allocated via [`Idx<Expr>`];
//! types and patterns are stored inline or boxed where noted.

use musi_shared::{Arena, Idx, Span, Symbol};

/// Holds the three arenas that back all AST nodes for a single parse.
pub struct ParseCtx {
    pub exprs: Arena<Expr>,
    pub tys: Arena<Ty>,
    pub pats: Arena<Pat>,
}

impl ParseCtx {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            exprs: Arena::new(),
            tys: Arena::new(),
            pats: Arena::new(),
        }
    }
}

impl Default for ParseCtx {
    fn default() -> Self {
        Self::new()
    }
}

/// The result of parsing a single source file.
pub struct ParsedModule {
    /// Top-level statement expressions.
    pub items: Vec<Idx<Expr>>,
    pub ctx: ParseCtx,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitValue {
    Int(i64),
    Float(f64),
    Str(Symbol),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindKind {
    Const,
    Var,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Modifier {
    Export,
    Opaque,
    Native(Option<Symbol>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Attr {
    pub name: Symbol,
    pub args: Vec<AttrArg>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AttrArg {
    Named {
        name: Symbol,
        value: Option<LitValue>,
        span: Span,
    },
    Lit(LitValue, Span),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyParam {
    pub name: Symbol,
    pub bounds: Vec<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub attrs: Vec<Attr>,
    pub mutable: bool,
    pub name: Symbol,
    pub ty: Option<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecField {
    pub attrs: Vec<Attr>,
    pub mutable: bool,
    pub name: Symbol,
    pub ty: Option<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportItem {
    pub name: Symbol,
    pub alias: Option<Symbol>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub attrs: Vec<Attr>,
    pub pat: Pat,
    pub guard: Option<Idx<Expr>>,
    pub body: Idx<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChoiceVariant {
    pub attrs: Vec<Attr>,
    pub name: Symbol,
    pub payload: Option<VariantPayload>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariantPayload {
    Positional(Vec<Ty>),
    Named(Vec<RecField>),
    Discriminant(LitValue), // Name := 0 / "tag" / ...
}

/// A condition in `if`/`while`/`loop` -- either a plain expression or a
/// pattern-binding destructure: `case const/var pat := expr`.
#[derive(Debug, Clone, PartialEq)]
pub enum Cond {
    Expr(Idx<Expr>),
    Case {
        kind: BindKind,
        pat: Pat,
        init: Idx<Expr>,
        span: Span,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Bitwise
    BitOr,
    BitXor,
    BitAnd,
    Shl,
    Shr,
    // Logical
    And,
    Or,
    Xor,
    // Comparison (non-associative in grammar)
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    In,
    // Range (non-associative)
    Range,
    RangeExcl,
    // Cons (left-assoc)
    Cons,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOp {
    Neg,
    Not,
    Deref,
    AddrOf,
    BitNot,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PostfixOp {
    Call {
        args: Vec<Idx<Expr>>,
        span: Span,
    },
    Index {
        args: Vec<Idx<Expr>>,
        span: Span,
    },
    Field {
        name: Symbol,
        span: Span,
    },
    RecDot {
        fields: Vec<RecLitField>,
        span: Span,
    },
    As {
        ty: Ty,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum RecLitField {
    Named {
        attrs: Vec<Attr>,
        mutable: bool,
        name: Symbol,
        value: Idx<Expr>,
        span: Span,
    },
    Spread {
        expr: Idx<Expr>,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ty {
    Arrow {
        params: Vec<Self>,
        ret: Box<Self>,
        span: Span,
    },
    Named {
        name: Symbol,
        args: Vec<Self>,
        span: Span,
    },
    Prod {
        elements: Vec<Self>,
        span: Span,
    },
    Arr {
        element: Box<Self>,
        size: Option<Idx<Expr>>,
        span: Span,
    },
    Var {
        name: Symbol,
        span: Span,
    },
    Error {
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pat {
    Ident {
        name: Symbol,
        suffix: Option<PatSuffix>,
        span: Span,
    },
    Lit {
        value: LitValue,
        span: Span,
    },
    Wild {
        span: Span,
    },
    Prod {
        elements: Vec<Self>,
        span: Span,
    },
    Arr {
        elements: Vec<Self>,
        span: Span,
    },
    AnonRec {
        fields: Vec<PatField>,
        span: Span,
    },
    Or {
        alternatives: Vec<Self>,
        span: Span,
    },
    Error {
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatSuffix {
    Positional { args: Vec<Pat>, span: Span },
    Named { fields: Vec<PatField>, span: Span },
}

#[derive(Debug, Clone, PartialEq)]
pub struct PatField {
    pub attrs: Vec<Attr>,
    pub mutable: bool,
    pub name: Symbol,
    pub pat: Option<Pat>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Atoms
    Lit {
        value: LitValue,
        span: Span,
    },
    Ident {
        name: Symbol,
        span: Span,
    },
    Unit {
        span: Span,
    },
    Paren {
        inner: Idx<Self>,
        span: Span,
    },
    Tuple {
        elements: Vec<Idx<Self>>,
        span: Span,
    },
    Block {
        stmts: Vec<Idx<Self>>,
        tail: Option<Idx<Self>>,
        span: Span,
    },
    Array {
        items: Vec<ArrayItem>,
        span: Span,
    },
    AnonRec {
        fields: Vec<RecLitField>,
        span: Span,
    },

    // Control flow
    If {
        cond: Box<Cond>,
        then_body: Idx<Self>,
        elif_chains: Vec<ElifChain>,
        else_body: Option<Idx<Self>>,
        span: Span,
    },
    Match {
        scrutinee: Idx<Self>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    While {
        cond: Box<Cond>,
        guard: Option<Idx<Self>>,
        body: Idx<Self>,
        span: Span,
    },
    Loop {
        body: Idx<Self>,
        post_cond: Option<Box<Cond>>,
        span: Span,
    },
    For {
        pat: Pat,
        iter: Idx<Self>,
        guard: Option<Idx<Self>>,
        body: Idx<Self>,
        span: Span,
    },
    Label {
        name: Symbol,
        body: Idx<Self>,
        span: Span,
    },
    Return {
        value: Option<Idx<Self>>,
        span: Span,
    },
    Break {
        label: Option<Symbol>,
        value: Option<Idx<Self>>,
        span: Span,
    },
    Cycle {
        label: Option<Symbol>,
        guard: Option<Idx<Self>>,
        span: Span,
    },
    Defer {
        body: Idx<Self>,
        span: Span,
    },
    Import {
        items: ImportClause,
        path: Symbol,
        span: Span,
    },

    // Declarations (first-class expressions in Musi)
    Record {
        attrs: Vec<Attr>,
        modifiers: Vec<Modifier>,
        name: Option<Symbol>,
        ty_params: Vec<TyParam>,
        fields: Vec<RecField>,
        span: Span,
    },
    Choice {
        attrs: Vec<Attr>,
        modifiers: Vec<Modifier>,
        name: Option<Symbol>,
        ty_params: Vec<TyParam>,
        variants: Vec<ChoiceVariant>,
        span: Span,
    },
    FnDef {
        attrs: Vec<Attr>,
        modifiers: Vec<Modifier>,
        name: Symbol,
        ty_params: Vec<TyParam>,
        params: Vec<Param>,
        ret_ty: Option<Ty>,
        body: Option<Idx<Self>>,
        span: Span,
    },
    Lambda {
        attrs: Vec<Attr>,
        ty_params: Vec<TyParam>,
        params: Vec<Param>,
        ret_ty: Option<Ty>,
        body: Idx<Self>,
        span: Span,
    },
    Bind {
        attrs: Vec<Attr>,
        modifiers: Vec<Modifier>,
        kind: BindKind,
        pat: Pat,
        ty: Option<Ty>,
        init: Option<Idx<Self>>,
        span: Span,
    },

    // Operators
    Prefix {
        op: PrefixOp,
        operand: Idx<Self>,
        span: Span,
    },
    Binary {
        op: BinOp,
        lhs: Idx<Self>,
        rhs: Idx<Self>,
        span: Span,
    },
    Assign {
        target: Idx<Self>,
        value: Idx<Self>,
        span: Span,
    },
    Postfix {
        base: Idx<Self>,
        op: PostfixOp,
        span: Span,
    },

    // Error sentinel
    Error {
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElifChain {
    pub cond: Box<Cond>,
    pub guard: Option<Idx<Expr>>,
    pub body: Idx<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArrayItem {
    Single(Idx<Expr>),
    Spread(Idx<Expr>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportClause {
    Glob,
    Items(Vec<ImportItem>),
}

#[cfg(test)]
mod tests;
