use musi_basic::span::Span;
use musi_lex::token::TokenKind;

use crate::{CondId, ExprId, Ident, PatId, StmtId, TyExprId};

#[derive(Debug, Clone)]
pub enum LitKind {
    Int(i64),
    Real(f64),
    String(Ident),
    Rune(char),
    Bool(bool),
    Template(Vec<TemplatePart>),
}

#[derive(Debug, Clone)]
pub enum TemplatePart {
    Text(Ident),
    Expr(ExprId),
}

#[derive(Debug, Clone)]
pub struct TyExpr {
    pub id: TyExprId,
    pub kind: TyExprKind,
    pub span: Span,
}

impl TyExpr {
    #[must_use]
    pub const fn new(id: TyExprId, kind: TyExprKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum TyExprKind {
    /// `Int`, `String`
    Ident(Ident),
    /// `List[Int]`, `Map[String, Int]`
    App { base: Ident, args: Vec<TyExprId> },
    /// `?Int`
    Optional(TyExprId),
    /// `[10]Int`, `[]Int`
    Array { size: Option<i64>, elem: TyExprId },
    /// `^Int`
    Ptr(TyExprId),
    /// `Int -> String`
    Fn { param: TyExprId, ret: TyExprId },
    /// `(Int, String)`
    Tuple(Vec<TyExprId>),
}

#[derive(Debug, Clone)]
pub struct Pat {
    pub id: PatId,
    pub kind: PatKind,
    pub span: Span,
}

impl Pat {
    #[must_use]
    pub const fn new(id: PatId, kind: PatKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum PatKind {
    /// `x`, `foo`
    Ident(Ident),
    /// `42`, `"hello"`
    Lit(LitKind),
    ///`_`
    Wild,
    /// `(a, b, c)`
    Tuple(Vec<PatId>),
    /// `[a, b, c]`
    Array(Vec<PatId>),
    /// `Point.{x, y}`
    Record {
        base: Option<ExprId>,
        fields: Vec<Ident>,
    },
    /// `Some(x)`, `None`
    Variant {
        name: Ident,
        ty_args: Vec<TyExprId>,
        args: Vec<PatId>,
    },
    /// `head :: tail`
    Cons(Vec<PatId>),
    /// `a | b`
    Or(Vec<PatId>),
    /// `pat as name`
    As { inner: PatId, binding: Ident },
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub id: ExprId,
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    #[must_use]
    pub const fn new(id: ExprId, kind: ExprKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

#[derive(Debug, Clone)]
pub struct Cond {
    pub id: CondId,
    pub kind: CondKind,
}

impl Cond {
    #[must_use]
    pub const fn new(id: CondId, kind: CondKind) -> Self {
        Self { id, kind }
    }
}

#[derive(Debug, Clone)]
pub enum CondKind {
    /// `cond`
    Expr(ExprId),
    /// `case pat := expr, expr, expr, ...`
    Case {
        pat: PatId,
        init: ExprId,
        extra: Vec<ExprId>,
    },
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// `42`, `"hello"`, `true`
    Lit(LitKind),
    /// `x`, `foo`
    Ident(Ident),
    /// `(a, b, c)`
    Tuple(Vec<ExprId>),
    /// `[a, b, c]`
    Array(Vec<ExprId>),
    /// `Point.{x := 1, y := 2}`
    Record {
        base: Option<ExprId>,
        fields: Vec<Field>,
    },
    /// `{ stmt; stmt; expr }`
    Block {
        stmts: Vec<StmtId>,
        expr: Option<ExprId>,
    },
    /// `if cond { } else if { } else { }`
    If {
        cond: CondId,
        then_br: ExprId,
        else_br: Option<ExprId>,
    },
    While {
        cond: CondId,
        guard: Option<ExprId>,
        body: ExprId,
    },
    /// `for pat in iter { }`
    For {
        pat: PatId,
        iter: ExprId,
        guard: Option<ExprId>,
        body: ExprId,
    },
    /// `match expr { cases }`
    Match {
        scrutinee: ExprId,
        cases: Vec<MatchCase>,
    },
    /// `return expr`
    Return(Option<ExprId>),
    /// `defer expr`
    Defer(ExprId),
    /// `break expr`
    Break(Option<ExprId>),
    /// `cycle`
    Cycle,
    /// `unsafe { }`
    Unsafe(ExprId),
    /// `import "path"`
    Import(Ident),
    /// `record Point { x: Int; y: Int }`
    RecordDef {
        attrs: Vec<Attr>,
        mods: Modifiers,
        name: Option<Ident>,
        ty_params: Vec<Ident>,
        fields: Vec<Field>,
    },
    /// `choice Option[T] { case Some(T), case None }`
    ChoiceDef {
        attrs: Vec<Attr>,
        mods: Modifiers,
        name: Option<Ident>,
        ty_params: Vec<Ident>,
        cases: Vec<ChoiceCase>,
    },
    /// `alias Name := Type`
    Alias {
        attrs: Vec<Attr>,
        mods: Modifiers,
        name: Ident,
        ty_params: Vec<Ident>,
        ty: TyExprId,
    },
    /// `fn name(params) { body }` or `fn(params) => expr`
    Fn {
        attrs: Vec<Attr>,
        mods: Modifiers,
        sig: FnSig,
        body: ExprId,
    },
    /// `val x := 1` or `var x := 1`
    Bind {
        mods: Modifiers,
        mutable: bool,
        pat: PatId,
        ty: Option<TyExprId>,
        init: ExprId,
    },
    /// `f(args)`
    Call { callee: ExprId, args: Vec<ExprId> },
    /// `arr[i]`
    Index { base: ExprId, index: ExprId },
    /// `obj.field`
    Field { base: ExprId, field: Ident },
    /// `expr.^`
    Deref(ExprId),
    /// `expr?` — nil propagation
    Propagate(ExprId),
    /// `expr!` — force unwrap
    Force(ExprId),
    /// `-x`, `not x`, `~x`, `@x`
    Unary { op: TokenKind, operand: ExprId },
    /// `a + b`, `a and b`
    Binary {
        op: TokenKind,
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `a..b`, `a..<b`
    Range {
        start: ExprId,
        end: Option<ExprId>,
        inclusive: bool,
    },
    /// `x <- y`
    Assign { target: ExprId, value: ExprId },
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub id: StmtId,
    pub kind: StmtKind,
    pub span: Span,
}

impl Stmt {
    #[must_use]
    pub const fn new(id: StmtId, kind: StmtKind, span: Span) -> Self {
        Self { id, kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// `expr;`
    Expr(ExprId),
}

/// `[var] name [: ty] [:= init]`
#[derive(Debug, Clone)]
pub struct Field {
    pub mutable: bool,
    pub name: Ident,
    pub ty: Option<TyExprId>,
    pub init: Option<ExprId>,
}

/// `name[T](params): RetType`
#[derive(Debug, Clone)]
pub struct FnSig {
    pub name: Option<Ident>,
    pub ty_params: Vec<Ident>,
    pub params: Vec<Field>,
    pub ret: Option<TyExprId>,
    pub span: Span,
}

/// `case pat if guard => body`
#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pat: PatId,
    pub guard: Option<ExprId>,
    pub body: ExprId,
}

/// `case Name[T](fields)`
#[derive(Debug, Clone)]
pub struct ChoiceCase {
    pub name: Ident,
    pub ty_args: Vec<TyExprId>,
    pub fields: Vec<ChoiceCaseItem>,
}

#[derive(Debug, Clone)]
pub enum ChoiceCaseItem {
    Type(TyExprId),
    Field(Field),
}

/// `@[Name(args)]`
#[derive(Debug, Clone)]
pub struct Attr {
    pub name: Ident,
    pub args: Vec<AttrArg>,
}

/// `name := value` or literal
#[derive(Debug, Clone)]
pub struct AttrArg {
    pub name: Option<Ident>,
    pub value: Option<ExprId>,
    pub lit: Option<LitKind>,
}

#[derive(Debug, Clone, Default)]
pub struct Modifiers {
    pub exportness: bool,
    pub externness: (Option<Ident>, bool),
    pub unsafeness: bool,
}

#[derive(Debug)]
pub struct Prog {
    pub stmts: Vec<StmtId>,
}
