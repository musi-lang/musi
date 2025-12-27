use musi_basic::span::Span;
use musi_lex::token::TokenKind;

use crate::{
    AttrArgs, Attrs, ChoiceCaseItems, CondId, ExprId, ExprIds, Fields, Ident, Idents, OptExprId,
    OptIdent, OptTyExprId, PatId, PatIds, StmtId, StmtIds, TyExprId, TyExprIds,
};

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
    App { base: Ident, args: TyExprIds },
    /// `?Int`
    Optional(TyExprId),
    /// `[10]Int`, `[]Int`
    Array { size: Option<i64>, elem: TyExprId },
    /// `^Int`
    Ptr(TyExprId),
    /// `Int -> String`
    Fn { param: TyExprId, ret: TyExprId },
    /// `(Int, String)`
    Tuple(TyExprIds),
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
    Tuple(PatIds),
    /// `[a, b, c]`
    Array(PatIds),
    /// `Point.{x, y}`
    Record { base: OptExprId, fields: Idents },
    /// `Some(x)`, `None`
    Choice {
        name: Ident,
        ty_args: TyExprIds,
        args: PatIds,
    },
    /// `head :: tail`
    Cons(PatIds),
    /// `a | b`
    Or(PatIds),
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
        extra: ExprIds,
    },
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// `42`, `"hello"`, `true`
    Lit(LitKind),
    /// `x`, `foo`
    Ident(Ident),
    /// `(a, b, c)`
    Tuple(ExprIds),
    /// `[a, b, c]`
    Array(ExprIds),
    /// `Point.{x := 1, y := 2}`
    Record {
        base: OptExprId,
        fields: Fields,
    },
    /// Block: `{ stmt; stmt; expr }`
    Block {
        stmts: StmtIds,
        expr: OptExprId,
    },
    /// `if cond { } else if { } else { }`
    If {
        cond: CondId,
        then_br: ExprId,
        else_br: OptExprId,
    },
    While {
        cond: CondId,
        body: ExprId,
    },
    /// `for pat in iter { }`
    For {
        pat: PatId,
        iter: ExprId,
        body: ExprId,
    },
    /// `match expr { cases }`
    Match {
        scrutinee: ExprId,
        cases: Vec<MatchCase>,
    },
    /// `return expr`
    Return(OptExprId),
    /// `defer expr`
    Defer(ExprId),
    /// `break expr`
    Break(OptExprId),
    /// `cycle`
    Cycle,
    /// `unsafe { }`
    Unsafe(ExprId),
    /// `import "path"`
    Import(Ident),
    /// `extern "C" { fn foo(); }`
    Extern {
        abi: OptIdent,
        fns: Vec<FnSig>,
    },
    /// `record Point { x: Int; y: Int }`
    RecordDef {
        attrs: Attrs,
        mods: Modifiers,
        name: OptIdent,
        ty_params: Idents,
        fields: Fields,
    },
    /// `choice Option[T] { case Some(T), case None }`
    ChoiceDef {
        attrs: Attrs,
        mods: Modifiers,
        name: OptIdent,
        ty_params: Idents,
        cases: Vec<ChoiceCase>,
    },
    /// `alias Name := Type`
    Alias {
        attrs: Attrs,
        mods: Modifiers,
        name: Ident,
        ty_params: Idents,
        ty: TyExprId,
    },
    /// `fn name(params) { body }` or `fn(params) => expr`
    Fn {
        attrs: Attrs,
        mods: Modifiers,
        sig: FnSig,
        body: ExprId,
    },
    /// `val x := 1` or `var x := 1`
    Bind {
        mods: Modifiers,
        mutable: bool,
        pat: PatId,
        ty: OptTyExprId,
        init: ExprId,
    },
    /// `f(args)`
    Call {
        callee: ExprId,
        args: ExprIds,
    },
    /// `arr[i]`
    Index {
        base: ExprId,
        index: ExprId,
    },
    /// `obj.field`
    Field {
        base: ExprId,
        field: Ident,
    },
    /// `expr.^`
    Deref(ExprId),
    /// `-x`, `not x`, `~x`, `@x`
    Unary {
        op: TokenKind,
        operand: ExprId,
    },
    /// `a + b`, `a and b`
    Binary {
        op: TokenKind,
        lhs: ExprId,
        rhs: ExprId,
    },
    /// `a..b`, `a..<b`
    Range {
        start: ExprId,
        end: OptExprId,
        inclusive: bool,
    },
    /// `x <- y`
    Assign {
        target: ExprId,
        value: ExprId,
    },
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
    pub ty: OptTyExprId,
    pub init: OptExprId,
}

/// `name[T](params): RetType`
#[derive(Debug, Clone)]
pub struct FnSig {
    pub name: OptIdent,
    pub ty_params: Idents,
    pub params: Fields,
    pub ret: OptTyExprId,
}

/// `case pat if guard => body`
#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pat: PatId,
    pub guard: OptExprId,
    pub body: ExprId,
}

/// `case Name[T](fields)`
#[derive(Debug, Clone)]
pub struct ChoiceCase {
    pub name: Ident,
    pub ty_args: TyExprIds,
    pub fields: ChoiceCaseItems,
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
    pub args: AttrArgs,
}

/// `name := value` or literal
#[derive(Debug, Clone)]
pub struct AttrArg {
    pub name: OptIdent,
    pub value: OptExprId,
    pub lit: Option<LitKind>,
}

#[derive(Debug, Clone, Default)]
pub struct Modifiers {
    pub exportness: bool,
    pub externness: (OptIdent, bool),
    pub unsafeness: bool,
}

#[derive(Debug)]
pub struct Prog {
    pub stmts: StmtIds,
}
