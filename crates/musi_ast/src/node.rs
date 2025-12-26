use musi_basic::span::Span;
use musi_lex::token::TokenKind;

use crate::{
    AttrArgList, AttrList, CondPtr, ExprList, ExprPtr, FieldList, Ident, Idents, OptExpr,
    OptExprPtr, OptIdent, OptTyExpr, PatList, StmtList, SumCaseItemList, TyExprList, TyExprPtr,
};

// ============================================================================
// LITERALS
// ============================================================================

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
    Expr(ExprPtr),
}

// ============================================================================
// TYPES
// ============================================================================

#[derive(Debug, Clone)]
pub struct TyExpr {
    pub kind: TyExprKind,
    pub span: Span,
}

impl TyExpr {
    #[must_use]
    pub const fn new(kind: TyExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum TyExprKind {
    /// `Int`, `String`
    Ident(Ident),
    /// `List[Int]`, `Map[String, Int]`
    App { base: Ident, args: TyExprList },
    /// `?Int`
    Optional(TyExprPtr),
    /// `[10]Int`, `[]Int`
    Array { size: Option<i64>, elem: TyExprPtr },
    /// `^Int`
    Ptr(TyExprPtr),
    /// `Int -> String`
    Fn { param: TyExprPtr, ret: TyExprPtr },
    /// `(Int, String)`
    Tuple(TyExprList),
}

// ============================================================================
// PATTERNS
// ============================================================================

#[derive(Debug, Clone)]
pub struct Pat {
    pub kind: PatKind,
    pub span: Span,
}

impl Pat {
    #[must_use]
    pub const fn new(kind: PatKind, span: Span) -> Self {
        Self { kind, span }
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
    Tuple(PatList),
    /// `[a, b, c]`
    Array(PatList),
    /// `Point.{x, y}`
    Record { ty: OptExprPtr, fields: Idents },
    /// `Some(x)`, `None`
    Variant {
        name: Ident,
        ty_args: TyExprList,
        args: PatList,
    },
    /// `head :: tail`
    Cons(PatList),
    /// `a | b`
    Or(PatList),
}

// ============================================================================
// EXPRESSIONS
// ============================================================================

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    #[must_use]
    pub const fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }

    #[must_use]
    pub fn binary(op: TokenKind, lhs: Self, rhs: Self, span: Span) -> Self {
        Self::new(
            ExprKind::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
            span,
        )
    }

    #[must_use]
    pub fn unary(op: TokenKind, operand: Self, span: Span) -> Self {
        Self::new(
            ExprKind::Unary {
                op,
                operand: Box::new(operand),
            },
            span,
        )
    }
}

#[derive(Debug, Clone)]
pub enum Cond {
    /// `cond`
    Expr(Expr),
    /// `case pat := expr, expr, expr, ...`
    Case {
        pat: Pat,
        init: Expr,
        extra: ExprList,
    },
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// `42`, `"hello"`, `true`
    Lit(LitKind),
    /// `x`, `foo`
    Ident(Ident),
    /// `(a, b, c)`
    Tuple(ExprList),
    /// `[a, b, c]`
    Array(ExprList),
    /// `Point.{x := 1, y := 2}`
    Record {
        ty: OptExprPtr,
        fields: FieldList,
    },
    /// Block: `{ stmt; stmt; expr }`
    Block {
        stmts: StmtList,
        expr: OptExprPtr,
    },
    /// `if cond { } else if { } else { }`
    If {
        cond: CondPtr,
        then_br: ExprPtr,
        else_br: OptExprPtr,
    },
    While {
        cond: CondPtr,
        body: ExprPtr,
    },
    /// `for pat in iter { }`
    For {
        pat: Pat,
        iter: ExprPtr,
        body: ExprPtr,
    },
    /// `match expr { cases }`
    Match {
        scrutinee: ExprPtr,
        cases: Vec<MatchCase>,
    },
    /// `return expr`
    Return(OptExprPtr),
    /// `defer expr`
    Defer(ExprPtr),
    /// `break expr`
    Break(OptExprPtr),
    /// `cycle`
    Cycle,
    /// `unsafe { }`
    Unsafe(ExprPtr),
    /// `import "path"`
    Import(Ident),
    /// `extern "C" { fn foo(); }`
    Extern {
        abi: OptIdent,
        fns: Vec<FnSig>,
    },
    /// `record Point { x: Int; y: Int }`
    RecordDef {
        attrs: AttrList,
        mods: Modifiers,
        name: OptIdent,
        ty_params: Idents,
        fields: FieldList,
    },
    /// `sum Option[T] { case Some(T), case None }`
    SumDef {
        attrs: AttrList,
        mods: Modifiers,
        name: OptIdent,
        ty_params: Idents,
        cases: Vec<SumCase>,
    },
    /// `alias Name := Type`
    Alias {
        attrs: AttrList,
        mods: Modifiers,
        name: Ident,
        ty_params: Idents,
        ty: TyExpr,
    },
    /// `fn name(params) { body }`
    Fn {
        attrs: AttrList,
        mods: Modifiers,
        sig: FnSig,
        body: ExprPtr,
    },
    /// `val x := 1` or `var x := 1`
    Bind {
        mods: Modifiers,
        mutable: bool,
        pat: Pat,
        ty: OptTyExpr,
        init: ExprPtr,
    },
    /// `f(args)`
    Call {
        callee: ExprPtr,
        args: ExprList,
    },
    /// `arr[i]`
    Index {
        base: ExprPtr,
        index: ExprPtr,
    },
    /// `obj.field`
    Field {
        base: ExprPtr,
        field: Ident,
    },
    /// `expr.^`
    Deref(ExprPtr),
    /// `-x`, `not x`, `~x`, `@x`
    Unary {
        op: TokenKind,
        operand: ExprPtr,
    },
    /// `a + b`, `a and b`
    Binary {
        op: TokenKind,
        lhs: ExprPtr,
        rhs: ExprPtr,
    },
    /// `a..b`, `a..<b`
    Range {
        start: ExprPtr,
        end: OptExprPtr,
        inclusive: bool,
    },
    /// `x <- y`
    Assign {
        target: ExprPtr,
        value: ExprPtr,
    },
}

// ============================================================================
// STATEMENTS
// ============================================================================

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// `expr;`
    Expr(Expr),
}

// ============================================================================
// SUPPORT TYPES
// ============================================================================

/// `[var] name [: ty] [:= init]`
#[derive(Debug, Clone)]
pub struct Field {
    pub mutable: bool,
    pub name: Ident,
    pub ty: OptTyExpr,
    pub init: OptExpr,
}

/// `name[T](params): RetType`
#[derive(Debug, Clone)]
pub struct FnSig {
    pub name: OptIdent,
    pub ty_params: Idents,
    pub params: FieldList,
    pub ret: OptTyExpr,
}

/// `case pat if guard => body`
#[derive(Debug, Clone)]
pub struct MatchCase {
    pub pat: Pat,
    pub guard: OptExpr,
    pub body: Expr,
}

/// `case Name[T](fields)`
#[derive(Debug, Clone)]
pub struct SumCase {
    pub name: Ident,
    pub ty_args: TyExprList,
    pub fields: SumCaseItemList,
}

#[derive(Debug, Clone)]
pub enum SumCaseItem {
    Type(TyExpr),
    Field(Field),
}

/// `@[Name(args)]`
#[derive(Debug, Clone)]
pub struct Attr {
    pub name: Ident,
    pub args: AttrArgList,
}

/// `name := value` or literal
#[derive(Debug, Clone)]
pub struct AttrArg {
    pub name: OptIdent,
    pub value: OptExpr,
    pub lit: Option<LitKind>,
}

#[derive(Debug, Clone, Default)]
pub struct Modifiers {
    pub exportness: bool,
    pub externness: (OptIdent, bool),
    pub unsafeness: bool,
}

// ============================================================================
// PROGRAM
// ============================================================================

#[derive(Debug, Clone)]
pub struct Prog {
    pub stmts: StmtList,
}
