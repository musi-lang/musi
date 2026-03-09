//! Expression nodes — the core of the AST.

#[cfg(test)]
mod tests;

use music_shared::{Idx, Span, Symbol};

use crate::attr::Attr;
use crate::decl::{ClassMember, EffectOp, ExportItem};
use crate::lit::Lit;
use crate::pat::Pat;
use crate::ty::{Constraint, Quantifier, Ty, TyParam};
use crate::{ExprList, TyList};

/// Expression node. All recursive children use arena indices.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // -- literals & names ----------------------------------------------------
    Lit {
        lit: Lit,
        span: Span,
    },
    Name {
        ident: Symbol,
        span: Span,
    },

    // -- grouping ------------------------------------------------------------
    Paren {
        inner: Idx<Self>,
        span: Span,
    },
    Tuple {
        elems: ExprList,
        span: Span,
    },
    Block {
        stmts: ExprList,
        tail: Option<Idx<Self>>,
        span: Span,
    },

    // -- bindings ------------------------------------------------------------
    Let {
        fields: LetFields,
        body: Option<Idx<Self>>,
        span: Span,
    },

    // -- functions -----------------------------------------------------------
    Fn {
        params: Vec<Param>,
        arrow: Arrow,
        ret_ty: Option<Idx<Ty>>,
        body: Idx<Self>,
        span: Span,
    },
    Call {
        callee: Idx<Self>,
        args: Vec<Arg>,
        span: Span,
    },

    // -- access & update -----------------------------------------------------
    Field {
        object: Idx<Self>,
        field: FieldKey,
        safe: bool,
        span: Span,
    },
    Index {
        object: Idx<Self>,
        index: Idx<Self>,
        span: Span,
    },
    Update {
        base: Idx<Self>,
        fields: Vec<RecField>,
        span: Span,
    },

    // -- constructors --------------------------------------------------------
    Record {
        ty_name: Option<Symbol>,
        fields: Vec<RecField>,
        span: Span,
    },
    Array {
        items: Vec<ArrayItem>,
        span: Span,
    },
    Variant {
        name: Symbol,
        args: ExprList,
        span: Span,
    },

    // -- operators -----------------------------------------------------------
    BinOp {
        op: BinOp,
        left: Idx<Self>,
        right: Idx<Self>,
        span: Span,
    },
    UnaryOp {
        op: UnaryOp,
        operand: Idx<Self>,
        span: Span,
    },

    // -- conditionals --------------------------------------------------------
    Piecewise {
        arms: Vec<PwArm>,
        span: Span,
    },
    Match {
        scrutinee: Idx<Self>,
        arms: Vec<MatchArm>,
        span: Span,
    },

    // -- control flow --------------------------------------------------------
    Return {
        value: Option<Idx<Self>>,
        span: Span,
    },

    // -- quantification ------------------------------------------------------
    Quantified {
        kind: Quantifier,
        params: Vec<TyParam>,
        constraints: Vec<Constraint>,
        body: Idx<Self>,
        span: Span,
    },

    // -- module --------------------------------------------------------------
    Import {
        path: Symbol,
        span: Span,
    },
    Export {
        items: Vec<ExportItem>,
        source: Option<Symbol>,
        span: Span,
    },
    Annotated {
        attrs: Vec<Attr>,
        inner: Idx<Self>,
        span: Span,
    },

    // -- declarations --------------------------------------------------------
    Binding {
        exported: bool,
        fields: LetFields,
        span: Span,
    },
    Class {
        name: Symbol,
        params: Vec<TyParam>,
        constraints: Vec<Constraint>,
        members: Vec<ClassMember>,
        span: Span,
    },
    Given {
        target: TyNamed,
        constraints: Vec<Constraint>,
        members: Vec<ClassMember>,
        span: Span,
    },
    Effect {
        name: Symbol,
        params: Vec<TyParam>,
        ops: Vec<EffectOp>,
        span: Span,
    },

    // -- error recovery ------------------------------------------------------
    Error {
        span: Span,
    },
}

/// Shared fields for let-bindings (`Let`, `Binding`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetFields {
    pub kind: BindKind,
    pub heap: bool,
    pub pat: Idx<Pat>,
    pub ty: Option<Idx<Ty>>,
    pub value: Idx<Expr>,
    pub span: Span,
}

/// Whether a binding is immutable (`let`) or mutable (`var`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BindKind {
    Immut,
    Mut,
}

/// Function parameter mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ParamMode {
    Plain,
    Var,
    Inout,
    Ref,
}

/// Function arrow kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Arrow {
    Pure,
    Effectful,
}

/// A function parameter.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub mode: ParamMode,
    pub name: Symbol,
    pub ty: Option<Idx<Ty>>,
    pub default: Option<Idx<Expr>>,
    pub span: Span,
}

/// A function argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Arg {
    Pos { expr: Idx<Expr>, span: Span },
    Hole { span: Span },
}

/// A field in a record expression or update.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecField {
    Named {
        name: Symbol,
        value: Option<Idx<Expr>>,
        span: Span,
    },
    Spread {
        expr: Idx<Expr>,
        span: Span,
    },
}

/// An item in an array literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArrayItem {
    Elem { expr: Idx<Expr>, span: Span },
    Spread { expr: Idx<Expr>, span: Span },
}

/// A field access key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FieldKey {
    Name { ident: Symbol, span: Span },
    Pos { index: u32, span: Span },
}

/// An arm in a piecewise expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PwArm {
    pub result: Idx<Expr>,
    pub guard: PwGuard,
    pub span: Span,
}

/// Guard of a piecewise arm.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PwGuard {
    Any { span: Span },
    When { expr: Idx<Expr>, span: Span },
}

/// An arm in a match expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub attrs: Vec<Attr>,
    pub pat: Idx<Pat>,
    pub guard: Option<Idx<Expr>>,
    pub result: Idx<Expr>,
    pub span: Span,
}

/// Binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    // arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // logical / bitwise
    And,
    Or,
    Xor,
    Shl,
    Shr,
    ShrUn,
    // membership
    In,
    // special infix
    Pipe,
    Assign,
    RangeInc,
    RangeExc,
    Cons,
    NilCoal,
}

/// Unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
    Defer,
    Spawn,
    Await,
    Try,
}

/// A named type reference (used in `Given` targets).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyNamed {
    pub name: Symbol,
    pub args: TyList,
    pub span: Span,
}
