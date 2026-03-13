//! Expression nodes — the core of the AST.

#[cfg(test)]
mod tests;

use music_shared::{Span, Symbol};

use crate::attr::Attr;
use crate::decl::{ClassMember, EffectOp, ExportItem, ForeignDecl};
use crate::lit::Lit;
use crate::ty::{Constraint, EffectSet, TyNamedRef, TyParam};
use crate::{ExprIdx, ExprList, PatIdx, TyIdx};

/// Expression node. All recursive children use arena indices.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // -- literals & names ----------------------------------------------------
    Lit {
        lit: Lit,
        span: Span,
    },
    Name {
        name: Symbol,
        span: Span,
    },

    // -- grouping ------------------------------------------------------------
    Paren {
        inner: ExprIdx,
        span: Span,
    },
    Tuple {
        elems: ExprList,
        span: Span,
    },
    Block {
        stmts: ExprList,
        tail: Option<ExprIdx>,
        span: Span,
    },

    // -- bindings ------------------------------------------------------------
    Let {
        fields: LetFields,
        body: Option<ExprIdx>,
        span: Span,
    },

    // -- functions -----------------------------------------------------------
    Fn {
        params: Vec<Param>,
        ret_ty: Option<TyIdx>,
        body: ExprIdx,
        span: Span,
    },
    Call {
        callee: ExprIdx,
        args: Vec<Arg>,
        span: Span,
    },

    // -- access & update -----------------------------------------------------
    Field {
        object: ExprIdx,
        field: FieldKey,
        safe: bool,
        span: Span,
    },
    Index {
        object: ExprIdx,
        index: ExprIdx,
        span: Span,
    },
    Update {
        base: ExprIdx,
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
        elems: Vec<ArrayElem>,
        span: Span,
    },
    Variant {
        name: Symbol,
        args: ExprList,
        span: Span,
    },
    Choice {
        body: TyIdx,
        span: Span,
    },
    RecordDef {
        fields: Vec<RecDefField>,
        span: Span,
    },

    // -- operators -----------------------------------------------------------
    BinOp {
        op: BinOp,
        left: ExprIdx,
        right: ExprIdx,
        span: Span,
    },
    UnaryOp {
        op: UnaryOp,
        operand: ExprIdx,
        span: Span,
    },

    // -- conditionals --------------------------------------------------------
    Piecewise {
        arms: Vec<PwArm>,
        span: Span,
    },
    Match {
        scrutinee: ExprIdx,
        arms: Vec<MatchArm>,
        span: Span,
    },

    // -- control flow --------------------------------------------------------
    Return {
        value: Option<ExprIdx>,
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
        inner: ExprIdx,
        span: Span,
    },

    // -- declarations --------------------------------------------------------
    Binding {
        exported: bool,
        fields: LetFields,
        span: Span,
    },
    Class {
        exported: bool,
        name: Symbol,
        params: Vec<TyParam>,
        constraints: Vec<Constraint>,
        members: Vec<ClassMember>,
        span: Span,
    },
    Instance {
        exported: bool,
        target: TyNamedRef,
        params: Vec<TyParam>,
        constraints: Vec<Constraint>,
        members: Vec<ClassMember>,
        span: Span,
    },
    Effect {
        exported: bool,
        name: Symbol,
        params: Vec<TyParam>,
        ops: Vec<EffectOp>,
        span: Span,
    },
    Foreign {
        exported: bool,
        abi: Symbol,
        decls: Vec<ForeignDecl>,
        span: Span,
    },

    // -- type test / cast ----------------------------------------------------
    TypeCheck {
        kind: TypeCheckKind,
        operand: ExprIdx,
        ty: TyIdx,
        binding: Option<Symbol>,
        span: Span,
    },

    // -- effects -------------------------------------------------------------
    Handle {
        effect_ty: TyIdx,
        ops: Vec<HandlerOp>,
        body: ExprIdx,
        span: Span,
    },

    // -- error recovery ------------------------------------------------------
    Error {
        span: Span,
    },
}

/// A single operation handler inside a `handle` expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandlerOp {
    pub name: Symbol,
    pub params: Vec<Param>,
    pub body: ExprIdx,
    pub span: Span,
}

/// Shared fields for let-bindings (`Let`, `Binding`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetFields {
    pub kind: BindKind,
    pub pat: PatIdx,
    pub params: Vec<TyParam>,
    pub constraints: Vec<Constraint>,
    pub ty: Option<TyIdx>,
    pub value: Option<ExprIdx>,
    pub with_effects: Option<EffectSet>,
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
    Mut,
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
    pub ty: Option<TyIdx>,
    pub default: Option<ExprIdx>,
    pub span: Span,
}

/// A function argument.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Arg {
    Pos { expr: ExprIdx, span: Span },
    Spread { expr: ExprIdx, span: Span },
}

/// A field in a record expression or update.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecField {
    Named {
        name: Symbol,
        value: Option<ExprIdx>,
        span: Span,
    },
    Spread {
        expr: ExprIdx,
        span: Span,
    },
}

/// A field in a record type definition (`record { name: Ty }`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecDefField {
    pub name: Symbol,
    pub ty: TyIdx,
    pub default: Option<ExprIdx>,
    pub span: Span,
}

/// An element in an array literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArrayElem {
    Elem { expr: ExprIdx, span: Span },
    Spread { expr: ExprIdx, span: Span },
}

/// A field access key.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FieldKey {
    Name { name: Symbol, span: Span },
    Pos { index: u32, span: Span },
}

/// An arm in a piecewise expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PwArm {
    pub result: ExprIdx,
    pub guard: PwGuard,
    pub span: Span,
}

/// Guard of a piecewise arm.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PwGuard {
    Any { span: Span },
    When { expr: ExprIdx, span: Span },
}

/// An arm in a match expression.
#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub attrs: Vec<Attr>,
    pub pat: PatIdx,
    pub guard: Option<ExprIdx>,
    pub result: ExprIdx,
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
    // membership
    In,
    // special infix
    Pipe,
    Assign,
    RangeInc,
    RangeExc,
    Cons,
    NilCoal,
    ForceCoal,
}

/// Unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
    Defer,
    Try,
    Do,
    Propagate,
    ForceUnwrap,
}

/// Discriminator for type-check operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeCheckKind {
    Test,
    Cast,
}
