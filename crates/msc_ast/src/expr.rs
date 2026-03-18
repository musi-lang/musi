//! Expression nodes - the core of the AST.

#[cfg(test)]
mod tests;

use msc_shared::{Span, Symbol};

use crate::attr::Attr;
use crate::decl::{ClassMember, EffectOp, ExportItem, ForeignDecl};
use crate::lit::Lit;
use crate::ty_param::{Constraint, TyParam};
use crate::{ExprIdx, ExprList, NameRefIdx, PatIdx};

/// Expression node. All recursive children use arena indices.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // Literals & names
    Lit {
        lit: Lit,
        span: Span,
    },
    Name {
        name_ref: NameRefIdx,
        span: Span,
    },

    // Grouping
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

    // Bindings
    Let {
        fields: LetFields,
        body: Option<ExprIdx>,
        span: Span,
    },

    // Functions
    Fn {
        params: Vec<Param>,
        ret_ty: Option<ExprIdx>,
        body: ExprIdx,
        span: Span,
    },
    Call {
        callee: ExprIdx,
        args: Vec<Arg>,
        span: Span,
    },

    // Access & update
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

    // Constructors
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
        body: ExprIdx,
        span: Span,
    },
    RecordDef {
        fields: Vec<RecDefField>,
        span: Span,
    },

    // Operators
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

    // Conditionals
    Piecewise {
        arms: Vec<PwArm>,
        span: Span,
    },
    Match {
        scrutinee: ExprIdx,
        arms: Vec<MatchArm>,
        span: Span,
    },

    // Control flow
    Return {
        value: Option<ExprIdx>,
        span: Span,
    },
    /// Effect operation: `need op(args)` — perform an effect operation.
    Need {
        operand: ExprIdx,
        span: Span,
    },
    /// Resume continuation inside a handler: `resume value`.
    Resume {
        value: Option<ExprIdx>,
        span: Span,
    },

    // Module
    Import {
        path: Symbol,
        alias: Option<Symbol>,
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

    // Declarations
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
        target: ExprIdx,
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

    // Type test / cast
    TypeCheck {
        kind: TypeCheckKind,
        operand: ExprIdx,
        ty: ExprIdx,
        binding: Option<Symbol>,
        span: Span,
    },

    // Effects
    Handle {
        effect_ty: ExprIdx,
        ops: Vec<HandlerOp>,
        body: ExprIdx,
        span: Span,
    },

    // Type expressions
    /// Type application: `List of Int`, `Map of (String, Int)`.
    TypeApp {
        callee: ExprIdx,
        args: ExprList,
        span: Span,
    },
    /// Function type: `Int -> String` or `Int ~> String with { IO }`.
    FnType {
        params: ExprList,
        ret: ExprIdx,
        arrow: Arrow,
        effects: Option<EffectSet>,
        span: Span,
    },
    /// Option type sugar: `?Int`.
    OptionType {
        inner: ExprIdx,
        span: Span,
    },
    /// Product type: `Int * String`.
    ProductType {
        fields: ExprList,
        span: Span,
    },
    /// Sum type: `Int + String`.
    SumType {
        variants: ExprList,
        span: Span,
    },
    /// Array type: `[]Int` or `[3]Int`.
    ArrayType {
        len: Option<u32>,
        elem: ExprIdx,
        span: Span,
    },
    /// Dependent function type: `(x : A) -> B` where x may appear in B.
    PiType {
        param: Symbol,
        param_ty: ExprIdx,
        body: ExprIdx,
        span: Span,
    },

    // Error recovery
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
    pub ty: Option<ExprIdx>,
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
    pub ty: Option<ExprIdx>,
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
    pub ty: ExprIdx,
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

impl BinOp {
    /// Returns the operator text for this binary op if it's an overloadable operator.
    #[must_use]
    pub const fn operator_name(self) -> Option<&'static str> {
        match self {
            Self::Add => Some("+"),
            Self::Sub => Some("-"),
            Self::Mul => Some("*"),
            Self::Div => Some("/"),
            Self::Rem => Some("%"),
            Self::Eq => Some("="),
            Self::Ne => Some("/="),
            Self::Lt => Some("<"),
            Self::Gt => Some(">"),
            Self::Le => Some("<="),
            Self::Ge => Some(">="),
            Self::Shl => Some("<<"),
            Self::Shr => Some(">>"),
            Self::Cons => Some("::"),
            Self::Pipe => Some("|>"),
            _ => None,
        }
    }
}

/// Unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
    Defer,
    Try,
    Propagate,
    ForceUnwrap,
}

/// Discriminator for type-check operations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeCheckKind {
    Test,
    Cast,
}

/// A set of effects on a function type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectSet {
    pub effects: Vec<EffectItem>,
    pub span: Span,
}

/// An individual effect in an effect set.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EffectItem {
    Named {
        name: Symbol,
        arg: Option<ExprIdx>,
        span: Span,
    },
    Var {
        name: Symbol,
        span: Span,
    },
}
