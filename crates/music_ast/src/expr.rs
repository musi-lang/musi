use music_found::{Ident, Literal};

use crate::common::{
    EffectSet, FnDecl, ForeignBinding, MemberKind, RecordDefField, TyRef, VariantDef, WhereClause,
};
use crate::{AttrList, ExprId, ExprList, IdentList, ParamList, PatId, TyId};

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    // Lit suffix = creates a value
    Lit(Literal),
    VariantLit(Ident, ExprList),
    TupleLit(ExprList),
    ArrayLit(ExprList),
    MatrixLit(Vec<ExprList>),
    RecordLit(Vec<RecordField>),
    FStrLit(Vec<FStrPart>),

    // Def suffix = defines a type
    RecordDef(Vec<RecordDefField>),
    ChoiceDef(Vec<VariantDef>),
    EffectDef(Vec<MemberKind>),
    ClassDef {
        where_clause: Option<WhereClause>,
        members: Vec<MemberKind>,
    },
    InstanceDef(InstanceExpr),

    // No suffix = operations
    Var(Ident),
    App(ExprId, ExprList),
    BinOp(BinOp, ExprId, ExprId),
    UnaryOp(UnaryOp, ExprId),
    Access {
        expr: ExprId,
        field: Ident,
        mode: AccessMode,
    },
    Index {
        expr: ExprId,
        indices: ExprList,
        kind: IndexKind,
    },
    TypeOp {
        expr: ExprId,
        ty: TyId,
        kind: TypeOpKind,
    },
    Postfix {
        expr: ExprId,
        op: PostfixOp,
    },
    Seq(ExprList),
    Comprehension {
        expr: ExprId,
        clauses: Vec<CompClause>,
    },

    // Binding
    Let(LetBinding),
    Assign(ExprId, ExprId),
    Lambda {
        params: ParamList,
        ret_ty: Option<TyId>,
        body: ExprId,
    },

    // Control
    Match(ExprId, Vec<MatchArm>),
    Return(Option<ExprId>),
    Resume(Option<ExprId>),

    // Module
    Import {
        path: String,
        kind: ImportKind,
    },

    // Effects
    Need(ExprId),
    Handle {
        effect: TyRef,
        handlers: Vec<FnDecl>,
        body: ExprId,
    },

    // FFI
    Foreign(ForeignKind),

    // Metaprogramming
    Quote(QuoteKind),
    Splice(SpliceKind),

    // AST-only (replaced during lowering)
    Piecewise(Vec<PiecewiseArm>),

    // HIR-only (created during lowering)
    Branch {
        cond: ExprId,
        then_br: ExprId,
        else_br: ExprId,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    NilCoalesce,
    PipeRight,
    Or,
    Xor,
    And,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Range,
    RangeExcl,
    Cons,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Not,
    Mut,
    Spread,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AccessMode {
    Direct,
    Optional,
    Forced,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IndexKind {
    Point,
    Slice,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeOpKind {
    Test(Option<Ident>),
    Cast,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PostfixOp {
    Force,
    Propagate,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ImportKind {
    Qualified(Ident),
    Wildcard,
    Selective(Ident, IdentList),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetBinding {
    pub exported: bool,
    pub opaque: bool,
    pub foreign_abi: Option<String>,
    pub mutable: bool,
    pub attrs: AttrList,
    pub pat: PatId,
    pub params: Option<ParamList>,
    pub ty_params: Option<IdentList>,
    pub where_clause: Option<WhereClause>,
    pub with_clause: Option<EffectSet>,
    pub ret_ty: Option<TyId>,
    pub value: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordField {
    Named { name: Ident, value: Option<ExprId> },
    Spread(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm {
    pub attrs: AttrList,
    pub pat: PatId,
    pub guard: Option<ExprId>,
    pub body: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PiecewiseArm {
    pub value: ExprId,
    pub guard: PwGuard,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PwGuard {
    Wildcard,
    Expr(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompClause {
    Generator { pat: PatId, iter: ExprId },
    Filter(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum FStrPart {
    Lit(String),
    Expr(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceExpr {
    pub ty_params: Option<IdentList>,
    pub where_clause: Option<WhereClause>,
    pub ty: TyRef,
    pub body: InstanceBody,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstanceBody {
    Methods(Vec<MemberKind>),
    Via(TyRef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ForeignKind {
    Import(String),
    Binding(ForeignBinding),
    Group(Vec<ForeignBinding>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum QuoteKind {
    Expr(ExprId),
    Block(ExprList),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SpliceKind {
    Ident(Ident),
    Expr(ExprId),
    Array(ExprList),
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
