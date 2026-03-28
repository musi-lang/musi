use music_shared::{Ident, Literal, Symbol};

use crate::common::{Constraint, MemberDecl, RecordDefField, Signature, TyRef, VariantDef};
use crate::{AttrList, ExprId, ExprList, IdentList, ParamList, PatId, TyId};

use super::common::ModifierSet;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    // Lit = creates a value
    Lit(Literal),
    VariantLit(Ident, ExprList),
    TupleLit(ExprList),
    ArrayLit(ExprList),
    MatrixLit(Vec<ExprList>),
    RecordLit(Vec<RecordField>),
    RecordUpdate {
        base: ExprId,
        fields: Vec<RecordField>,
    },
    FStrLit(Vec<FStrPart>),

    // Def = defines a type
    DataDef(Box<DataBody>),
    EffectDef(Vec<MemberDecl>),
    ClassDef(Box<ClassDefData>),
    InstanceDef(Box<InstanceDef>),

    // Operations
    Var(Ident),
    App(ExprId, ExprList),
    BinOp(BinOp, ExprId, ExprId),
    UnaryOp(UnaryOp, ExprId),
    Access {
        expr: ExprId,
        field: FieldTarget,
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
    Comprehension(Box<ComprehensionData>),

    // Binding
    Let(Box<LetBinding>),
    Assign(ExprId, ExprId),
    Lambda {
        params: ParamList,
        ret_ty: Option<TyId>,
        body: ExprId,
    },

    // Control
    Case(Box<CaseData>),
    Return(Option<ExprId>),
    Resume(Option<ExprId>),

    // Module
    Import {
        path: Symbol,
        kind: ImportKind,
    },
    ForeignImport(Symbol),

    // Effects
    Perform(ExprId),
    Handle(Box<HandleData>),

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
    Shl,
    Shr,
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
pub enum FieldTarget {
    Name(Ident),
    Index(u32),
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
    pub modifiers: ModifierSet,
    pub attrs: AttrList,
    pub pat: PatId,
    pub sig: Option<Box<Signature>>,
    pub value: Option<ExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordField {
    Named { name: Ident, value: Option<ExprId> },
    Spread(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DataBody {
    Product(Vec<RecordDefField>),
    Sum(Vec<VariantDef>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassDefData {
    pub constraints: Vec<Constraint>,
    pub members: Vec<MemberDecl>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HandleData {
    pub effect: TyRef,
    pub body: ExprId,
    pub clauses: Vec<HandlerClause>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HandlerClause {
    Return {
        binder: Ident,
        body: ExprId,
    },
    Op {
        name: Ident,
        args: IdentList,
        cont: Ident,
        body: ExprId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseData {
    pub scrutinee: ExprId,
    pub arms: Vec<CaseArm>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ComprehensionData {
    pub expr: ExprId,
    pub clauses: Vec<CompClause>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseArm {
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
pub struct InstanceDef {
    pub attrs: AttrList,
    pub exported: bool,
    pub ty_params: IdentList,
    pub constraints: Vec<Constraint>,
    pub ty: TyRef,
    pub body: InstanceBody,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstanceBody {
    Methods(Vec<MemberDecl>),
    Via(TyRef),
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
