use music_arena::SliceRange;
use music_names::Ident;

use crate::module::{HirExprId, HirLitId, HirPatId};
use crate::origin::HirOrigin;
use crate::ty::HirDim;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HirBinder {
    pub name: Ident,
    pub ty: Option<HirExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirExpr {
    pub origin: HirOrigin,
    pub kind: HirExprKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirExprKind {
    Error,
    Name {
        name: Ident,
    },
    Lit {
        lit: HirLitId,
    },
    Template {
        parts: SliceRange<HirTemplatePart>,
    },

    Sequence {
        exprs: SliceRange<HirExprId>,
    },
    Tuple {
        items: SliceRange<HirExprId>,
    },
    Array {
        items: SliceRange<HirArrayItem>,
    },
    ArrayTy {
        dims: SliceRange<HirDim>,
        item: HirExprId,
    },
    Record {
        items: SliceRange<HirRecordItem>,
    },
    Variant {
        tag: Ident,
        args: SliceRange<HirExprId>,
    },

    Pi {
        binder: Ident,
        binder_ty: HirExprId,
        ret: HirExprId,
        is_effectful: bool,
    },
    Lambda {
        params: SliceRange<HirParam>,
        ret_ty: Option<HirExprId>,
        body: HirExprId,
    },

    Call {
        callee: HirExprId,
        args: SliceRange<HirArg>,
    },
    Apply {
        callee: HirExprId,
        args: SliceRange<HirExprId>,
    },
    Index {
        base: HirExprId,
        args: SliceRange<HirExprId>,
    },
    Field {
        base: HirExprId,
        access: HirAccessKind,
        name: Ident,
    },
    RecordUpdate {
        base: HirExprId,
        items: SliceRange<HirRecordItem>,
    },
    TypeTest {
        base: HirExprId,
        ty: HirExprId,
        as_name: Option<Ident>,
    },
    TypeCast {
        base: HirExprId,
        ty: HirExprId,
    },

    Prefix {
        op: HirPrefixOp,
        expr: HirExprId,
    },
    Binary {
        op: HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    },

    Let {
        mods: HirLetMods,
        pat: HirPatId,
        type_params: SliceRange<HirBinder>,
        has_param_clause: bool,
        params: SliceRange<HirParam>,
        constraints: SliceRange<HirConstraint>,
        effects: Option<HirEffectSet>,
        sig: Option<HirExprId>,
        value: HirExprId,
    },
    Import {
        arg: HirExprId,
    },
    Export {
        opaque: bool,
        foreign_abi: Option<Box<str>>,
        expr: HirExprId,
    },

    Case {
        scrutinee: HirExprId,
        arms: SliceRange<HirCaseArm>,
    },
    Data {
        variants: SliceRange<HirVariantDef>,
        fields: SliceRange<HirFieldDef>,
    },
    Effect {
        members: SliceRange<HirMemberDef>,
    },
    Class {
        constraints: SliceRange<HirConstraint>,
        members: SliceRange<HirMemberDef>,
    },
    Instance {
        type_params: SliceRange<HirBinder>,
        constraints: SliceRange<HirConstraint>,
        class: HirExprId,
        members: SliceRange<HirMemberDef>,
    },
    Foreign {
        abi: Option<Box<str>>,
        decls: SliceRange<HirForeignDecl>,
    },
    Perform {
        expr: HirExprId,
    },
    Handle {
        expr: HirExprId,
        handler: Ident,
        clauses: SliceRange<HirHandleClause>,
    },
    Resume {
        expr: Option<HirExprId>,
    },

    Quote {
        kind: HirQuoteKind,
    },
    Splice {
        kind: HirSpliceKind,
    },
    Attributed {
        attrs: SliceRange<HirAttr>,
        expr: HirExprId,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirAccessKind {
    Direct,
    Optional,
    Unwrap,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirPrefixOp {
    Neg,
    Not,
    Mut,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirBinaryOp {
    Assign,
    Pipe,
    Arrow,
    EffectArrow,
    Or,
    Xor,
    And,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
    In,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    UserOp(Ident),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirArg {
    pub spread: bool,
    pub expr: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirParam {
    pub name: Ident,
    pub ty: Option<HirExprId>,
    pub default: Option<HirExprId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HirLetMods {
    pub is_rec: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirArrayItem {
    pub spread: bool,
    pub expr: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirRecordItem {
    pub spread: bool,
    pub name: Option<Ident>,
    pub value: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirTemplatePart {
    Text { value: Box<str> },
    Expr { expr: HirExprId },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAttr {
    pub origin: HirOrigin,
    pub path: SliceRange<Ident>,
    pub args: SliceRange<HirAttrArg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirAttrArg {
    pub name: Option<Ident>,
    pub value: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirCaseArm {
    pub attrs: SliceRange<HirAttr>,
    pub pat: HirPatId,
    pub guard: Option<HirExprId>,
    pub expr: HirExprId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirConstraintKind {
    Subtype,
    Implements,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirConstraint {
    pub name: Ident,
    pub kind: HirConstraintKind,
    pub value: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirEffectSet {
    pub items: SliceRange<HirEffectItem>,
    pub open: Option<Ident>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirEffectItem {
    pub name: Ident,
    pub arg: Option<HirExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirMemberKind {
    Let,
    Law,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirMemberDef {
    pub origin: HirOrigin,
    pub attrs: SliceRange<HirAttr>,
    pub kind: HirMemberKind,
    pub name: Ident,
    pub params: SliceRange<HirParam>,
    pub sig: Option<HirExprId>,
    pub value: Option<HirExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirVariantDef {
    pub origin: HirOrigin,
    pub attrs: SliceRange<HirAttr>,
    pub name: Ident,
    pub arg: Option<HirExprId>,
    pub value: Option<HirExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirFieldDef {
    pub origin: HirOrigin,
    pub attrs: SliceRange<HirAttr>,
    pub name: Ident,
    pub ty: HirExprId,
    pub value: Option<HirExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirForeignDecl {
    pub origin: HirOrigin,
    pub attrs: SliceRange<HirAttr>,
    pub name: Ident,
    pub type_params: SliceRange<HirBinder>,
    pub params: SliceRange<HirParam>,
    pub constraints: SliceRange<HirConstraint>,
    pub sig: Option<HirExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirHandleClause {
    pub op: Ident,
    pub params: SliceRange<Ident>,
    pub body: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirQuoteKind {
    Expr { expr: HirExprId },
    Block { exprs: SliceRange<HirExprId> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirSpliceKind {
    Name { name: Ident },
    Expr { expr: HirExprId },
    Exprs { exprs: SliceRange<HirExprId> },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirLit {
    pub origin: HirOrigin,
    pub kind: HirLitKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirLitKind {
    Int { raw: Box<str> },
    Float { raw: Box<str> },
    String { value: Box<str> },
    Rune { value: u32 },
}
