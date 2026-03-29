use music_ast::{SyntaxNodeId, SyntaxTokenId};
use music_basic::Span;
use music_names::Ident;
use music_storage::Idx;

use super::*;

pub type HirExprId = Idx<HirExpr>;
pub type HirSpliceId = Idx<HirSplice>;

#[derive(Debug, Clone, PartialEq)]
pub struct HirExpr {
    pub origin: HirOrigin,
    pub ty: HirTyId,
    pub kind: HirExprKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirExprKind {
    Sequence {
        exprs: HirExprIds,
        yields_unit: bool,
    },
    Let {
        mods: HirDeclMods,
        pat: HirPatId,
        params: HirParams,
        type_params: HirTypeParams,
        where_: HirConstraints,
        effects: Option<HirEffectSet>,
        annot: Option<HirTyId>,
        value: Option<HirExprId>,
    },
    Import {
        path: HirStringLit,
    },
    ForeignBlock {
        abi: Option<HirStringLit>,
        items: HirExprIds,
    },
    Data {
        variants: Option<Box<[HirVariantDef]>>,
        fields: Option<Box<[HirFieldDef]>>,
    },
    Effect {
        members: HirMemberDefs,
    },
    Class {
        where_: HirConstraints,
        members: HirMemberDefs,
    },
    Instance {
        mods: HirDeclMods,
        type_params: HirTypeParams,
        where_: HirConstraints,
        target: HirTyId,
        members: HirMemberDefs,
    },
    Name {
        ident: Ident,
    },
    Lit {
        lit: HirLit,
    },
    Tuple {
        items: HirExprIds,
    },
    Array {
        items: Box<[HirArrayItem]>,
    },
    Record {
        items: HirRecordItems,
    },
    Variant {
        name: Ident,
        payload: Option<HirExprId>,
    },
    Lambda {
        params: HirParams,
        ret: Option<HirTyId>,
        body: HirExprId,
    },
    Call {
        callee: HirExprId,
        args: Box<[HirArg]>,
    },
    Member {
        base: HirExprId,
        chain: HirChainKind,
        key: HirMemberKey,
    },
    Index {
        base: HirExprId,
        indices: HirExprIds,
    },
    RecordUpdate {
        base: HirExprId,
        items: HirRecordItems,
    },
    TypeTest {
        expr: HirExprId,
        ty: HirTyId,
        alias: Option<Ident>,
    },
    TypeCast {
        expr: HirExprId,
        ty: HirTyId,
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
    Case {
        scrut: HirExprId,
        arms: Box<[HirCaseArm]>,
    },
    Perform {
        expr: HirExprId,
    },
    Handle {
        expr: HirExprId,
        handler: Ident,
        clauses: Box<[HirHandleClause]>,
    },
    Resume {
        value: Option<HirExprId>,
    },
    Quote {
        body_syntax: SyntaxNodeId,
    },
    Splice {
        splice: HirSpliceId,
    },
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirPrefixOp {
    Negate,
    Not,
    Mut,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirBinaryOp {
    Assign,
    Pipe,
    Or,
    Xor,
    And,
    Eq,
    NotEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Symbolic(Ident),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HirChainKind {
    Normal,
    Optional,
    Forced,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HirMemberKey {
    Name(Ident),
    IntLit {
        span: Span,
        syntax: Option<SyntaxTokenId>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirDeclMods {
    pub attrs: HirAttrIds,
    pub exported: bool,
    pub opaque: bool,
    pub external_abi: Option<HirStringLit>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirParam {
    pub origin: HirOrigin,
    pub mutable: bool,
    pub name: Ident,
    pub annot: Option<HirTyId>,
    pub default: Option<HirExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirTypeParam {
    pub origin: HirOrigin,
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirConstraint {
    pub origin: HirOrigin,
    pub kind: HirConstraintKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirConstraintKind {
    Subtype { name: Ident, bound: HirTyId },
    Implements { name: Ident, class: HirTyId },
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirEffectSet {
    pub origin: HirOrigin,
    pub items: Box<[HirEffectItem]>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirEffectItem {
    pub origin: HirOrigin,
    pub name: Ident,
    pub arg: Option<HirTyId>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirArg {
    Expr(HirExprId),
    Spread { origin: HirOrigin, expr: HirExprId },
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirArrayItem {
    Expr(HirExprId),
    Spread { origin: HirOrigin, expr: HirExprId },
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirRecordItem {
    Field {
        origin: HirOrigin,
        name: Ident,
        value: Option<HirExprId>,
    },
    Spread {
        origin: HirOrigin,
        expr: HirExprId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirRecordPatField {
    pub origin: HirOrigin,
    pub mutable: bool,
    pub name: Ident,
    pub sub: Option<HirPatId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirCaseArm {
    pub origin: HirOrigin,
    pub attrs: HirAttrIds,
    pub pat: HirPatId,
    pub guard: Option<HirExprId>,
    pub body: HirExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HirCallableName {
    pub name: Ident,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirMemberDef {
    Let {
        origin: HirOrigin,
        attrs: HirAttrIds,
        name: HirCallableName,
        params: HirParams,
        ret: Option<HirTyId>,
        value: Option<HirExprId>,
    },
    Law {
        origin: HirOrigin,
        attrs: HirAttrIds,
        name: Ident,
        params: HirParams,
        value: HirExprId,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirVariantDef {
    pub origin: HirOrigin,
    pub attrs: HirAttrIds,
    pub name: Ident,
    pub payload_ty: Option<HirTyId>,
    pub value: Option<HirExprId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirFieldDef {
    pub origin: HirOrigin,
    pub name: Ident,
    pub ty: HirTyId,
    pub value: Option<HirExprId>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirHandleClause {
    pub origin: HirOrigin,
    pub name: Ident,
    pub params: HirIdents,
    pub body: HirExprId,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirSplice {
    pub origin: HirOrigin,
    pub kind: HirSpliceKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirSpliceKind {
    Name(Ident),
    Expr(HirExprId),
    ExprArray(HirExprIds),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HirLit {
    pub kind: HirLitKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirLitKind {
    Int {
        span: Span,
        syntax: Option<SyntaxTokenId>,
    },
    Float {
        span: Span,
        syntax: Option<SyntaxTokenId>,
    },
    Rune {
        span: Span,
        syntax: Option<SyntaxTokenId>,
    },
    String(HirStringLit),
    FString {
        span: Span,
        syntax: Option<SyntaxTokenId>,
        parts: Box<[HirFStringPart]>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum HirFStringPart {
    Literal { span: Span },
    Expr { origin: HirOrigin, expr: HirExprId },
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
