use music_found::Ident;

use crate::{AttrList, ExprId, IdentList, ParamList, TyId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub mutable: bool,
    pub name: Ident,
    pub ty: Option<TyId>,
    pub default: Option<ExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    pub name: Ident,
    pub args: Vec<AttrArg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttrArg {
    Named { name: Ident, value: ExprId },
    Positional(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhereClause {
    pub constraints: Vec<Constraint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    Subtype { ty: Ident, bound: TyRef },
    Implements { ty: Ident, class: TyRef },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyRef {
    pub name: Ident,
    pub args: Vec<TyId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectSet {
    pub effects: Vec<EffectItem>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectItem {
    pub name: Ident,
    pub arg: Option<TyId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberKind {
    Fn(FnDecl),
    Law(LawDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub name: MemberName,
    pub params: Option<ParamList>,
    pub ret_ty: Option<TyId>,
    pub body: Option<ExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberName {
    Ident(Ident),
    Op(Ident),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawDecl {
    pub name: Ident,
    pub params: Option<ParamList>,
    pub body: ExprId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordDefField {
    pub name: Ident,
    pub ty: TyId,
    pub default: Option<ExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VariantDef {
    pub name: Ident,
    pub payload: Option<TyId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignBinding {
    pub attrs: AttrList,
    pub name: Ident,
    pub ty_params: Option<IdentList>,
    pub alias: Option<String>,
    pub where_clause: Option<WhereClause>,
    pub ty: Option<TyId>,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
