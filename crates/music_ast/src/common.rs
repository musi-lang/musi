use music_shared::{Ident, Symbol};

use crate::{AttrList, ExprId, IdentList, ParamList, TyId, TyList};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ModifierSet {
    pub exported: bool,
    pub opaque: bool,
    pub mutable: bool,
    pub foreign_abi: Option<Symbol>,
    pub foreign_alias: Option<Symbol>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Signature {
    pub params: ParamList,
    pub ty_params: IdentList,
    pub constraints: Vec<Constraint>,
    pub effects: Vec<EffectItem>,
    pub ret_ty: Option<TyId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Param {
    pub mutable: bool,
    pub name: Ident,
    pub ty: Option<TyId>,
    pub default: Option<ExprId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    pub path: IdentList,
    pub args: Vec<AttrArg>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttrArg {
    Named { name: Ident, value: ExprId },
    Positional(ExprId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TyRef {
    pub name: Ident,
    pub args: TyList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    Subtype { ty: Ident, bound: TyRef },
    Implements { ty: Ident, class: TyRef },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EffectItem {
    Named { name: Ident, arg: Option<TyId> },
    Rest(Ident),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberDecl {
    Fn(FnDecl),
    Law(LawDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FnDecl {
    pub attrs: AttrList,
    pub name: MemberName,
    pub params: Option<ParamList>,
    pub ret_ty: Option<TyId>,
    pub body: Option<ExprId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum OpFixity {
    /// `(_ + _)` — both operand placeholders present.
    Infix,
    /// `(- _)` — right operand placeholder only.
    Prefix,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MemberName {
    Ident(Ident),
    Op(Ident, OpFixity),
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
    pub attrs: AttrList,
    pub name: Ident,
    pub payload: Option<TyId>,
    pub default: Option<ExprId>,
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
