//! Common `Box<[...]>` shapes used throughout the HIR model.

use music_names::Ident;

use super::*;

pub type HirAttrIds = Box<[HirAttrId]>;
pub type HirConstraints = Box<[HirConstraint]>;
pub type HirExprIds = Box<[HirExprId]>;
pub type HirIdents = Box<[Ident]>;
pub type HirMemberDefs = Box<[HirMemberDef]>;
pub type HirParams = Box<[HirParam]>;
pub type HirPatIds = Box<[HirPatId]>;
pub type HirRecordItems = Box<[HirRecordItem]>;
pub type HirTypeParams = Box<[HirTyParam]>;
pub type HirTyIds = Box<[HirTyId]>;
