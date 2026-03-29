//! Typed high-level IR (HIR) data model.
//!
//! This crate defines the semantic output representation used by later lowering
//! stages. It does not implement name resolution or semantic analysis.

pub mod attr;
pub mod boxed;
pub mod expr;
pub mod module;
pub mod origin;
pub mod pat;
pub mod ty;

pub use attr::{HirAttr, HirAttrArg, HirAttrArgKind, HirAttrId, HirAttrPath, HirStringLit};
pub use boxed::{
    HirAttrIds, HirConstraints, HirExprIds, HirIdents, HirMemberDefs, HirParams, HirPatIds,
    HirRecordItems, HirTyIds, HirTypeParams,
};
pub use expr::{
    HirArg, HirArrayItem, HirCallableName, HirCaseArm, HirChainKind, HirConstraint,
    HirConstraintKind, HirDeclMods, HirEffectItem, HirEffectSet, HirExpr, HirExprId, HirExprKind,
    HirFStringPart, HirFieldDef, HirHandleClause, HirLit, HirLitKind, HirMemberDef, HirMemberKey,
    HirParam, HirRecordItem, HirRecordPatField, HirSplice, HirSpliceId, HirSpliceKind,
    HirTypeParam, HirVariantDef,
};
pub use module::{HirModule, HirStore};
pub use origin::HirOrigin;
pub use pat::{HirPat, HirPatId, HirPatKind};
pub use ty::{HirDim, HirTy, HirTyBinOp, HirTyId, HirTyKind};
