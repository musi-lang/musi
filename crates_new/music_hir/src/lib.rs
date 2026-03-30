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
    HirArg, HirArrayItem, HirBinaryOp, HirCallableName, HirCaseArm, HirChainKind, HirConstraint,
    HirConstraintKind, HirDeclMods, HirEffectItem, HirEffectSet, HirExpr, HirExprId, HirExprKind,
    HirFStringPart, HirFieldDef, HirHandleClause, HirLit, HirLitKind, HirMemberDef, HirMemberKey,
    HirParam, HirPrefixOp, HirRecordItem, HirRecordPatField, HirSplice, HirSpliceId, HirSpliceKind,
    HirTyParam, HirVariantDef,
};
pub use module::{HirModule, HirStore};
pub use origin::HirOrigin;
pub use pat::{HirPat, HirPatId, HirPatKind};
pub use ty::{HirArrowFlavor, HirDim, HirTy, HirTyBinOp, HirTyId, HirTyKind};
