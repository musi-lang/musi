mod api;
mod diag;
mod lower;

pub use api::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCasePattern, IrCaseRecordField, IrDataDef,
    IrDataVariantDef, IrDiagList, IrEffectDef, IrExpr, IrExprKind, IrForeignDef, IrGivenDef,
    IrGlobal, IrHandleOp, IrIntrinsicKind, IrLit, IrMatchArm, IrModule, IrModuleInitPart,
    IrNameRef, IrOrigin, IrParam, IrRangeEndpoint, IrRangeKind, IrRecordField, IrRecordLayoutField,
    IrSeqPart, IrShapeDef, IrTempId, ir_diag_kind, lower_surface_type_term,
};
pub use diag::IrDiagKind;
pub use lower::lower_module;
pub use music_sema::DefinitionKey;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
