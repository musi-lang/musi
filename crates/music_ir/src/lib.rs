mod api;
mod diag;
mod lower;

pub use api::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCaseArm, IrCasePattern, IrCaseRecordField,
    IrClassDef, IrDataDef, IrDataVariantDef, IrDiagList, IrEffectDef, IrExpr, IrExprKind,
    IrForeignDef, IrGlobal, IrHandleOp, IrInstanceDef, IrLit, IrModule, IrNameRef, IrOrigin,
    IrParam, IrRangeKind, IrRecordField, IrRecordLayoutField, IrSeqPart, IrTempId, ir_diag_kind,
    lower_surface_type_term,
};
pub use diag::IrDiagKind;
pub use lower::lower_module;
pub use music_sema::DefinitionKey;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
