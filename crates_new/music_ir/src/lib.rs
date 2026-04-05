mod api;
mod lower;

pub use api::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCaseArm, IrCasePattern, IrClassDef, IrDataDef,
    IrDiagList, IrEffectDef, IrExpr, IrExprKind, IrForeignDef, IrGlobal, IrHandleOp, IrInstanceDef,
    IrLit, IrModule, IrNameRef, IrOrigin, IrParam, IrRecordField, IrRecordLayoutField, IrTempId,
};
pub use lower::lower_module;
pub use music_sema::DefinitionKey;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
