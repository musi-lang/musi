mod api;
mod diag;

pub use api::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCallable, IrCasePattern, IrCaseRecordField, IrDataDef,
    IrDataVariantDef, IrDiagList, IrExpr, IrExprKind, IrForeignDef, IrGlobal, IrIntrinsicKind,
    IrLit, IrMatchArm, IrMetaRecord, IrModule, IrModuleInitPart, IrModuleParts, IrNameRef,
    IrOrigin, IrParam, IrRangeEndpoint, IrRangeKind, IrRecordField, IrRecordLayoutField, IrSeqPart,
    IrShapeDef, IrStaticImport, IrTempId, ir_diag_kind, lower_surface_type_term,
};
pub use diag::IrDiagKind;
pub use music_sema::DefinitionKey;
