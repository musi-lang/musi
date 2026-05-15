mod defs;
mod diagnostics;
mod expr;
mod module;
mod surface_type_terms;

pub use defs::{
    IrCallable, IrDataDef, IrDataVariantDef, IrForeignDef, IrGlobal, IrMetaRecord,
    IrModuleInitPart, IrShapeDef,
};
pub use diagnostics::{IrDiagList, ir_diag_kind};
pub use expr::{
    IrArg, IrAssignTarget, IrBinaryOp, IrCasePattern, IrCaseRecordField, IrExpr, IrExprKind,
    IrIntrinsicKind, IrLit, IrMatchArm, IrNameRef, IrOrigin, IrParam, IrRangeEndpoint, IrRangeKind,
    IrRecordField, IrRecordLayoutField, IrSeqPart, IrTempId,
};
pub use module::{IrModule, IrModuleParts, IrStaticImport};
pub use surface_type_terms::lower_surface_type_term;
