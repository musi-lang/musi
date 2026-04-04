mod api;
mod lower;

pub use api::{
    IrArg, IrBinaryOp, IrCallable, IrClassDef, IrDataDef, IrDiagList, IrEffectDef, IrExpr,
    IrExprKind, IrForeignDef, IrGlobal, IrInstanceDef, IrLit, IrModule, IrOrigin, IrParam,
};
pub use lower::lower_module;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
