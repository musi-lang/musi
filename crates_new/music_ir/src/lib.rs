mod api;
mod lower;

pub use api::{
    IrCallable, IrClassDef, IrDataDef, IrDiagList, IrEffectDef, IrForeignDef, IrInstanceDef,
    IrModule,
};
pub use lower::lower_module;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
