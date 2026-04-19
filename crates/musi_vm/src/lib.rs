mod api;
mod diag;
mod error;
mod gc;
mod host;
mod loader;
mod opcode;
pub(crate) mod program;
mod types;
pub(crate) mod value;
mod vm;

pub use api::{
    ClosureView, ForeignView, HeapCollectionStats, ModuleView, Program, ProgramDataLayout,
    ProgramDataVariantLayout, ProgramExport, ProgramExportKind, ProgramTypeAbiKind, RecordView,
    RejectingHost, RejectingLoader, SeqView, StringView, Value, ValueView, Vm, VmHost,
    VmHostCallContext, VmHostContext, VmLoader, VmOptions, render_value_view,
};
pub use diag::VmDiagKind;
pub use error::{
    NativeFailureStage, OperandShape, VmError, VmErrorKind, VmIndexSpace, VmStackKind, VmValueKind,
};
pub use host::{EffectCall, ForeignCall};
pub use types::VmResult;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
