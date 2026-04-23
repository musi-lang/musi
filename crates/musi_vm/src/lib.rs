#![allow(unsafe_code)]

mod api;
mod diag;
mod error;
mod gc;
mod host;
mod loader;
pub(crate) mod program;
mod program_init;
mod program_kernel;
mod types;
pub(crate) mod value;
mod vm;

pub use api::{
    BitsValue, BoundI64Call, BoundInitCall, BoundSeq2x2Call, BoundSeq2x2PackedArg, ClosureView,
    ForeignView, HeapCollectionStats, IsolateId, ModuleView, Program, ProgramDataLayout,
    ProgramDataVariantLayout, ProgramExport, ProgramExportKind, ProgramTypeAbiKind, RecordView,
    RejectingHost, RejectingLoader, SeqView, StringView, Value, ValueView, Vm, VmHost,
    VmHostCallContext, VmHostContext, VmLoader, VmOptions, VmRuntime, render_value_view,
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
