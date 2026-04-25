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
    BitsValue, BoundExportCall, BoundI64Call, BoundInitCall, BoundSeq2x2Arg, BoundSeq2x2Call,
    BoundSeq8Call, ClosureView, ForeignView, HeapCollectionStats, HeapValueKind, IsolateId,
    ModuleView, MvmFeatures, MvmMode, MvmOptionsParseError, Program, ProgramDataLayout,
    ProgramDataVariantLayout, ProgramExport, ProgramExportKind, ProgramTypeAbiKind, RecordView,
    RejectingHost, RejectingLoader, SeqView, StringView, Value, ValueView, Vm, VmHost,
    VmHostCallContext, VmHostContext, VmLoader, VmOptimizationLevel, VmOptions, VmRuntime,
    render_value_view,
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
