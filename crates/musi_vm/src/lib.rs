mod api;
mod error;
mod host;
mod loader;
mod opcode;
pub(crate) mod program;
mod types;
pub(crate) mod value;
mod vm;

pub use api::{
    Program, ProgramExport, ProgramExportKind, RecordView, RejectingHost, RejectingLoader, SeqView,
    StringView, Value, ValueView, Vm, VmHost, VmLoader, VmOptions,
};
pub use error::{OperandShape, VmError, VmErrorKind, VmValueKind};
pub use host::{EffectCall, ForeignCall};
pub use types::VmResult;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
