mod api;
mod error;
mod host;
mod opcode;
pub(crate) mod program;
mod types;
pub(crate) mod value;
mod vm;

pub use api::{
    NativeHost, Program, ProgramExport, ProgramExportKind, RecordView, SeqView, StringView, Value,
    ValueView, Vm, VmHost, VmOptions,
};
pub use error::{OperandShape, VmError, VmErrorKind, VmValueKind};
pub use host::{EffectCall, ForeignCall};
pub use types::VmResult;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
