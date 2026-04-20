use super::gc::{HeapCollectionStats, HeapOptions, RuntimeHeap};
pub use super::host::{EffectCall, ForeignCall, VmHostContext};
pub use super::loader::{RejectingLoader, VmLoader};
pub use super::program::{
    CompareOp, RuntimeCallMode, RuntimeCallShape, RuntimeFusedOp, RuntimeInstruction,
    RuntimeInstructionList, RuntimeKernel, RuntimeOperand, RuntimeSeq2Mutation,
};
pub use super::value::{
    ClosureValue, ClosureView, ContinuationFrame, ContinuationHandler, ContinuationValue,
    DataValue, ForeignValue, GcRef, ModuleValue, ModuleView, ProcedureValue, SequenceValue,
    SyntaxView, ValueList,
};

pub use super::{
    OperandShape, Program, RecordView, RejectingHost, SeqView, StringView, Value, ValueView,
    VmError, VmErrorKind, VmHost, VmResult, VmValueKind,
};

mod bound;
mod alloc;
mod call;
mod dispatch;
mod exec_control;
mod frames;
mod gc_roots;
mod inspect;
mod instructions;
mod kernel;
mod locals;
mod module;
mod operands;
mod ops;
mod state;
mod value_support;

use self::state::{
    CallFrame, CallFrameList, EffectHandlerList, LoadedModuleList, ModuleSlotMap, ResumeList,
};

mod boundary;
mod core;
mod options;
mod runtime;

pub use bound::BoundExport;
pub use options::{VmOptimizationLevel, VmOptions};
pub use runtime::VmRuntime;

use self::boundary::{HostState, LoaderState};
pub struct Vm {
    loaded_modules: LoadedModuleList,
    module_slots: Option<ModuleSlotMap>,
    loader: LoaderState,
    host: HostState,
    options: VmOptions,
    frames: CallFrameList,
    spare_frames: Vec<CallFrame>,
    handlers: EffectHandlerList,
    active_resumes: ResumeList,
    next_handler_id: u64,
    continuation_target_handler: Option<u64>,
    return_depth: Option<usize>,
    heap: RuntimeHeap,
    heap_dirty: bool,
    executed_instructions: u64,
    external_roots: Vec<Value>,
}
