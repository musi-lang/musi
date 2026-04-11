use std::fmt::{self, Display, Formatter};

use music_assembly::AssemblyError;
use music_bc::{Opcode, Operand};
use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OperandShape {
    None,
    Local,
    Global,
    Constant,
    I16,
    String,
    Label,
    BranchTable,
    Method,
    WideMethodCaptures,
    TypeLen,
    Type,
    EffectId,
    Effect,
    Foreign,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmValueKind {
    Unit,
    Int,
    Float,
    Bool,
    String,
    Syntax,
    Seq,
    Data,
    Closure,
    Continuation,
    Type,
    Module,
    Foreign,
    Effect,
    Class,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmErrorKind {
    DecodeFailed {
        detail: Box<str>,
    },
    InvalidProgram {
        detail: Box<str>,
    },
    InitializationRequired,
    ModuleInitCycle {
        spec: Box<str>,
    },
    ExportNotFound {
        module: Box<str>,
        export: Box<str>,
    },
    NonCallableValue {
        found: VmValueKind,
    },
    MissingEntryMethod {
        module: Box<str>,
    },
    EmptyCallFrameStack,
    EmptyOperandStack,
    EmptyHandlerStack,
    OperandCountMismatch {
        needed: usize,
        available: usize,
    },
    LocalOutOfBounds {
        slot: u16,
        len: usize,
    },
    GlobalOutOfBounds {
        module: Box<str>,
        slot: usize,
        len: usize,
    },
    MethodOutOfBounds {
        method: u32,
        len: usize,
    },
    ModuleSlotOutOfBounds {
        slot: usize,
        len: usize,
    },
    InvalidBranchTarget {
        method: Box<str>,
        label: u16,
    },
    InvalidBranchIndex {
        method: Box<str>,
        index: i64,
        len: usize,
    },
    InvalidOperandForOpcode {
        opcode: Opcode,
        found: OperandShape,
    },
    InvalidValueKind {
        expected: VmValueKind,
        found: VmValueKind,
    },
    InvalidSequenceIndex {
        index: i64,
        len: usize,
    },
    EmptySequenceIndexList,
    InvalidDataIndex {
        index: i64,
        len: usize,
    },
    InvalidTypeCast {
        expected: Box<str>,
        found: VmValueKind,
    },
    ModuleLoadRejected {
        spec: Box<str>,
    },
    ForeignCallRejected {
        foreign: Box<str>,
    },
    EffectRejected {
        effect: Box<str>,
        op: Option<Box<str>>,
        reason: Box<str>,
    },
    RootModuleRequired,
    ModuleSourceMissing {
        spec: Box<str>,
    },
    CallArityMismatch {
        callee: Box<str>,
        expected: usize,
        found: usize,
    },
    HandlerFrameMissing {
        handler_id: u64,
        frame_depth: usize,
    },
    MatchingHandlerPopMissing {
        method: Box<str>,
    },
    EffectOpOutOfBounds {
        effect: Box<str>,
        op_index: u16,
        op_count: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("{kind}")]
pub struct VmError {
    kind: VmErrorKind,
}

impl VmError {
    #[must_use]
    pub const fn new(kind: VmErrorKind) -> Self {
        Self { kind }
    }

    #[must_use]
    pub const fn kind(&self) -> &VmErrorKind {
        &self.kind
    }

    #[must_use]
    pub fn message(&self) -> String {
        self.kind.to_string()
    }
}

impl From<AssemblyError> for VmError {
    fn from(value: AssemblyError) -> Self {
        Self::new(VmErrorKind::DecodeFailed {
            detail: value.to_string().into(),
        })
    }
}

impl VmErrorKind {
    fn fmt_decode_and_lookup(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::DecodeFailed { detail } => {
                write!(f, "decode failed for SEAM bytes (`{detail}`)")
            }
            Self::InvalidProgram { detail } => {
                write!(f, "program is invalid (`{detail}`)")
            }
            Self::InitializationRequired => f.write_str("vm initialization required"),
            Self::ModuleInitCycle { spec } => {
                write!(f, "module init cycle detected for `{spec}`")
            }
            Self::ExportNotFound { module, export } => {
                write!(f, "export `{export}` not found in `{module}`")
            }
            Self::NonCallableValue { found } => {
                write!(f, "call requires callable value, found `{found}`")
            }
            Self::MissingEntryMethod { module } => {
                write!(f, "entry method missing in `{module}`")
            }
            _ => self.fmt_stack_and_bounds(f),
        }
    }

    fn fmt_stack_and_bounds(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::EmptyCallFrameStack => f.write_str("empty call frame stack"),
            Self::EmptyOperandStack => f.write_str("empty operand stack"),
            Self::EmptyHandlerStack => f.write_str("empty handler stack"),
            Self::OperandCountMismatch { needed, available } => {
                write!(
                    f,
                    "operand stack needs `{needed}` values, only `{available}` are available"
                )
            }
            Self::LocalOutOfBounds { slot, len } => {
                write!(f, "local slot `{slot}` out of bounds for `{len}` locals")
            }
            Self::GlobalOutOfBounds { module, slot, len } => {
                write!(
                    f,
                    "global slot `{slot}` out of bounds for `{module}` with `{len}` globals"
                )
            }
            Self::MethodOutOfBounds { method, len } => {
                write!(f, "method id `{method}` out of bounds for `{len}` methods")
            }
            Self::ModuleSlotOutOfBounds { slot, len } => {
                write!(f, "module slot `{slot}` out of bounds for `{len}` modules")
            }
            Self::InvalidBranchTarget { method, label } => {
                write!(f, "branch label `{label}` missing in `{method}`")
            }
            Self::InvalidBranchIndex { method, index, len } => {
                write!(
                    f,
                    "branch index `{index}` invalid for `{method}` with `{len}` labels"
                )
            }
            Self::InvalidOperandForOpcode { opcode, found } => {
                write!(
                    f,
                    "opcode `{}` does not accept operand shape `{found}`",
                    opcode.mnemonic()
                )
            }
            _ => self.fmt_value_and_runtime(f),
        }
    }

    fn fmt_value_and_runtime(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidValueKind { expected, found } => {
                write!(
                    f,
                    "value kind `{found}` does not match expected `{expected}`"
                )
            }
            Self::InvalidSequenceIndex { index, len } => {
                write!(f, "sequence index `{index}` invalid for length `{len}`")
            }
            Self::EmptySequenceIndexList => f.write_str("empty sequence index list"),
            Self::InvalidDataIndex { index, len } => {
                write!(f, "data field index `{index}` invalid for length `{len}`")
            }
            Self::InvalidTypeCast { expected, found } => {
                write!(
                    f,
                    "type cast to `{expected}` failed for value kind `{found}`"
                )
            }
            Self::ModuleLoadRejected { spec } => {
                write!(f, "module load rejected for `{spec}`")
            }
            Self::ForeignCallRejected { foreign } => {
                write!(f, "foreign call rejected for `{foreign}`")
            }
            Self::EffectRejected { effect, op, reason } => {
                if let Some(op) = op {
                    write!(
                        f,
                        "rejected effect `{effect}` operation `{op}` (`{reason}`)"
                    )
                } else {
                    write!(f, "rejected effect `{effect}` (`{reason}`)")
                }
            }
            Self::RootModuleRequired => f.write_str("root module required"),
            Self::ModuleSourceMissing { spec } => {
                write!(f, "module source missing for `{spec}`")
            }
            Self::CallArityMismatch {
                callee,
                expected,
                found,
            } => {
                write!(
                    f,
                    "call arity mismatch for `{callee}` expects `{expected}`, found `{found}`"
                )
            }
            Self::HandlerFrameMissing {
                handler_id,
                frame_depth,
            } => {
                write!(
                    f,
                    "handler `{handler_id}` frame depth `{frame_depth}` missing"
                )
            }
            Self::MatchingHandlerPopMissing { method } => {
                write!(f, "matching `hdl.pop` missing in `{method}`")
            }
            Self::EffectOpOutOfBounds {
                effect,
                op_index,
                op_count,
            } => {
                write!(
                    f,
                    "effect `{effect}` operation index `{op_index}` out of bounds for `{op_count}` operations"
                )
            }
            Self::DecodeFailed { .. }
            | Self::InvalidProgram { .. }
            | Self::InitializationRequired
            | Self::ModuleInitCycle { .. }
            | Self::ExportNotFound { .. }
            | Self::NonCallableValue { .. }
            | Self::MissingEntryMethod { .. }
            | Self::EmptyCallFrameStack
            | Self::EmptyOperandStack
            | Self::EmptyHandlerStack
            | Self::OperandCountMismatch { .. }
            | Self::LocalOutOfBounds { .. }
            | Self::GlobalOutOfBounds { .. }
            | Self::MethodOutOfBounds { .. }
            | Self::ModuleSlotOutOfBounds { .. }
            | Self::InvalidBranchTarget { .. }
            | Self::InvalidBranchIndex { .. }
            | Self::InvalidOperandForOpcode { .. } => self.fmt_decode_and_lookup(f),
        }
    }
}

impl Display for VmErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_decode_and_lookup(f)
    }
}

impl Display for OperandShape {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let text = match self {
            Self::None => "none",
            Self::Local => "local",
            Self::Global => "global",
            Self::Constant => "constant",
            Self::I16 => "i16",
            Self::String => "string",
            Self::Label => "label",
            Self::BranchTable => "branch-table",
            Self::Method => "method",
            Self::WideMethodCaptures => "method-captures",
            Self::TypeLen => "type-len",
            Self::Type => "type",
            Self::EffectId => "effect-id",
            Self::Effect => "effect",
            Self::Foreign => "foreign",
        };
        f.write_str(text)
    }
}

impl Display for VmValueKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let text = match self {
            Self::Unit => "Unit",
            Self::Int => "Int",
            Self::Float => "Float",
            Self::Bool => "Bool",
            Self::String => "String",
            Self::Syntax => "Syntax",
            Self::Seq => "Seq",
            Self::Data => "Data",
            Self::Closure => "Closure",
            Self::Continuation => "Continuation",
            Self::Type => "Type",
            Self::Module => "Module",
            Self::Foreign => "Foreign",
            Self::Effect => "Effect",
            Self::Class => "Class",
        };
        f.write_str(text)
    }
}

impl From<&Operand> for OperandShape {
    fn from(value: &Operand) -> Self {
        match value {
            Operand::None => Self::None,
            Operand::Local(_) => Self::Local,
            Operand::Global(_) => Self::Global,
            Operand::Constant(_) => Self::Constant,
            Operand::I16(_) => Self::I16,
            Operand::String(_) => Self::String,
            Operand::Label(_) => Self::Label,
            Operand::BranchTable(_) => Self::BranchTable,
            Operand::Method(_) => Self::Method,
            Operand::WideMethodCaptures { .. } => Self::WideMethodCaptures,
            Operand::TypeLen { .. } => Self::TypeLen,
            Operand::Type(_) => Self::Type,
            Operand::EffectId(_) => Self::EffectId,
            Operand::Effect { .. } => Self::Effect,
            Operand::Foreign(_) => Self::Foreign,
        }
    }
}
