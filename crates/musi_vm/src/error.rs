use std::fmt::{self, Display, Formatter};

use music_seam::AssemblyError;
use music_seam::{Opcode, Operand};
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
    CPtr,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmStackKind {
    CallFrame,
    Operand,
    Handler,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VmIndexSpace {
    Local,
    Global,
    Method,
    ModuleSlot,
    EffectOp,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeFailureStage {
    LibraryLoad,
    SymbolLoad,
    AbiUnsupported,
    ArgInvalid,
    ResultInvalid,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VmErrorKind {
    SeamDecodeFailed {
        detail: Box<str>,
    },
    ProgramShapeInvalid {
        detail: Box<str>,
    },
    TypeTermInvalid {
        ty: Box<str>,
        detail: Box<str>,
    },
    SyntaxConstantInvalid {
        detail: Box<str>,
    },
    VmInitializationRequired,
    ModuleInitCycle {
        spec: Box<str>,
    },
    ExportNotFound {
        module: Box<str>,
        export: Box<str>,
    },
    OpaqueExport {
        module: Box<str>,
        export: Box<str>,
    },
    NonCallableValue {
        found: VmValueKind,
    },
    MissingEntryMethod {
        module: Box<str>,
    },
    StackEmpty {
        stack: VmStackKind,
    },
    OperandCountMismatch {
        needed: usize,
        available: usize,
    },
    IndexOutOfBounds {
        space: VmIndexSpace,
        owner: Option<Box<str>>,
        index: i64,
        len: usize,
    },
    BranchTargetInvalid {
        method: Box<str>,
        label: Option<u16>,
        index: Option<i64>,
        len: Option<usize>,
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
    InvalidRangeBounds {
        start: VmValueKind,
        end: VmValueKind,
    },
    InvalidRangeEvidence {
        found: VmValueKind,
    },
    InvalidRangeStep {
        detail: Box<str>,
    },
    RangeMaterializeTooLarge {
        len: usize,
        limit: usize,
    },
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
    PointerIntrinsicFailed {
        intrinsic: Box<str>,
        detail: Box<str>,
    },
    NativeCallFailed {
        foreign: Box<str>,
        stage: NativeFailureStage,
        subject: Option<Box<str>>,
        index: Option<usize>,
        detail: Box<str>,
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
        Self::new(VmErrorKind::SeamDecodeFailed {
            detail: value.to_string().into(),
        })
    }
}

impl VmErrorKind {
    fn fmt_decode(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::SeamDecodeFailed { detail } => {
                write!(f, "SEAM decode failed (`{detail}`)")
            }
            Self::ProgramShapeInvalid { detail } => {
                write!(f, "program shape invalid (`{detail}`)")
            }
            Self::TypeTermInvalid { ty, detail } => {
                write!(f, "type term invalid for `{ty}` (`{detail}`)")
            }
            Self::SyntaxConstantInvalid { detail } => {
                write!(f, "syntax constant invalid (`{detail}`)")
            }
            Self::VmInitializationRequired => f.write_str("vm initialization required"),
            Self::ModuleInitCycle { spec } => {
                write!(f, "module init cycle detected for `{spec}`")
            }
            Self::ExportNotFound { module, export } => {
                write!(f, "export `{export}` not found in `{module}`")
            }
            Self::OpaqueExport { module, export } => {
                write!(
                    f,
                    "opaque export `{export}` is not accessible from `{module}`"
                )
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
            Self::StackEmpty { stack } => write!(f, "empty {stack} stack"),
            Self::OperandCountMismatch { needed, available } => {
                write!(
                    f,
                    "operand stack needs `{needed}` values, only `{available}` are available"
                )
            }
            Self::IndexOutOfBounds {
                space,
                owner,
                index,
                len,
            } => match owner {
                Some(owner) => {
                    write!(
                        f,
                        "{space} `{index}` out of bounds for `{owner}` with `{len}` entries"
                    )
                }
                None => write!(f, "{space} `{index}` out of bounds for `{len}` entries"),
            },
            Self::BranchTargetInvalid {
                method,
                label: Some(label),
                ..
            } => write!(f, "branch label `{label}` missing in `{method}`"),
            Self::BranchTargetInvalid {
                method,
                index: Some(index),
                len: Some(len),
                ..
            } => {
                write!(
                    f,
                    "branch index `{index}` invalid for `{method}` with `{len}` labels"
                )
            }
            Self::BranchTargetInvalid { method, .. } => {
                write!(f, "branch target invalid in `{method}`")
            }
            Self::InvalidOperandForOpcode { opcode, found } => {
                write!(
                    f,
                    "opcode `{}` does not accept operand shape `{found}`",
                    opcode.mnemonic()
                )
            }
            _ => self.fmt_value_and_types(f),
        }
    }

    fn fmt_value_and_types(&self, f: &mut Formatter<'_>) -> fmt::Result {
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
            Self::InvalidRangeBounds { start, end } => {
                write!(
                    f,
                    "range bounds `{start}` and `{end}` must both be integer values"
                )
            }
            Self::InvalidRangeEvidence { found } => {
                write!(f, "range evidence invalid for value kind `{found}`")
            }
            Self::InvalidRangeStep { detail } => {
                write!(f, "range stepping failed (`{detail}`)")
            }
            Self::RangeMaterializeTooLarge { len, limit } => {
                write!(
                    f,
                    "range materialize length `{len}` exceeds limit `{limit}`"
                )
            }
            Self::InvalidDataIndex { index, len } => {
                write!(f, "data field index `{index}` invalid for length `{len}`")
            }
            Self::InvalidTypeCast { expected, found } => {
                write!(
                    f,
                    "type cast to `{expected}` failed for value kind `{found}`"
                )
            }
            _ => self.fmt_module_and_call(f),
        }
    }

    fn fmt_module_and_call(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ModuleLoadRejected { spec } => {
                write!(f, "module load rejected for `{spec}`")
            }
            Self::ForeignCallRejected { foreign } => {
                write!(f, "foreign call rejected for `{foreign}`")
            }
            Self::PointerIntrinsicFailed { intrinsic, detail } => {
                write!(f, "pointer intrinsic `{intrinsic}` failed (`{detail}`)")
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
            _ => self.fmt_native_and_effects(f),
        }
    }

    fn fmt_native_and_effects(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::NativeCallFailed {
                foreign,
                stage,
                subject,
                index,
                detail,
            } => match stage {
                NativeFailureStage::LibraryLoad => {
                    let library = subject.as_deref().unwrap_or("<unknown>");
                    write!(
                        f,
                        "native library load failed for `{foreign}` from `{library}` (`{detail}`)"
                    )
                }
                NativeFailureStage::SymbolLoad => {
                    let symbol = subject.as_deref().unwrap_or("<unknown>");
                    write!(
                        f,
                        "native symbol load failed for `{foreign}` symbol `{symbol}` (`{detail}`)"
                    )
                }
                NativeFailureStage::AbiUnsupported => {
                    write!(f, "native abi unsupported for `{foreign}` (`{detail}`)")
                }
                NativeFailureStage::ArgInvalid => {
                    let index = index.unwrap_or(usize::MAX);
                    write!(
                        f,
                        "native argument `{index}` invalid for `{foreign}` (`{detail}`)"
                    )
                }
                NativeFailureStage::ResultInvalid => {
                    write!(f, "native result invalid for `{foreign}` (`{detail}`)")
                }
            },
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
            _ => self.fmt_runtime_state(f),
        }
    }

    fn fmt_runtime_state(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
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
            _ => self.fmt_decode(f),
        }
    }
}

impl Display for VmErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.fmt_decode(f)
    }
}

impl Display for OperandShape {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write_kebab_debug_name(f, &format!("{self:?}"))
    }
}

impl Display for VmValueKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Display for VmStackKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::CallFrame => f.write_str("call frame"),
            Self::Operand => f.write_str("operand"),
            Self::Handler => f.write_str("handler"),
        }
    }
}

impl Display for VmIndexSpace {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Local => f.write_str("local slot"),
            Self::Global => f.write_str("global slot"),
            Self::Method => f.write_str("method id"),
            Self::ModuleSlot => f.write_str("module slot"),
            Self::EffectOp => f.write_str("effect operation index"),
        }
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

fn write_kebab_debug_name(f: &mut Formatter<'_>, text: &str) -> fmt::Result {
    for (index, ch) in text.chars().enumerate() {
        if ch.is_ascii_uppercase() && index > 0 {
            f.write_str("-")?;
        }
        let mut buf = [0; 4];
        for lower in ch.to_lowercase() {
            f.write_str(lower.encode_utf8(&mut buf))?;
        }
    }
    Ok(())
}
