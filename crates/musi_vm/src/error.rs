use std::error::Error;
use std::fmt::{self, Display, Formatter};

use music_base::diag::{CatalogDiagnostic, DiagContext};
use music_seam::AssemblyError;
use music_seam::{Opcode, Operand};

use crate::{VmDiagKind, diag::vm_error_kind};

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
    Procedure,
    WideProcedureCaptures,
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
    Nat,
    Float,
    Bool,
    String,
    CPtr,
    Syntax,
    Seq,
    Data,
    Closure,
    Procedure,
    Continuation,
    Type,
    Module,
    Foreign,
    Effect,
    Shape,
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
    Procedure,
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
    InvalidProgramShape {
        detail: Box<str>,
    },
    InvalidTypeTerm {
        ty: Box<str>,
        detail: Box<str>,
    },
    InvalidSyntaxConstant {
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
    MissingEntryProcedure {
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
    InvalidBranchTarget {
        procedure: Box<str>,
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
    ArithmeticFailed {
        detail: Box<str>,
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
    MissingModuleSource {
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
    MissingMatchingHandlerPop {
        procedure: Box<str>,
    },
    HeapLimitExceeded {
        allocated: usize,
        limit: usize,
    },
    HeapObjectTooLarge {
        bytes: usize,
        limit: usize,
    },
    StackFrameLimitExceeded {
        frames: usize,
        limit: usize,
    },
    InstructionBudgetExhausted {
        budget: u64,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<VmDiagKind> {
        self.kind.diagnostic()
    }
}

impl Display for VmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl Error for VmError {}

impl From<AssemblyError> for VmError {
    fn from(value: AssemblyError) -> Self {
        Self::new(VmErrorKind::SeamDecodeFailed {
            detail: value.to_string().into(),
        })
    }
}

impl VmErrorKind {
    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<VmDiagKind> {
        CatalogDiagnostic::new(self.diag_kind(), self.diag_context())
    }

    const fn diag_kind(&self) -> VmDiagKind {
        vm_error_kind(self)
    }

    fn diag_context(&self) -> DiagContext {
        self.decode_diag_context()
            .or_else(|| self.stack_diag_context())
            .or_else(|| self.value_diag_context())
            .or_else(|| self.runtime_diag_context())
            .unwrap_or_default()
    }

    fn decode_diag_context(&self) -> Option<DiagContext> {
        Some(match self {
            Self::SeamDecodeFailed { detail }
            | Self::InvalidProgramShape { detail }
            | Self::InvalidSyntaxConstant { detail } => DiagContext::new().with("detail", detail),
            Self::InvalidTypeTerm { ty, detail } => {
                DiagContext::new().with("ty", ty).with("detail", detail)
            }
            Self::ModuleInitCycle { spec } => DiagContext::new().with("spec", spec),
            Self::ExportNotFound { module, export } | Self::OpaqueExport { module, export } => {
                DiagContext::new()
                    .with("module", module)
                    .with("export", export)
            }
            Self::NonCallableValue { found } => DiagContext::new().with("found", found),
            Self::MissingEntryProcedure { module } => DiagContext::new().with("module", module),
            Self::VmInitializationRequired => DiagContext::new(),
            _ => return None,
        })
    }

    fn stack_diag_context(&self) -> Option<DiagContext> {
        Some(match self {
            Self::StackEmpty { stack } => DiagContext::new().with("stack", stack),
            Self::OperandCountMismatch { needed, available } => DiagContext::new()
                .with("needed", needed)
                .with("available", available),
            Self::IndexOutOfBounds {
                space,
                owner,
                index,
                len,
            } => DiagContext::new()
                .with("space", space)
                .with("owner", owner.as_deref().unwrap_or("<unknown>"))
                .with("index", index)
                .with("len", len),
            Self::InvalidBranchTarget {
                procedure,
                label,
                index,
                ..
            } => DiagContext::new()
                .with("procedure", procedure)
                .with("target", branch_target_name(*label, *index)),
            Self::InvalidOperandForOpcode { opcode, found } => DiagContext::new()
                .with("opcode", opcode.mnemonic())
                .with("found", found),
            _ => return None,
        })
    }

    fn value_diag_context(&self) -> Option<DiagContext> {
        Some(match self {
            Self::InvalidValueKind { expected, found } => DiagContext::new()
                .with("expected", expected)
                .with("found", found),
            Self::InvalidTypeCast { expected, found } => DiagContext::new()
                .with("expected", expected)
                .with("found", found),
            Self::InvalidSequenceIndex { index, len } | Self::InvalidDataIndex { index, len } => {
                DiagContext::new().with("index", index).with("len", len)
            }
            Self::InvalidRangeBounds { start, end } => {
                DiagContext::new().with("start", start).with("end", end)
            }
            Self::InvalidRangeEvidence { found } => DiagContext::new().with("found", found),
            Self::InvalidRangeStep { detail } | Self::ArithmeticFailed { detail } => {
                DiagContext::new().with("detail", detail)
            }
            Self::RangeMaterializeTooLarge { len, limit } => {
                DiagContext::new().with("len", len).with("limit", limit)
            }
            Self::ModuleLoadRejected { spec } | Self::MissingModuleSource { spec } => {
                DiagContext::new().with("spec", spec)
            }
            Self::ForeignCallRejected { foreign } => DiagContext::new().with("foreign", foreign),
            Self::PointerIntrinsicFailed { intrinsic, detail } => DiagContext::new()
                .with("intrinsic", intrinsic)
                .with("detail", detail),
            Self::EmptySequenceIndexList => DiagContext::new(),
            _ => return None,
        })
    }

    fn runtime_diag_context(&self) -> Option<DiagContext> {
        Some(match self {
            Self::NativeCallFailed {
                foreign,
                stage,
                subject,
                index,
                detail,
            } => DiagContext::new()
                .with("foreign", foreign)
                .with("stage", stage)
                .with("subject", native_subject(subject.as_deref(), *index))
                .with("detail", detail),
            Self::EffectRejected { effect, op, reason } => DiagContext::new()
                .with("effect", effect)
                .with("op", op.as_deref().unwrap_or("<any>"))
                .with("reason", reason),
            Self::CallArityMismatch {
                callee,
                expected,
                found,
            } => DiagContext::new()
                .with("callee", callee)
                .with("expected", expected)
                .with("found", found),
            Self::HandlerFrameMissing {
                handler_id,
                frame_depth,
            } => DiagContext::new()
                .with("handler_id", handler_id)
                .with("frame_depth", frame_depth),
            Self::MissingMatchingHandlerPop { procedure } => {
                DiagContext::new().with("procedure", procedure)
            }
            Self::HeapLimitExceeded { allocated, limit } => DiagContext::new()
                .with("allocated", allocated)
                .with("limit", limit),
            Self::HeapObjectTooLarge { bytes, limit } => {
                DiagContext::new().with("bytes", bytes).with("limit", limit)
            }
            Self::StackFrameLimitExceeded { frames, limit } => DiagContext::new()
                .with("frames", frames)
                .with("limit", limit),
            Self::InstructionBudgetExhausted { budget } => {
                DiagContext::new().with("budget", budget)
            }
            Self::RootModuleRequired => DiagContext::new(),
            _ => return None,
        })
    }
}
impl Display for VmErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl Display for NativeFailureStage {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::LibraryLoad => f.write_str("library load"),
            Self::SymbolLoad => f.write_str("symbol load"),
            Self::AbiUnsupported => f.write_str("application binary interface validation"),
            Self::ArgInvalid => f.write_str("argument validation"),
            Self::ResultInvalid => f.write_str("result validation"),
        }
    }
}

fn branch_target_name(label: Option<u16>, index: Option<i64>) -> String {
    match (label, index) {
        (Some(label), _) => format!("label {label}"),
        (_, Some(index)) => format!("index {index}"),
        _ => "<unknown>".to_owned(),
    }
}

fn native_subject(subject: Option<&str>, index: Option<usize>) -> String {
    subject.map_or_else(
        || {
            index.map_or_else(
                || "<unknown>".to_owned(),
                |index| format!("argument {index}"),
            )
        },
        str::to_owned,
    )
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
            Self::Procedure => f.write_str("procedure identifier"),
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
            Operand::Procedure(_) => Self::Procedure,
            Operand::WideProcedureCaptures { .. } => Self::WideProcedureCaptures,
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
