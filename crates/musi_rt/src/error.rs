use std::error::Error;
use std::fmt::{self, Display, Formatter, Result as FmtResult};

use musi_vm::{VmError, VmValueKind};
use music_base::diag::{CatalogDiagnostic, DiagContext};
use music_session::SessionError;

use crate::{RuntimeDiagKind, diag::runtime_error_kind};

pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuntimeSessionPhase {
    Setup,
    Parse,
    Resolve,
    Sema,
    Ir,
    Emit,
}

impl Display for RuntimeSessionPhase {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::Setup => f.write_str("setup"),
            Self::Parse => f.write_str("parse"),
            Self::Resolve => f.write_str("resolve"),
            Self::Sema => f.write_str("semantic check"),
            Self::Ir => f.write_str("lowering"),
            Self::Emit => f.write_str("emit"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    RootModuleRequired,
    MissingModuleSource {
        spec: Box<str>,
    },
    InvalidSyntaxValue {
        found: VmValueKind,
    },
    SessionFailed {
        phase: RuntimeSessionPhase,
        detail: Box<str>,
    },
    VmExecutionFailed(VmError),
}

#[derive(Debug)]
pub struct RuntimeError {
    kind: RuntimeErrorKind,
}

impl RuntimeError {
    #[must_use]
    pub const fn new(kind: RuntimeErrorKind) -> Self {
        Self { kind }
    }

    #[must_use]
    pub const fn kind(&self) -> &RuntimeErrorKind {
        &self.kind
    }

    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<RuntimeDiagKind> {
        self.kind.diagnostic()
    }
}

impl Display for RuntimeErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl RuntimeErrorKind {
    #[must_use]
    pub fn diagnostic(&self) -> CatalogDiagnostic<RuntimeDiagKind> {
        CatalogDiagnostic::new(self.diag_kind(), self.diag_context())
    }

    const fn diag_kind(&self) -> RuntimeDiagKind {
        runtime_error_kind(self)
    }

    fn diag_context(&self) -> DiagContext {
        match self {
            Self::MissingModuleSource { spec } => DiagContext::new().with("spec", spec),
            Self::InvalidSyntaxValue { found } => DiagContext::new().with("found", found),
            Self::SessionFailed { phase, detail } => DiagContext::new()
                .with("phase", phase)
                .with("detail", detail),
            Self::VmExecutionFailed(source) => DiagContext::new().with("source", source),
            Self::RootModuleRequired => DiagContext::new(),
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.diagnostic(), f)
    }
}

impl Error for RuntimeError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match &self.kind {
            RuntimeErrorKind::VmExecutionFailed(source) => Some(source),
            _ => None,
        }
    }
}

impl From<VmError> for RuntimeError {
    fn from(value: VmError) -> Self {
        Self::new(RuntimeErrorKind::VmExecutionFailed(value))
    }
}

impl From<SessionError> for RuntimeError {
    fn from(value: SessionError) -> Self {
        session_error(&value)
    }
}

fn session_error(value: &SessionError) -> RuntimeError {
    let detail = session_error_detail(value);
    let phase = match value {
        SessionError::ModuleParseFailed { .. } => RuntimeSessionPhase::Parse,
        SessionError::ModuleResolveFailed { .. } => RuntimeSessionPhase::Resolve,
        SessionError::ModuleSemanticCheckFailed { .. } => RuntimeSessionPhase::Sema,
        SessionError::ModuleLoweringFailed { .. } => RuntimeSessionPhase::Ir,
        SessionError::ModuleEmissionFailed { .. } => RuntimeSessionPhase::Emit,
        _ => RuntimeSessionPhase::Setup,
    };
    let kind = RuntimeErrorKind::SessionFailed { phase, detail };
    RuntimeError::new(kind)
}

fn session_error_detail(value: &SessionError) -> Box<str> {
    match value {
        SessionError::ModuleParseFailed { syntax, .. } => syntax
            .diags()
            .first()
            .map_or_else(|| value.to_string().into(), |diag| diag.message().into()),
        SessionError::ModuleResolveFailed { diags, .. }
        | SessionError::ModuleSemanticCheckFailed { diags, .. }
        | SessionError::ModuleLoweringFailed { diags, .. }
        | SessionError::ModuleEmissionFailed { diags, .. } => diags
            .first()
            .map_or_else(|| value.to_string().into(), |diag| diag.message().into()),
        _ => value.to_string().into(),
    }
}
