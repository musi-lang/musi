use std::fmt::{Display, Formatter, Result as FmtResult};

use musi_vm::{VmError, VmValueKind};
use music_session::SessionError;
use thiserror::Error;

pub type RuntimeResult<T = ()> = Result<T, RuntimeError>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeErrorKind {
    RootModuleRequired,
    ModuleSourceMissing { spec: Box<str> },
    InvalidSyntaxValue { found: VmValueKind },
    SessionSetupFailed { detail: Box<str> },
    SessionParseFailed { detail: Box<str> },
    SessionResolveFailed { detail: Box<str> },
    SessionSemanticCheckFailed { detail: Box<str> },
    SessionLoweringFailed { detail: Box<str> },
    SessionEmitFailed { detail: Box<str> },
    VmExecutionFailed(VmError),
}

#[derive(Debug, Error)]
#[error("{kind}")]
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
}

impl Display for RuntimeErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Self::RootModuleRequired => f.write_str("root module required"),
            Self::ModuleSourceMissing { spec } => {
                write!(f, "module source missing for `{spec}`")
            }
            Self::InvalidSyntaxValue { found } => {
                write!(f, "syntax value required, found `{found}`")
            }
            Self::SessionSetupFailed { detail } => {
                write!(f, "session setup failed (`{detail}`)")
            }
            Self::SessionParseFailed { detail } => {
                write!(f, "session parse failed (`{detail}`)")
            }
            Self::SessionResolveFailed { detail } => {
                write!(f, "session resolve failed (`{detail}`)")
            }
            Self::SessionSemanticCheckFailed { detail } => {
                write!(f, "session semantic check failed (`{detail}`)")
            }
            Self::SessionLoweringFailed { detail } => {
                write!(f, "session lowering failed (`{detail}`)")
            }
            Self::SessionEmitFailed { detail } => {
                write!(f, "session emit failed (`{detail}`)")
            }
            Self::VmExecutionFailed(err) => err.fmt(f),
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
    let kind = match value {
        SessionError::ModuleParseFailed { .. } => RuntimeErrorKind::SessionParseFailed { detail },
        SessionError::ModuleResolveFailed { .. } => {
            RuntimeErrorKind::SessionResolveFailed { detail }
        }
        SessionError::ModuleSemanticCheckFailed { .. } => {
            RuntimeErrorKind::SessionSemanticCheckFailed { detail }
        }
        SessionError::ModuleLoweringFailed { .. } => {
            RuntimeErrorKind::SessionLoweringFailed { detail }
        }
        SessionError::ModuleEmissionFailed { .. } => RuntimeErrorKind::SessionEmitFailed { detail },
        _ => RuntimeErrorKind::SessionSetupFailed { detail },
    };
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
