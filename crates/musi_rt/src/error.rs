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
    SessionSemaFailed { detail: Box<str> },
    SessionIrFailed { detail: Box<str> },
    SessionEmitFailed { detail: Box<str> },
    Vm(VmError),
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
            Self::SessionSemaFailed { detail } => {
                write!(f, "session sema failed (`{detail}`)")
            }
            Self::SessionIrFailed { detail } => {
                write!(f, "session ir failed (`{detail}`)")
            }
            Self::SessionEmitFailed { detail } => {
                write!(f, "session emit failed (`{detail}`)")
            }
            Self::Vm(err) => err.fmt(f),
        }
    }
}

impl From<VmError> for RuntimeError {
    fn from(value: VmError) -> Self {
        Self::new(RuntimeErrorKind::Vm(value))
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
        SessionError::Parse { .. } => RuntimeErrorKind::SessionParseFailed { detail },
        SessionError::Resolve { .. } => RuntimeErrorKind::SessionResolveFailed { detail },
        SessionError::Sema { .. } => RuntimeErrorKind::SessionSemaFailed { detail },
        SessionError::Ir { .. } => RuntimeErrorKind::SessionIrFailed { detail },
        SessionError::Emit { .. } => RuntimeErrorKind::SessionEmitFailed { detail },
        _ => RuntimeErrorKind::SessionSetupFailed { detail },
    };
    RuntimeError::new(kind)
}

fn session_error_detail(value: &SessionError) -> Box<str> {
    match value {
        SessionError::Parse { syntax, .. } => syntax
            .diags()
            .first()
            .map_or_else(|| value.to_string().into(), |diag| diag.message().into()),
        SessionError::Resolve { diags, .. }
        | SessionError::Sema { diags, .. }
        | SessionError::Ir { diags, .. }
        | SessionError::Emit { diags, .. } => diags
            .first()
            .map_or_else(|| value.to_string().into(), |diag| diag.message().into()),
        _ => value.to_string().into(),
    }
}
