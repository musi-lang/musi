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
    Session { detail: Box<str> },
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
            Self::Session { detail } => write!(f, "runtime session failed (`{detail}`)"),
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
        Self::new(RuntimeErrorKind::Session {
            detail: value.to_string().into(),
        })
    }
}
