use musi_foundation::syntax;
use musi_vm::{VmError, VmErrorKind};
use music_session::SessionError;

use crate::error::{RuntimeError, RuntimeErrorKind};
use crate::runtime::syntax_eval::vm_syntax_value_kind_error;

pub(super) fn runtime_session_error(err: SessionError) -> RuntimeError {
    RuntimeError::from(err)
}

pub(super) fn vm_runtime_error(err: &RuntimeError) -> VmError {
    match err.kind() {
        RuntimeErrorKind::SessionFailed { detail, .. } => {
            VmError::new(VmErrorKind::EffectRejected {
                effect: syntax::EFFECT.into(),
                op: Some(syntax::EVAL_OP.into()),
                reason: detail.clone(),
            })
        }
        RuntimeErrorKind::MissingModuleSource { spec } => {
            VmError::new(VmErrorKind::MissingModuleSource { spec: spec.clone() })
        }
        RuntimeErrorKind::InvalidSyntaxValue { found } => vm_syntax_value_kind_error(*found),
        RuntimeErrorKind::RootModuleRequired => VmError::new(VmErrorKind::InvalidProgramShape {
            detail: "root module required".into(),
        }),
        RuntimeErrorKind::VmExecutionFailed(err) => err.clone(),
    }
}

pub(super) fn vm_session_error(err: &SessionError) -> VmError {
    let stage = match &err {
        SessionError::ModuleParseFailed { .. } => "parse failed",
        SessionError::ModuleResolveFailed { .. } => "resolve failed",
        SessionError::ModuleSemanticCheckFailed { .. } => "semantic check failed",
        SessionError::ModuleLoweringFailed { .. } => "lowering failed",
        SessionError::ModuleEmissionFailed { .. } => "emission failed",
        _ => "session setup failed",
    };
    let detail = session_error_detail(err);
    VmError::new(VmErrorKind::InvalidProgramShape {
        detail: format!("{stage} (`{detail}`)").into(),
    })
}

fn session_error_detail(err: &SessionError) -> Box<str> {
    match err {
        SessionError::ModuleParseFailed { syntax, .. } => {
            first_diag_message_or_error(syntax.diags(), err, |diag| -> &str { diag.message() })
        }
        SessionError::ModuleResolveFailed { diags, .. }
        | SessionError::ModuleSemanticCheckFailed { diags, .. }
        | SessionError::ModuleLoweringFailed { diags, .. }
        | SessionError::ModuleEmissionFailed { diags, .. } => {
            first_diag_message_or_error(diags, err, |diag| -> &str { diag.message() })
        }
        _ => err.to_string().into(),
    }
}

fn first_diag_message_or_error<T>(
    diags: &[T],
    err: &SessionError,
    message: impl Fn(&T) -> &str,
) -> Box<str> {
    diags
        .first()
        .map_or_else(|| err.to_string().into(), |diag| message(diag).into())
}
