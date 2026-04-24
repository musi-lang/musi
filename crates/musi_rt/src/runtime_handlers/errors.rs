use std::fmt::Display;

use musi_vm::{EffectCall, VmDiagKind, VmError, VmErrorKind};
use music_base::diag::DiagContext;

fn effect_rejected(effect: &EffectCall, reason: impl Into<Box<str>>) -> VmError {
    VmError::new(VmErrorKind::EffectRejected {
        effect: effect.effect_name().into(),
        op: Some(effect.op_name().into()),
        reason: reason.into(),
    })
}

pub(super) fn invalid_runtime_args(
    effect: &EffectCall,
    expected: &str,
    found: impl Display,
) -> VmError {
    runtime_effect_reason(
        effect,
        VmDiagKind::RuntimeEffectArgsInvalid,
        DiagContext::new()
            .with("effect", effect.effect_name())
            .with("op", effect.op_name())
            .with("expected", expected)
            .with("found", found),
    )
}

pub(super) fn runtime_effect_failed(effect: &EffectCall, source: impl Display) -> VmError {
    runtime_effect_reason(
        effect,
        VmDiagKind::RuntimeEffectOperationFailed,
        DiagContext::new()
            .with("effect", effect.effect_name())
            .with("op", effect.op_name())
            .with("source", source),
    )
}

pub(super) fn runtime_host_unavailable(effect: &EffectCall, subject: &str) -> VmError {
    runtime_effect_reason(
        effect,
        VmDiagKind::RuntimeHostUnavailable,
        DiagContext::new().with("subject", subject),
    )
}

pub(super) fn runtime_effect_unsupported(effect: &EffectCall) -> VmError {
    runtime_effect_reason(
        effect,
        VmDiagKind::RuntimeEffectUnsupported,
        DiagContext::new()
            .with("effect", effect.effect_name())
            .with("op", effect.op_name()),
    )
}

fn runtime_effect_reason(effect: &EffectCall, kind: VmDiagKind, context: DiagContext) -> VmError {
    let reason = kind.message_with(&context);
    drop(context);
    effect_rejected(effect, reason)
}
