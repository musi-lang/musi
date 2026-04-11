use musi_vm::{EffectCall, ForeignCall, Value, VmError, VmErrorKind, VmResult};

#[derive(Debug, Default)]
pub struct PlatformHost;

impl PlatformHost {
    #[must_use]
    pub const fn new() -> Self {
        Self
    }

    #[must_use]
    pub fn call_foreign(&self, foreign: &ForeignCall, _args: &[Value]) -> Option<VmResult<Value>> {
        if is_supported_target() {
            return None;
        }
        Some(Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        })))
    }

    #[must_use]
    pub fn handle_effect(&self, effect: &EffectCall, _args: &[Value]) -> Option<VmResult<Value>> {
        if is_supported_target() {
            return None;
        }
        Some(Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: unsupported_target_reason(),
        })))
    }
}

#[must_use]
pub(crate) const fn is_supported_target() -> bool {
    cfg!(any(
        target_os = "macos",
        target_os = "linux",
        target_os = "windows"
    ))
}

fn unsupported_target_reason() -> Box<str> {
    format!("unsupported native host target `{}`", std::env::consts::OS).into_boxed_str()
}
