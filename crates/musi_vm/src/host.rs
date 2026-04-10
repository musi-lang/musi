use music_bc::{EffectId, ForeignId};

use super::{Value, VmError, VmErrorKind, VmResult};

#[derive(Debug, Clone)]
pub struct ForeignCall {
    pub foreign: ForeignId,
    pub module: Box<str>,
    pub name: Box<str>,
    pub abi: Box<str>,
    pub symbol: Box<str>,
    pub link: Option<Box<str>>,
    pub param_count: u16,
}

#[derive(Debug, Clone)]
pub struct EffectCall {
    pub effect: EffectId,
    pub module: Box<str>,
    pub effect_name: Box<str>,
    pub op: u16,
    pub op_name: Box<str>,
    pub param_count: u16,
}

pub trait VmHost {
    /// Calls one foreign descriptor at one host boundary.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if host rejects one foreign call.
    fn call_foreign(&mut self, foreign: &ForeignCall, args: &[Value]) -> VmResult<Value>;

    /// Handles one effect invocation that escapes VM-owned execution.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if host rejects one effect call.
    fn handle_effect(&mut self, effect: &EffectCall, args: &[Value]) -> VmResult<Value>;
}

#[derive(Debug, Default)]
pub struct NativeHost;

impl VmHost for NativeHost {
    fn call_foreign(&mut self, foreign: &ForeignCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name.clone(),
        }))
    }

    fn handle_effect(&mut self, effect: &EffectCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name.clone(),
            op: Some(effect.op_name.clone()),
            reason: "host rejected effect call".into(),
        }))
    }
}
