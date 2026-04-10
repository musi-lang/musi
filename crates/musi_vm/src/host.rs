use music_bc::{EffectId, ForeignId};

use super::{Program, Value, VmError, VmErrorKind, VmResult};

#[derive(Debug, Clone)]
pub struct ForeignCall {
    pub foreign: ForeignId,
    pub name: Box<str>,
    pub abi: Box<str>,
    pub symbol: Box<str>,
    pub link: Option<Box<str>>,
}

#[derive(Debug, Clone)]
pub struct EffectCall {
    pub effect: EffectId,
    pub effect_name: Box<str>,
    pub op: u16,
    pub op_name: Box<str>,
}

pub trait VmHost {
    /// Loads one runtime module by specifier text.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if loading is rejected or fails.
    fn load_module(&mut self, spec: &str) -> VmResult<Program>;

    /// Calls one foreign target with runtime values.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if the foreign boundary rejects the call.
    fn call_foreign(&mut self, foreign: &ForeignCall, args: &[Value]) -> VmResult<Value>;

    /// Handles one effect invocation that escapes VM-owned execution.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if the host rejects the effect call.
    fn handle_effect(&mut self, effect: &EffectCall, args: &[Value]) -> VmResult<Value>;

    /// Evaluates one syntax value through the host compile/eval boundary.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] if syntax evaluation is unavailable or fails.
    fn eval_syntax(&mut self, syntax: &str) -> VmResult<Value>;
}

#[derive(Debug, Default)]
pub struct NativeHost;

impl VmHost for NativeHost {
    fn load_module(&mut self, spec: &str) -> VmResult<Program> {
        Err(VmError::new(VmErrorKind::ModuleLoadRejected {
            spec: spec.into(),
        }))
    }

    fn call_foreign(&mut self, foreign: &ForeignCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name.clone(),
        }))
    }

    fn handle_effect(&mut self, effect: &EffectCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name.clone(),
            op: Some(effect.op_name.clone()),
            reason: "host did not handle effect".into(),
        }))
    }

    fn eval_syntax(&mut self, syntax: &str) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::SyntaxEvalRejected {
            syntax: syntax.into(),
        }))
    }
}
