use music_bc::{EffectId, ForeignId, TypeId};
use music_term::TypeTerm;

use super::{Program, Value, VmError, VmErrorKind, VmResult};

#[derive(Debug, Clone)]
pub struct ForeignCall {
    pub(crate) program: Program,
    pub(crate) module: Box<str>,
    pub(crate) foreign: ForeignId,
    pub(crate) name: Box<str>,
    pub(crate) abi: Box<str>,
    pub(crate) symbol: Box<str>,
    pub(crate) link: Option<Box<str>>,
    pub(crate) param_tys: Box<[TypeId]>,
    pub(crate) result_ty: TypeId,
}

impl ForeignCall {
    #[must_use]
    pub fn module(&self) -> &str {
        &self.module
    }

    #[must_use]
    pub const fn foreign(&self) -> ForeignId {
        self.foreign
    }

    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub fn abi(&self) -> &str {
        &self.abi
    }

    #[must_use]
    pub fn symbol(&self) -> &str {
        &self.symbol
    }

    #[must_use]
    pub fn link(&self) -> Option<&str> {
        self.link.as_deref()
    }

    #[must_use]
    pub fn param_tys(&self) -> &[TypeId] {
        &self.param_tys
    }

    #[must_use]
    pub const fn result_ty(&self) -> TypeId {
        self.result_ty
    }

    #[must_use]
    pub fn param_ty_name(&self, index: usize) -> Option<&str> {
        self.param_tys
            .get(index)
            .map(|ty| self.program.type_name(*ty))
    }

    #[must_use]
    pub fn result_ty_name(&self) -> &str {
        self.program.type_name(self.result_ty)
    }

    #[must_use]
    pub fn type_name(&self, ty: TypeId) -> &str {
        self.program.type_name(ty)
    }

    #[must_use]
    pub fn type_term(&self, ty: TypeId) -> TypeTerm {
        self.program.type_term(ty)
    }
}

#[derive(Debug, Clone)]
pub struct EffectCall {
    pub(crate) program: Program,
    pub(crate) effect: EffectId,
    pub(crate) module: Box<str>,
    pub(crate) effect_name: Box<str>,
    pub(crate) op: u16,
    pub(crate) op_name: Box<str>,
    pub(crate) param_tys: Box<[TypeId]>,
    pub(crate) result_ty: TypeId,
}

impl EffectCall {
    #[must_use]
    pub fn module(&self) -> &str {
        &self.module
    }

    #[must_use]
    pub const fn effect(&self) -> EffectId {
        self.effect
    }

    #[must_use]
    pub fn effect_name(&self) -> &str {
        &self.effect_name
    }

    #[must_use]
    pub const fn op(&self) -> u16 {
        self.op
    }

    #[must_use]
    pub fn op_name(&self) -> &str {
        &self.op_name
    }

    #[must_use]
    pub fn param_tys(&self) -> &[TypeId] {
        &self.param_tys
    }

    #[must_use]
    pub const fn result_ty(&self) -> TypeId {
        self.result_ty
    }

    #[must_use]
    pub fn param_ty_name(&self, index: usize) -> Option<&str> {
        self.param_tys
            .get(index)
            .map(|ty| self.program.type_name(*ty))
    }

    #[must_use]
    pub fn result_ty_name(&self) -> &str {
        self.program.type_name(self.result_ty)
    }

    #[must_use]
    pub fn type_name(&self, ty: TypeId) -> &str {
        self.program.type_name(ty)
    }

    #[must_use]
    pub fn type_term(&self, ty: TypeId) -> TypeTerm {
        self.program.type_term(ty)
    }
}

pub trait VmHost {
    /// Handles one runtime foreign call.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when host foreign execution rejects or fails.
    fn call_foreign(&mut self, foreign: &ForeignCall, args: &[Value]) -> VmResult<Value>;

    /// Handles one unhandled runtime effect invocation.
    ///
    /// # Errors
    ///
    /// Returns [`VmError`] when host effect handling rejects or fails.
    fn handle_effect(&mut self, effect: &EffectCall, args: &[Value]) -> VmResult<Value>;
}

#[derive(Debug, Default, Clone, Copy)]
pub struct RejectingHost;

impl VmHost for RejectingHost {
    fn call_foreign(&mut self, foreign: &ForeignCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        }))
    }

    fn handle_effect(&mut self, effect: &EffectCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: "native host rejected runtime effect".into(),
        }))
    }
}
