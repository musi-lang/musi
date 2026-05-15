use crate::artifact::{DataId, ProcedureId, StringId, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClosureDescriptor {
    pub name: StringId,
    pub procedure: ProcedureId,
    pub capture_count: u16,
    pub capture_tys: Box<[TypeId]>,
    pub env_layout: Option<DataId>,
    pub param_tys: Box<[TypeId]>,
    pub result_tys: Box<[TypeId]>,
    pub domain: Option<StringId>,
    pub effect: Option<StringId>,
    pub suspending: bool,
}

impl ClosureDescriptor {
    #[must_use]
    pub fn new(name: StringId, procedure: ProcedureId, capture_count: u16) -> Self {
        Self {
            name,
            procedure,
            capture_count,
            capture_tys: Box::new([]),
            env_layout: None,
            param_tys: Box::new([]),
            result_tys: Box::new([]),
            domain: None,
            effect: None,
            suspending: false,
        }
    }

    #[must_use]
    pub fn with_capture_tys(mut self, capture_tys: Box<[TypeId]>) -> Self {
        self.capture_tys = capture_tys;
        self
    }

    #[must_use]
    pub const fn with_env_layout(mut self, env_layout: DataId) -> Self {
        self.env_layout = Some(env_layout);
        self
    }

    #[must_use]
    pub fn with_param_tys(mut self, param_tys: Box<[TypeId]>) -> Self {
        self.param_tys = param_tys;
        self
    }

    #[must_use]
    pub fn with_result_tys(mut self, result_tys: Box<[TypeId]>) -> Self {
        self.result_tys = result_tys;
        self
    }

    #[must_use]
    pub const fn with_domain(mut self, domain: StringId) -> Self {
        self.domain = Some(domain);
        self
    }

    #[must_use]
    pub const fn with_effect(mut self, effect: StringId) -> Self {
        self.effect = Some(effect);
        self
    }

    #[must_use]
    pub const fn with_suspending(mut self, suspending: bool) -> Self {
        self.suspending = suspending;
        self
    }
}
