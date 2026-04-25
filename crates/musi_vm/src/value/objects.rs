use music_seam::{ForeignId, ProcedureId, TypeId};

use crate::value::scalar::ValueList;

#[derive(Debug, Clone)]
pub struct DataValue {
    pub(crate) ty: TypeId,
    pub(crate) tag: i64,
    pub(crate) fields: ValueList,
}

impl DataValue {
    #[must_use]
    pub const fn new(ty: TypeId, tag: i64, fields: ValueList) -> Self {
        Self { ty, tag, fields }
    }
}

#[derive(Debug, Clone)]
pub struct ClosureValue {
    pub(crate) module_slot: usize,
    pub(crate) procedure: ProcedureId,
    pub(crate) params: u16,
    pub(crate) locals: u16,
    pub(crate) captures: ValueList,
}

impl ClosureValue {
    #[must_use]
    pub const fn new(module_slot: usize, procedure: ProcedureId, captures: ValueList) -> Self {
        Self {
            module_slot,
            procedure,
            params: 0,
            locals: 0,
            captures,
        }
    }

    #[must_use]
    pub const fn with_shape(mut self, params: u16, locals: u16) -> Self {
        self.params = params;
        self.locals = locals;
        self
    }

    #[must_use]
    pub fn param_count(&self) -> usize {
        usize::from(self.params)
    }

    #[must_use]
    pub fn local_count(&self) -> usize {
        usize::from(self.locals.max(self.params))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ProcedureValue {
    pub(crate) module_slot: usize,
    pub(crate) procedure: ProcedureId,
    pub(crate) params: u16,
    pub(crate) locals: u16,
}

impl ProcedureValue {
    #[must_use]
    pub const fn new(module_slot: usize, procedure: ProcedureId, params: u16, locals: u16) -> Self {
        Self {
            module_slot,
            procedure,
            params,
            locals,
        }
    }

    #[must_use]
    pub const fn module_slot(self) -> usize {
        self.module_slot
    }

    #[must_use]
    pub const fn procedure(self) -> ProcedureId {
        self.procedure
    }
}

#[derive(Debug, Clone)]
pub struct ModuleValue {
    pub(crate) spec: Box<str>,
    pub(crate) slot: usize,
}

impl ModuleValue {
    #[must_use]
    pub fn new(spec: impl Into<Box<str>>, slot: usize) -> Self {
        Self {
            spec: spec.into(),
            slot,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignValue {
    pub(crate) module_slot: usize,
    pub(crate) foreign: ForeignId,
    pub(crate) type_args: Box<[TypeId]>,
}

impl ForeignValue {
    #[must_use]
    pub fn new(module_slot: usize, foreign: ForeignId) -> Self {
        Self {
            module_slot,
            foreign,
            type_args: Box::default(),
        }
    }

    #[must_use]
    pub fn with_type_args(mut self, type_args: impl Into<Box<[TypeId]>>) -> Self {
        self.type_args = type_args.into();
        self
    }
}
