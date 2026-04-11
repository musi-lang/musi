use std::collections::HashMap;
use std::iter::repeat_n;

use music_seam::{EffectId, MethodId};

use super::{ContinuationValuePtr, Program, Value, ValueList};

pub type LoadedModuleList = Vec<LoadedModule>;
pub type ModuleSlotMap = HashMap<Box<str>, usize>;
pub type CallFrameList = Vec<CallFrame>;
pub type EffectHandlerList = Vec<EffectHandler>;
pub type ResumeList = Vec<ContinuationValuePtr>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ModuleState {
    Uninitialized,
    Initializing,
    Initialized,
}

#[derive(Debug, Clone)]
pub struct LoadedModule {
    pub(crate) spec: Box<str>,
    pub(crate) program: Program,
    pub(crate) globals: ValueList,
    pub(crate) state: ModuleState,
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    pub(crate) module_slot: usize,
    pub(crate) method: MethodId,
    pub(crate) ip: usize,
    pub(crate) locals: ValueList,
    pub(crate) stack: ValueList,
}

#[derive(Debug, Clone)]
pub struct EffectHandler {
    pub(crate) handler_id: u64,
    pub(crate) effect: EffectId,
    pub(crate) handler: Value,
    pub(crate) frame_depth: usize,
    pub(crate) stack_depth: usize,
    pub(crate) pop_ip: usize,
}

pub enum StepOutcome {
    Continue,
    Return(Value),
}

impl LoadedModule {
    pub(crate) fn new(spec: impl Into<Box<str>>, program: Program) -> Self {
        let globals = repeat_n(Value::Unit, program.artifact().globals.len()).collect();
        Self {
            spec: spec.into(),
            program,
            globals,
            state: ModuleState::Uninitialized,
        }
    }

    pub(crate) const fn is_initialized(&self) -> bool {
        matches!(self.state, ModuleState::Initialized)
    }
}
