use std::collections::HashMap;
use std::iter::repeat_n;

use music_seam::{EffectId, ProcedureId};

use super::{GcRef, Program, RuntimeInstruction, RuntimeInstructionList, Value, ValueList};

pub type LoadedModuleList = Vec<LoadedModule>;
pub type ModuleSlotMap = HashMap<Box<str>, usize>;
pub type CallFrameList = Vec<CallFrame>;
pub type EffectHandlerList = Vec<EffectHandler>;
pub type ResumeList = Vec<GcRef>;

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
    pub(crate) procedure: ProcedureId,
    pub(crate) ip: usize,
    pub(crate) locals: ValueList,
    pub(crate) stack: ValueList,
    pub(crate) code: Option<RuntimeInstructionList>,
}

impl CallFrame {
    #[must_use]
    pub const fn new(
        module_slot: usize,
        procedure: ProcedureId,
        locals: ValueList,
        stack: ValueList,
    ) -> Self {
        Self {
            module_slot,
            procedure,
            ip: 0,
            locals,
            stack,
            code: None,
        }
    }

    #[must_use]
    pub const fn with_ip(mut self, ip: usize) -> Self {
        self.ip = ip;
        self
    }

    #[must_use]
    pub fn with_runtime_code(mut self, code: RuntimeInstructionList) -> Self {
        self.code = Some(code);
        self
    }

    pub(crate) fn set_runtime_code(&mut self, code: RuntimeInstructionList) {
        self.code = Some(code);
    }

    pub(crate) fn runtime_instruction(&self) -> Option<RuntimeInstruction> {
        self.code.as_ref()?.get(self.ip).copied()
    }

    pub(crate) fn reset_empty(
        &mut self,
        module_slot: usize,
        procedure: ProcedureId,
        code: RuntimeInstructionList,
    ) {
        self.module_slot = module_slot;
        self.procedure = procedure;
        self.ip = 0;
        self.locals.clear();
        self.stack.clear();
        self.code = Some(code);
    }

    pub(crate) fn recycle(&mut self) {
        self.ip = 0;
        self.locals.clear();
        self.stack.clear();
        self.code = None;
    }
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

impl EffectHandler {
    #[must_use]
    pub const fn new(handler_id: u64, effect: EffectId, handler: Value) -> Self {
        Self {
            handler_id,
            effect,
            handler,
            frame_depth: 0,
            stack_depth: 0,
            pop_ip: 0,
        }
    }

    #[must_use]
    pub const fn with_stack_state(
        mut self,
        frame_depth: usize,
        stack_depth: usize,
        pop_ip: usize,
    ) -> Self {
        self.frame_depth = frame_depth;
        self.stack_depth = stack_depth;
        self.pop_ip = pop_ip;
        self
    }
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
