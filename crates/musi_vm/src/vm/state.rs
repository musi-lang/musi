use std::borrow::Cow;
use std::collections::HashMap;
use std::iter::repeat_n;
use std::ptr::null;
use std::slice::Iter;
use std::sync::Arc;

use music_seam::{EffectId, ProcedureId};
use smallvec::SmallVec;

use super::{GcRef, Program, RuntimeInstruction, RuntimeInstructionList, Value, ValueList};

pub type LoadedModuleList = SmallVec<[LoadedModule; 2]>;
pub type ModuleSlotMap = HashMap<Box<str>, usize>;
pub type ModuleSpec = Cow<'static, str>;
type RuntimeInstructionPtr = *const RuntimeInstruction;
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
    pub(crate) spec: ModuleSpec,
    pub(crate) program: Program,
    pub(crate) globals: ModuleGlobals,
    pub(crate) state: ModuleState,
}

#[derive(Debug, Clone)]
pub enum ModuleGlobals {
    Shared(Arc<[Value]>),
    Owned(Box<ValueList>),
}

impl ModuleGlobals {
    #[must_use]
    pub(crate) fn len(&self) -> usize {
        match self {
            Self::Shared(values) => values.len(),
            Self::Owned(values) => values.len(),
        }
    }

    #[must_use]
    pub(crate) fn get(&self, index: usize) -> Option<&Value> {
        match self {
            Self::Shared(values) => values.get(index),
            Self::Owned(values) => values.get(index),
        }
    }

    pub(crate) fn get_mut(&mut self, index: usize) -> Option<&mut Value> {
        self.make_owned().get_mut(index)
    }

    pub(crate) fn iter(&self) -> Iter<'_, Value> {
        match self {
            Self::Shared(values) => values.iter(),
            Self::Owned(values) => values.iter(),
        }
    }

    fn make_owned(&mut self) -> &mut ValueList {
        loop {
            match self {
                Self::Owned(values) => return values,
                Self::Shared(values) => {
                    let owned = values.iter().cloned().collect();
                    *self = Self::Owned(Box::new(owned));
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    pub(crate) module_slot: usize,
    pub(crate) procedure: ProcedureId,
    pub(crate) ip: usize,
    pub(crate) ip_ptr: RuntimeInstructionPtr,
    pub(crate) code_end_ptr: RuntimeInstructionPtr,
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
            ip_ptr: null(),
            code_end_ptr: null(),
            locals,
            stack,
            code: None,
        }
    }

    #[must_use]
    pub fn with_ip(mut self, ip: usize) -> Self {
        self.set_ip(ip);
        self
    }

    #[must_use]
    pub fn with_runtime_code(mut self, code: RuntimeInstructionList) -> Self {
        self.code = Some(code);
        self.rebind_ip_cache();
        self
    }

    pub(crate) fn set_runtime_code(&mut self, code: RuntimeInstructionList) {
        self.code = Some(code);
        self.rebind_ip_cache();
    }

    pub(crate) fn set_ip(&mut self, ip: usize) {
        self.ip = ip;
        self.rebind_ip_cache();
    }

    pub(crate) const fn advance_ip(&mut self) {
        self.ip = self.ip.saturating_add(1);
        self.ip_ptr = advance_ptr(self.ip_ptr);
    }

    pub(crate) fn next_runtime_instruction_cached(&mut self) -> Option<RuntimeInstruction> {
        if self.ip_ptr.is_null() || self.ip_ptr >= self.code_end_ptr {
            return None;
        }
        let instruction = read_instruction(self.ip_ptr);
        self.advance_ip();
        Some(instruction)
    }

    fn rebind_ip_cache(&mut self) {
        let code = self.code.as_deref().unwrap_or(&[]);
        let start = code.as_ptr();
        let end = add_ptr(start, code.len());
        self.code_end_ptr = end;
        self.ip_ptr = if self.ip <= code.len() {
            add_ptr(start, self.ip)
        } else {
            end
        };
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
        self.rebind_ip_cache();
    }

    pub(crate) fn recycle(&mut self) {
        self.ip = 0;
        self.ip_ptr = null();
        self.code_end_ptr = null();
        self.locals.clear();
        self.stack.clear();
        self.code = None;
    }
}

const fn add_ptr(base: RuntimeInstructionPtr, offset: usize) -> RuntimeInstructionPtr {
    // SAFETY: caller passes pointer derived from runtime-code slice; offset is checked by caller.
    unsafe { base.add(offset) }
}

const fn advance_ptr(ptr: RuntimeInstructionPtr) -> RuntimeInstructionPtr {
    // SAFETY: caller ensures pointer is within valid runtime-code range before advancing.
    unsafe { ptr.add(1) }
}

const fn read_instruction(ptr: RuntimeInstructionPtr) -> RuntimeInstruction {
    // SAFETY: caller ensures pointer is non-null and within runtime-code range.
    unsafe { ptr.read() }
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
    pub(crate) fn new(spec: impl Into<ModuleSpec>, program: Program) -> Self {
        let (globals, state) = program.global_init_image().map_or_else(
            || {
                (
                    ModuleGlobals::Owned(Box::new(
                        repeat_n(Value::Unit, program.artifact().globals.len()).collect(),
                    )),
                    ModuleState::Uninitialized,
                )
            },
            |image| {
                (
                    ModuleGlobals::Shared(Arc::clone(image)),
                    ModuleState::Initialized,
                )
            },
        );
        Self {
            spec: spec.into(),
            program,
            globals,
            state,
        }
    }

    pub(crate) const fn is_initialized(&self) -> bool {
        matches!(self.state, ModuleState::Initialized)
    }
}
