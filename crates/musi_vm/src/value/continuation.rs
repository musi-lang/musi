use music_seam::{EffectId, ProcedureId};
use smallvec::SmallVec;

use crate::value::scalar::{Value, ValueList};

pub type ContinuationFrameList = SmallVec<[ContinuationFrame; 4]>;
pub type ContinuationHandlerList = SmallVec<[ContinuationHandler; 4]>;

#[derive(Debug, Clone)]
pub struct ContinuationFrame {
    pub(crate) module_slot: usize,
    pub(crate) procedure: ProcedureId,
    pub(crate) ip: usize,
    pub(crate) locals: ValueList,
    pub(crate) stack: ValueList,
}

impl ContinuationFrame {
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
        }
    }

    #[must_use]
    pub const fn with_ip(mut self, ip: usize) -> Self {
        self.ip = ip;
        self
    }
}

#[derive(Debug, Clone)]
pub struct ContinuationHandler {
    pub(crate) handler_id: u64,
    pub(crate) effect: EffectId,
    pub(crate) handler: Value,
    pub(crate) frame_depth: usize,
    pub(crate) stack_depth: usize,
    pub(crate) pop_ip: usize,
}

impl ContinuationHandler {
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

#[derive(Debug, Clone)]
pub struct ContinuationValue {
    pub(crate) frames: ContinuationFrameList,
    pub(crate) handlers: ContinuationHandlerList,
}

impl ContinuationValue {
    #[must_use]
    pub const fn new(frames: ContinuationFrameList, handlers: ContinuationHandlerList) -> Self {
        Self { frames, handlers }
    }
}
