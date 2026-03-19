//! Frame types and stack helpers.

use crate::VmResult;
use crate::error::{VmError, malformed};
use crate::value::Value;

/// An activation record for a single function invocation.
#[derive(Clone)]
pub struct Frame {
    /// Index into `module.functions`.
    pub fn_idx: usize,
    /// Byte offset of the *next* instruction to execute.
    pub ip: usize,
    /// Local variable slots (pre-zeroed, size = `local_count`).
    pub locals: Vec<Value>,
    /// Operand stack.
    pub stack: Vec<Value>,
    /// Active continuation markers (innermost last).
    pub marker_stack: Vec<ContMarker>,
    /// If this frame was entered via a closure call, the heap ref to the closure object.
    pub closure_ref: Option<Value>,
    /// Heap indices of open upvalue cells whose `frame_depth` points to this frame.
    /// Closed when this frame exits.
    pub open_upvalues: Vec<usize>,
}

/// An active continuation marker binding a marker id to its handler function.
#[derive(Clone, Copy, Debug)]
pub struct ContMarker {
    pub effect_id: u8,
    pub handler_fn_id: u32,
}

/// A captured one-shot continuation (frames between handler and `CONT_SAVE` site).
pub struct Continuation {
    pub frames: Vec<Frame>,
    /// The effect op id that triggered this continuation (for fatality checks on resume).
    pub op_id: u32,
}

impl Frame {
    /// Pop one value from the operand stack.
    ///
    /// # Errors
    ///
    /// Returns an error if the stack is empty.
    pub fn pop(&mut self) -> VmResult<Value> {
        self.stack
            .pop()
            .ok_or_else(|| malformed!("operand stack underflow"))
    }

    /// Pop two values: returns `(top, second)` i.e. `(b, a)` where `a` was pushed first.
    ///
    /// # Errors
    ///
    /// Returns an error if the stack has fewer than two values.
    pub fn pop2(&mut self) -> VmResult<(Value, Value)> {
        let b = self.pop()?;
        let a = self.pop()?;
        Ok((b, a))
    }

    /// Copy the top of the operand stack without removing it.
    ///
    /// # Errors
    ///
    /// Returns an error if the stack is empty.
    pub fn peek(&self) -> VmResult<Value> {
        self.stack
            .last()
            .copied()
            .ok_or_else(|| malformed!("peek on empty stack"))
    }

    /// Duplicate the top stack value.
    ///
    /// # Errors
    ///
    /// Returns an error if the stack is empty.
    pub fn dup(&mut self) -> VmResult {
        let top = self.peek()?;
        self.stack.push(top);
        Ok(())
    }

    /// Swap the top two stack values.
    ///
    /// # Errors
    ///
    /// Returns an error if the stack has fewer than two values.
    pub fn swp(&mut self) -> VmResult {
        let len = self.stack.len();
        if len < 2 {
            return Err(malformed!("swp requires at least 2 stack values"));
        }
        self.stack.swap(len - 1, len - 2);
        Ok(())
    }

    /// Read a local variable by slot index.
    ///
    /// # Errors
    ///
    /// Returns an error if `slot` is out of bounds for this frame's locals.
    pub fn get_local(&self, slot: usize) -> VmResult<Value> {
        self.locals.get(slot).copied().ok_or(VmError::OutOfBounds {
            index: slot,
            len: self.locals.len(),
        })
    }

    /// Write a local variable by slot index.
    ///
    /// # Errors
    ///
    /// Returns an error if `slot` is out of bounds for this frame's locals.
    pub fn set_local(&mut self, slot: usize, v: Value) -> VmResult {
        let len = self.locals.len();
        let dest = self
            .locals
            .get_mut(slot)
            .ok_or(VmError::OutOfBounds { index: slot, len })?;
        *dest = v;
        Ok(())
    }
}
