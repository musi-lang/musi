use crate::errors::{VmError, VmResult};
use crate::value::Value;

#[derive(Clone)]
pub struct CallFrame {
    locals: Vec<Value>,
    stack: Vec<Value>,
    /// PC to resume in the caller after this frame returns.
    pub return_pc: usize,
    /// Index into the `Vm`'s method table for the code being executed.
    pub method_idx: u16,
    /// Heap index of the closure this frame belongs to, if any.
    pub closure: Option<usize>,
}

impl CallFrame {
    #[must_use]
    pub fn new(locals_count: usize) -> Self {
        Self {
            locals: vec![Value::UNIT; locals_count],
            stack: Vec::new(),
            return_pc: 0,
            method_idx: 0,
            closure: None,
        }
    }

    #[must_use]
    pub fn new_call(
        locals_count: usize,
        return_pc: usize,
        method_idx: u16,
        closure: Option<usize>,
    ) -> Self {
        Self {
            locals: vec![Value::UNIT; locals_count],
            stack: Vec::new(),
            return_pc,
            method_idx,
            closure,
        }
    }

    pub fn push(&mut self, v: Value) {
        self.stack.push(v);
    }

    /// # Errors
    /// Returns `StackUnderflow` if the stack is empty.
    pub fn pop(&mut self) -> VmResult<Value> {
        self.stack.pop().ok_or(VmError::StackUnderflow)
    }

    /// # Errors
    /// Returns `StackUnderflow` if the stack is empty.
    pub fn peek(&self) -> VmResult<Value> {
        self.stack.last().copied().ok_or(VmError::StackUnderflow)
    }

    #[must_use]
    pub fn peek_or(&self, default: Value) -> Value {
        self.stack.last().copied().unwrap_or(default)
    }

    /// # Errors
    /// Returns `InvalidLocal` if `idx` is out of range.
    pub fn load_local(&self, idx: usize) -> VmResult<Value> {
        self.locals
            .get(idx)
            .copied()
            .ok_or(VmError::InvalidLocal(idx))
    }

    /// # Errors
    /// Returns `InvalidLocal` if `idx` is out of range.
    pub fn store_local(&mut self, idx: usize, v: Value) -> VmResult {
        let slot = self.locals.get_mut(idx).ok_or(VmError::InvalidLocal(idx))?;
        *slot = v;
        Ok(())
    }

    /// # Errors
    /// Returns `StackUnderflow` if the stack has fewer than one element.
    pub fn dup(&mut self) -> VmResult {
        let top = self.peek()?;
        self.push(top);
        Ok(())
    }

    /// # Errors
    /// Returns `StackUnderflow` if the stack has fewer than two elements.
    pub fn swap(&mut self) -> VmResult {
        let len = self.stack.len();
        if len < 2 {
            return Err(VmError::StackUnderflow);
        }
        self.stack.swap(len - 1, len - 2);
        Ok(())
    }

    #[must_use]
    pub const fn stack_depth(&self) -> usize {
        self.stack.len()
    }

    pub fn truncate_stack(&mut self, depth: usize) {
        self.stack.truncate(depth);
    }

    pub fn locals_iter(&self) -> impl Iterator<Item = Value> + '_ {
        self.locals.iter().copied()
    }

    pub fn stack_iter(&self) -> impl Iterator<Item = Value> + '_ {
        self.stack.iter().copied()
    }

    /// # Errors
    /// Returns `StackUnderflow` if the stack has fewer than three elements.
    pub fn rot(&mut self) -> VmResult {
        // Forth ROT: [a, b, c] (c=TOS) → [b, c, a] (a=TOS)
        let c = self.pop()?;
        let b = self.pop()?;
        let a = self.pop()?;
        self.push(b);
        self.push(c);
        self.push(a);
        Ok(())
    }
}

#[cfg(test)]
mod tests;
