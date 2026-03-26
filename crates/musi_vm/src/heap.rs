use std::cell::RefCell;
use std::rc::Rc;

use crate::effect::EffectHandler;
use crate::errors::VmError;
use crate::frame::CallFrame;
use crate::types::HeapIdx;
use crate::value::Value;

pub struct Closure {
    pub method_idx: u16,
    pub upvalues: Vec<Value>,
}

pub struct Continuation {
    pub frames: Vec<CallFrame>,
    pub resume_pc: usize,
    pub captured_handlers: Vec<EffectHandler>,
}

pub struct VmArray {
    pub tag: Value,
    pub elements: Vec<Value>,
}

pub enum HeapObject {
    Closure(Closure),
    Continuation(Continuation),
    Array(VmArray),
    String(String),
}

#[derive(Default)]
pub struct Heap {
    objects: Vec<Rc<RefCell<HeapObject>>>,
}

impl Heap {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    #[must_use]
    pub fn get(&self, idx: usize) -> Option<&Rc<RefCell<HeapObject>>> {
        self.objects.get(idx)
    }

    fn alloc(&mut self, obj: HeapObject) -> usize {
        let idx = self.objects.len();
        self.objects.push(Rc::new(RefCell::new(obj)));
        idx
    }

    pub fn alloc_closure(&mut self, method_idx: u16, upvalues: Vec<Value>) -> usize {
        self.alloc(HeapObject::Closure(Closure {
            method_idx,
            upvalues,
        }))
    }

    pub fn alloc_continuation(
        &mut self,
        frames: Vec<CallFrame>,
        resume_pc: usize,
        captured_handlers: Vec<EffectHandler>,
    ) -> usize {
        self.alloc(HeapObject::Continuation(Continuation {
            frames,
            resume_pc,
            captured_handlers,
        }))
    }

    pub fn alloc_array(&mut self, tag: Value, elements: Vec<Value>) -> usize {
        self.alloc(HeapObject::Array(VmArray { tag, elements }))
    }

    pub fn alloc_string(&mut self, data: String) -> usize {
        self.alloc(HeapObject::String(data))
    }

    /// Borrow a heap object immutably and apply `f`.
    ///
    /// # Errors
    /// Returns [`VmError::InvalidHeapIndex`] if `idx` is out of bounds,
    /// or whatever error `f` returns.
    pub fn with_obj<T>(
        &self,
        idx: HeapIdx,
        f: impl FnOnce(&HeapObject) -> Result<T, VmError>,
    ) -> Result<T, VmError> {
        let rc = self.get(idx).ok_or(VmError::InvalidHeapIndex(idx))?;
        let borrow = rc.borrow();
        f(&borrow)
    }

    /// Borrow a heap object mutably and apply `f`.
    ///
    /// # Errors
    /// Returns [`VmError::InvalidHeapIndex`] if `idx` is out of bounds,
    /// or whatever error `f` returns.
    pub fn with_obj_mut<T>(
        &self,
        idx: HeapIdx,
        f: impl FnOnce(&mut HeapObject) -> Result<T, VmError>,
    ) -> Result<T, VmError> {
        let rc = self.get(idx).ok_or(VmError::InvalidHeapIndex(idx))?;
        let mut borrow = rc.borrow_mut();
        f(&mut borrow)
    }
}

#[cfg(test)]
mod tests;
