use std::cell::RefCell;
use std::rc::Rc;

use crate::frame::CallFrame;
use crate::value::Value;

pub struct Closure {
    pub method_idx: u16,
    pub upvalues: Vec<Value>,
}

pub struct Continuation {
    pub frames: Vec<CallFrame>,
    pub resume_pc: usize,
}

enum HeapObject {
    Closure(Rc<RefCell<Closure>>),
    Continuation(Continuation),
}

#[derive(Default)]
pub struct Heap {
    objects: Vec<HeapObject>,
}

impl Heap {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            objects: Vec::new(),
        }
    }

    pub fn alloc_closure(&mut self, method_idx: u16, upvalues: Vec<Value>) -> usize {
        let idx = self.objects.len();
        self.objects
            .push(HeapObject::Closure(Rc::new(RefCell::new(Closure {
                method_idx,
                upvalues,
            }))));
        idx
    }

    #[must_use]
    pub fn get_closure(&self, idx: usize) -> Option<Rc<RefCell<Closure>>> {
        match self.objects.get(idx)? {
            HeapObject::Closure(c) => Some(Rc::clone(c)),
            HeapObject::Continuation(_) => None,
        }
    }

    pub fn alloc_continuation(&mut self, frames: Vec<CallFrame>, resume_pc: usize) -> usize {
        let idx = self.objects.len();
        self.objects
            .push(HeapObject::Continuation(Continuation { frames, resume_pc }));
        idx
    }

    #[must_use]
    pub fn get_continuation(&self, idx: usize) -> Option<&Continuation> {
        match self.objects.get(idx)? {
            HeapObject::Continuation(c) => Some(c),
            HeapObject::Closure(_) => None,
        }
    }
}

#[cfg(test)]
mod tests;
