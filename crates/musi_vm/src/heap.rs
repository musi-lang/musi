use std::cell::RefCell;
use std::rc::Rc;

use crate::value::Value;

pub struct Closure {
    pub method_idx: u16,
    pub upvalues: Vec<Value>,
}

#[derive(Default)]
pub struct Heap {
    objects: Vec<Rc<RefCell<Closure>>>,
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
        self.objects.push(Rc::new(RefCell::new(Closure {
            method_idx,
            upvalues,
        })));
        idx
    }

    #[must_use]
    pub fn get_closure(&self, idx: usize) -> Option<Rc<RefCell<Closure>>> {
        self.objects.get(idx).cloned()
    }
}

#[cfg(test)]
mod tests;
