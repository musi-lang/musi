use crate::value::{Value, ValueList};
use alloc::vec::Vec;

const BASE_STACK_SIZE: usize = 4096;

pub struct Stack {
    pub data: ValueList,
}

impl Stack {
    pub unsafe fn new() -> Self {
        Self {
            data: Vec::with_capacity(BASE_STACK_SIZE),
        }
    }

    pub unsafe fn push(&mut self, val: Value) {
        self.data.push(val);
    }

    pub unsafe fn pop(&mut self) -> Value {
        self.data.pop().unwrap()
    }

    pub unsafe fn dup(&mut self) {
        let val = *self.data.last().unwrap();
        self.data.push(val);
    }
}
