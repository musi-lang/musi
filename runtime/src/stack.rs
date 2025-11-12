use crate::types::{Stack, Value};

pub const BASE_STACK_SIZE: usize = 4096;

pub unsafe fn stack_new() -> Stack {
    let data = Box::into_raw(Box::new(Vec::with_capacity(BASE_STACK_SIZE)));
    Stack { data }
}

pub unsafe fn stack_drop(stack: Stack) {
    drop(Box::from_raw(stack.data));
}

pub unsafe fn stack_push(stack: Stack, val: Value) {
    (*stack.data).push(val);
}

pub unsafe fn stack_pop(stack: Stack) -> Value {
    match (*stack.data).pop() {
        Some(val) => val,
        None => panic!("stack underflow"),
    }
}

pub unsafe fn stack_dup(stack: Stack) {
    let len = (*stack.data).len();
    if len == 0 {
        panic!("cannot duplicate empty stack");
    }
    let top_value = (&(*stack.data))[len - 1];
    (*stack.data).push(top_value);
}

pub unsafe fn stack_len(stack: Stack) -> usize {
    (*stack.data).len()
}

