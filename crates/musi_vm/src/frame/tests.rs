#![allow(clippy::unwrap_used, clippy::panic)]

use super::CallFrame;
use crate::errors::VmError;
use crate::value::Value;

#[test]
fn push_and_pop() {
    let mut frame = CallFrame::new(0);
    frame.push(Value::from_int(7));
    assert_eq!(frame.pop().unwrap().as_int(), 7);
}

#[test]
fn pop_underflow() {
    let mut frame = CallFrame::new(0);
    assert!(matches!(frame.pop(), Err(VmError::StackUnderflow)));
}

#[test]
fn local_load_store() {
    let mut frame = CallFrame::new(2);
    frame.store_local(0, Value::from_int(10)).unwrap();
    frame.store_local(1, Value::from_bool(true)).unwrap();
    assert_eq!(frame.load_local(0).unwrap().as_int(), 10);
    assert!(frame.load_local(1).unwrap().as_bool());
}

#[test]
fn local_out_of_bounds() {
    let frame = CallFrame::new(1);
    assert!(matches!(frame.load_local(1), Err(VmError::InvalidLocal(1))));
}

#[test]
fn local_store_out_of_bounds() {
    let mut frame = CallFrame::new(1);
    assert!(matches!(
        frame.store_local(5, Value::UNIT),
        Err(VmError::InvalidLocal(5))
    ));
}

#[test]
fn dup() {
    let mut frame = CallFrame::new(0);
    frame.push(Value::from_int(3));
    frame.dup().unwrap();
    assert_eq!(frame.pop().unwrap().as_int(), 3);
    assert_eq!(frame.pop().unwrap().as_int(), 3);
}

#[test]
fn swap() {
    let mut frame = CallFrame::new(0);
    frame.push(Value::from_int(1));
    frame.push(Value::from_int(2));
    frame.swap().unwrap();
    assert_eq!(frame.pop().unwrap().as_int(), 1);
    assert_eq!(frame.pop().unwrap().as_int(), 2);
}

#[test]
fn rot() {
    let mut frame = CallFrame::new(0);
    frame.push(Value::from_int(1)); // a
    frame.push(Value::from_int(2)); // b
    frame.push(Value::from_int(3)); // c = TOS
    frame.rot().unwrap(); // → [b, c, a] = [2, 3, 1]
    assert_eq!(frame.pop().unwrap().as_int(), 1); // a is TOS
    assert_eq!(frame.pop().unwrap().as_int(), 3);
    assert_eq!(frame.pop().unwrap().as_int(), 2);
}

#[test]
fn peek_or_empty() {
    let frame = CallFrame::new(0);
    assert_eq!(frame.peek_or(Value::UNIT), Value::UNIT);
}
