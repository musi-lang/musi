#![allow(clippy::unwrap_used)]

use super::Heap;
use crate::value::Value;

#[test]
fn alloc_array_and_access() {
    let mut heap = Heap::new();
    let idx = heap.alloc_array(3);
    let arr = heap.get_array(idx).unwrap();
    assert_eq!(arr.borrow().elements.len(), 3);
    assert!(arr.borrow().tag.is_none());
    arr.borrow_mut().elements[1] = Value::from_int(42);
    assert_eq!(arr.borrow().elements[1].as_int(), 42);
}

#[test]
fn tagged_array() {
    let mut heap = Heap::new();
    let tag = Value::from_tag(7);
    let idx = heap.alloc_tagged_array(tag, 2);
    let arr = heap.get_array(idx).unwrap();
    assert_eq!(arr.borrow().elements.len(), 2);
    assert_eq!(arr.borrow().tag.unwrap(), tag);
}

#[test]
fn alloc_array_from_elements() {
    let mut heap = Heap::new();
    let elems = vec![Value::from_int(1), Value::from_int(2)];
    let idx = heap.alloc_array_from(None, elems);
    let arr = heap.get_array(idx).unwrap();
    assert_eq!(arr.borrow().elements[0].as_int(), 1);
    assert_eq!(arr.borrow().elements[1].as_int(), 2);
}

#[test]
fn get_array_wrong_type_returns_none() {
    let mut heap = Heap::new();
    let cls_idx = heap.alloc_closure(0, vec![]);
    assert!(heap.get_array(cls_idx).is_none());
}

#[test]
fn alloc_and_get() {
    let mut heap = Heap::new();
    let idx = heap.alloc_closure(3, vec![Value::from_int(42)]);
    assert_eq!(idx, 0);
    let cls = heap.get_closure(0).unwrap();
    let cls = cls.borrow();
    assert_eq!(cls.method_idx, 3);
    assert_eq!(cls.upvalues.len(), 1);
    assert_eq!(cls.upvalues[0].as_int(), 42);
}

#[test]
fn multiple_closures() {
    let mut heap = Heap::new();
    let a = heap.alloc_closure(1, vec![]);
    let b = heap.alloc_closure(2, vec![Value::TRUE]);
    let c = heap.alloc_closure(3, vec![]);
    assert_eq!(a, 0);
    assert_eq!(b, 1);
    assert_eq!(c, 2);
    assert_eq!(heap.get_closure(0).unwrap().borrow().method_idx, 1);
    assert_eq!(heap.get_closure(1).unwrap().borrow().method_idx, 2);
    assert_eq!(heap.get_closure(2).unwrap().borrow().method_idx, 3);
    assert!(heap.get_closure(3).is_none());
}

#[test]
fn upvalue_mutation() {
    let mut heap = Heap::new();
    let idx = heap.alloc_closure(0, vec![Value::from_int(10)]);
    {
        let cls = heap.get_closure(idx).unwrap();
        cls.borrow_mut().upvalues[0] = Value::from_int(99);
    }
    let cls = heap.get_closure(idx).unwrap();
    assert_eq!(cls.borrow().upvalues[0].as_int(), 99);
}
