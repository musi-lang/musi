#![allow(clippy::unwrap_used)]

use super::{Heap, HeapObject};
use crate::value::Value;

#[test]
fn alloc_array_and_access() {
    let mut heap = Heap::new();
    let idx = heap.alloc_array(Value::UNIT, vec![Value::UNIT; 3]);
    let obj = heap.get(idx).unwrap();
    let borrow = obj.borrow();
    let HeapObject::Array(arr) = &*borrow else {
        panic!("expected Array");
    };
    assert_eq!(arr.elements.len(), 3);
    assert_eq!(arr.tag, Value::UNIT);
    drop(borrow);
    {
        let mut borrow = obj.borrow_mut();
        let HeapObject::Array(arr) = &mut *borrow else {
            panic!("expected Array");
        };
        arr.elements[1] = Value::from_int(42);
    }
    let borrow = obj.borrow();
    let HeapObject::Array(arr) = &*borrow else {
        panic!("expected Array");
    };
    assert_eq!(arr.elements[1].as_int(), 42);
}

#[test]
fn tagged_array() {
    let mut heap = Heap::new();
    let tag = Value::from_tag(7);
    let idx = heap.alloc_array(tag, vec![Value::UNIT; 2]);
    let obj = heap.get(idx).unwrap();
    let borrow = obj.borrow();
    let HeapObject::Array(arr) = &*borrow else {
        panic!("expected Array");
    };
    assert_eq!(arr.elements.len(), 2);
    assert_eq!(arr.tag, tag);
}

#[test]
fn alloc_array_from_elements() {
    let mut heap = Heap::new();
    let elems = vec![Value::from_int(1), Value::from_int(2)];
    let idx = heap.alloc_array(Value::UNIT, elems);
    let obj = heap.get(idx).unwrap();
    let borrow = obj.borrow();
    let HeapObject::Array(arr) = &*borrow else {
        panic!("expected Array");
    };
    assert_eq!(arr.elements[0].as_int(), 1);
    assert_eq!(arr.elements[1].as_int(), 2);
}

#[test]
fn get_array_wrong_type_returns_closure() {
    let mut heap = Heap::new();
    let cls_idx = heap.alloc_closure(0, vec![]);
    let obj = heap.get(cls_idx).unwrap();
    let borrow = obj.borrow();
    assert!(matches!(&*borrow, HeapObject::Closure(_)));
}

#[test]
fn alloc_and_get() {
    let mut heap = Heap::new();
    let idx = heap.alloc_closure(3, vec![Value::from_int(42)]);
    assert_eq!(idx, 0);
    let obj = heap.get(0).unwrap();
    let borrow = obj.borrow();
    let HeapObject::Closure(cls) = &*borrow else {
        panic!("expected Closure");
    };
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
    for (idx, expected_method) in [(0, 1), (1, 2), (2, 3)] {
        let obj = heap.get(idx).unwrap();
        let borrow = obj.borrow();
        let HeapObject::Closure(cls) = &*borrow else {
            panic!("expected Closure");
        };
        assert_eq!(cls.method_idx, expected_method);
    }
    assert!(heap.get(3).is_none());
}

#[test]
fn upvalue_mutation() {
    let mut heap = Heap::new();
    let idx = heap.alloc_closure(0, vec![Value::from_int(10)]);
    {
        let obj = heap.get(idx).unwrap();
        let mut borrow = obj.borrow_mut();
        let HeapObject::Closure(cls) = &mut *borrow else {
            panic!("expected Closure");
        };
        cls.upvalues[0] = Value::from_int(99);
    }
    let obj = heap.get(idx).unwrap();
    let borrow = obj.borrow();
    let HeapObject::Closure(cls) = &*borrow else {
        panic!("expected Closure");
    };
    assert_eq!(cls.upvalues[0].as_int(), 99);
}

#[test]
fn alloc_string_and_get() {
    let mut heap = Heap::new();
    let idx = heap.alloc_string("hello".into());
    let obj = heap.get(idx).unwrap();
    let borrow = obj.borrow();
    let HeapObject::String(s) = &*borrow else {
        panic!("expected String");
    };
    assert_eq!(s, "hello");
}

#[test]
fn type_discrimination() {
    let mut heap = Heap::new();
    let str_idx = heap.alloc_string("test".into());
    let arr_idx = heap.alloc_array(Value::UNIT, vec![]);
    let cls_idx = heap.alloc_closure(0, vec![]);

    assert!(matches!(
        &*heap.get(str_idx).unwrap().borrow(),
        HeapObject::String(_)
    ));
    assert!(matches!(
        &*heap.get(arr_idx).unwrap().borrow(),
        HeapObject::Array(_)
    ));
    assert!(matches!(
        &*heap.get(cls_idx).unwrap().borrow(),
        HeapObject::Closure(_)
    ));
}
