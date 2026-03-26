#![allow(clippy::unwrap_used)]

use super::{Heap, HeapObject};
use crate::value::Value;

#[test]
fn alloc_array_and_access() {
    let mut heap = Heap::new();
    let idx = heap.alloc_array(Value::UNIT, vec![Value::UNIT; 3]);
    let HeapObject::Array(arr) = heap.get(idx).unwrap() else {
        panic!("expected Array");
    };
    assert_eq!(arr.elements.len(), 3);
    assert_eq!(arr.tag, Value::UNIT);
    let HeapObject::Array(arr) = heap.get_mut(idx).unwrap() else {
        panic!("expected Array");
    };
    arr.elements[1] = Value::from_int(42);
    let HeapObject::Array(arr) = heap.get(idx).unwrap() else {
        panic!("expected Array");
    };
    assert_eq!(arr.elements[1].as_int(), 42);
}

#[test]
fn tagged_array() {
    let mut heap = Heap::new();
    let tag = Value::from_tag(7);
    let idx = heap.alloc_array(tag, vec![Value::UNIT; 2]);
    let HeapObject::Array(arr) = heap.get(idx).unwrap() else {
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
    let HeapObject::Array(arr) = heap.get(idx).unwrap() else {
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
    assert!(matches!(obj, HeapObject::Closure(_)));
}

#[test]
fn alloc_and_get() {
    let mut heap = Heap::new();
    let idx = heap.alloc_closure(3, vec![Value::from_int(42)]);
    assert_eq!(idx, 0);
    let HeapObject::Closure(cls) = heap.get(0).unwrap() else {
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
        let HeapObject::Closure(cls) = heap.get(idx).unwrap() else {
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
        let HeapObject::Closure(cls) = heap.get_mut(idx).unwrap() else {
            panic!("expected Closure");
        };
        cls.upvalues[0] = Value::from_int(99);
    }
    let HeapObject::Closure(cls) = heap.get(idx).unwrap() else {
        panic!("expected Closure");
    };
    assert_eq!(cls.upvalues[0].as_int(), 99);
}

#[test]
fn alloc_string_and_get() {
    let mut heap = Heap::new();
    let idx = heap.alloc_string("hello".into());
    let HeapObject::String(s) = heap.get(idx).unwrap() else {
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

    assert!(matches!(heap.get(str_idx).unwrap(), HeapObject::String(_)));
    assert!(matches!(heap.get(arr_idx).unwrap(), HeapObject::Array(_)));
    assert!(matches!(heap.get(cls_idx).unwrap(), HeapObject::Closure(_)));
}

#[test]
fn alloc_slice() {
    let mut heap = Heap::new();
    let arr_idx = heap.alloc_array(
        Value::UNIT,
        vec![
            Value::from_int(10),
            Value::from_int(20),
            Value::from_int(30),
        ],
    );
    let slice_idx = heap.alloc_slice(arr_idx, 1, 3);
    let HeapObject::Slice(sl) = heap.get(slice_idx).unwrap() else {
        panic!("expected Slice");
    };
    assert_eq!(sl.source, arr_idx);
    assert_eq!(sl.start, 1);
    assert_eq!(sl.end, 3);
}

#[test]
fn slice_bounds_access() {
    let mut heap = Heap::new();
    let elems = vec![
        Value::from_int(10),
        Value::from_int(20),
        Value::from_int(30),
        Value::from_int(40),
    ];
    let arr_idx = heap.alloc_array(Value::UNIT, elems);
    let slice_idx = heap.alloc_slice(arr_idx, 1, 3);

    let HeapObject::Slice(sl) = heap.get(slice_idx).unwrap() else {
        panic!("expected Slice");
    };
    assert_eq!(sl.end - sl.start, 2);
    let source = sl.source;
    let start = sl.start;

    let HeapObject::Array(arr) = heap.get(source).unwrap() else {
        panic!("expected Array");
    };
    assert_eq!(arr.elements[start].as_int(), 20);
    assert_eq!(arr.elements[start + 1].as_int(), 30);
}

// ── GC tests ──────────────────────────────────────────────────────────────────

#[test]
fn gc_mark_and_sweep() {
    let mut heap = Heap::new();
    let a = heap.alloc_string("keep".into());
    let b = heap.alloc_string("discard_1".into());
    let c = heap.alloc_string("discard_2".into());

    heap.mark_object(a);
    heap.sweep();

    assert!(heap.get(a).is_some(), "marked object should survive");
    assert!(heap.get(b).is_none(), "unmarked object should be freed");
    assert!(heap.get(c).is_none(), "unmarked object should be freed");
}

#[test]
fn gc_pin_survives_sweep() {
    let mut heap = Heap::new();
    let idx = heap.alloc_string("pinned".into());
    heap.pin(idx);

    heap.sweep();

    assert!(
        heap.get(idx).is_some(),
        "pinned object should survive sweep"
    );

    heap.unpin(idx);
    heap.sweep();

    assert!(
        heap.get(idx).is_none(),
        "unpinned object should be freed after sweep"
    );
}

#[test]
fn gc_reuses_freed_slots() {
    let mut heap = Heap::new();
    let a = heap.alloc_string("first".into());
    let b = heap.alloc_string("second".into());
    let _c = heap.alloc_string("third".into());

    heap.mark_object(b);
    heap.sweep();

    let d = heap.alloc_string("reused".into());
    assert!(
        d == a || d == 2,
        "new allocation should reuse freed slot, got idx {d}"
    );

    let HeapObject::String(s) = heap.get(d).unwrap() else {
        panic!("expected String");
    };
    assert_eq!(s, "reused");
}

#[test]
fn gc_traces_closure_upvalues() {
    let mut heap = Heap::new();
    let inner = heap.alloc_string("inner".into());
    let cls = heap.alloc_closure(0, vec![Value::from_ptr(inner)]);

    heap.mark_object(cls);
    heap.sweep();

    assert!(heap.get(cls).is_some(), "closure should survive");
    assert!(
        heap.get(inner).is_some(),
        "string reachable via upvalue should survive"
    );
}

#[test]
fn gc_traces_array_elements() {
    let mut heap = Heap::new();
    let s = heap.alloc_string("elem".into());
    let arr = heap.alloc_array(Value::UNIT, vec![Value::from_ptr(s), Value::from_int(1)]);

    heap.mark_object(arr);
    heap.sweep();

    assert!(heap.get(arr).is_some());
    assert!(heap.get(s).is_some(), "string in array should survive");
}

#[test]
fn gc_cycle_terminates() {
    let mut heap = Heap::new();
    let arr = heap.alloc_array(Value::UNIT, vec![Value::UNIT]);
    let HeapObject::Array(a) = heap.get_mut(arr).unwrap() else {
        panic!("expected Array");
    };
    a.elements[0] = Value::from_ptr(arr);

    heap.mark_object(arr);
    heap.sweep();

    assert!(heap.get(arr).is_some(), "cyclic array should survive");
}
