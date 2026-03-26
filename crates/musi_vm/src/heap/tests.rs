#![allow(clippy::unwrap_used)]

use super::{is_nursery_idx, Heap, HeapObject};
use crate::value::Value;

// ── Nursery allocation tests ─────────────────────────────────────────────────

#[test]
fn nursery_alloc_uses_nursery_flag() {
    let mut heap = Heap::new();
    let idx = heap.alloc_array(Value::UNIT, vec![Value::UNIT; 3]);
    assert!(is_nursery_idx(idx), "first alloc should land in nursery");
    assert!(heap.get(idx).is_some());
}

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
    assert!(is_nursery_idx(idx), "first alloc should be nursery");
    let HeapObject::Closure(cls) = heap.get(idx).unwrap() else {
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
    assert!(is_nursery_idx(a));
    assert!(is_nursery_idx(b));
    assert!(is_nursery_idx(c));
    for (idx, expected_method) in [(a, 1), (b, 2), (c, 3)] {
        let HeapObject::Closure(cls) = heap.get(idx).unwrap() else {
            panic!("expected Closure");
        };
        assert_eq!(cls.method_idx, expected_method);
    }
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

// ── GC tests (use zero-capacity nursery for mature-only allocation) ──────────

fn mature_heap() -> Heap {
    Heap::new_with_nursery_capacity(0)
}

#[test]
fn gc_mark_and_sweep() {
    let mut heap = mature_heap();
    let a = heap.alloc_string("keep".into());
    let b = heap.alloc_string("discard_1".into());
    let c = heap.alloc_string("discard_2".into());

    assert!(!is_nursery_idx(a));
    heap.mark_object(a);
    heap.sweep();

    assert!(heap.get(a).is_some(), "marked object should survive");
    assert!(heap.get(b).is_none(), "unmarked object should be freed");
    assert!(heap.get(c).is_none(), "unmarked object should be freed");
}

#[test]
fn gc_pin_survives_sweep() {
    let mut heap = mature_heap();
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
    let mut heap = mature_heap();
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
    let mut heap = mature_heap();
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
    let mut heap = mature_heap();
    let s = heap.alloc_string("elem".into());
    let arr = heap.alloc_array(Value::UNIT, vec![Value::from_ptr(s), Value::from_int(1)]);

    heap.mark_object(arr);
    heap.sweep();

    assert!(heap.get(arr).is_some());
    assert!(heap.get(s).is_some(), "string in array should survive");
}

#[test]
fn gc_cycle_terminates() {
    let mut heap = mature_heap();
    let arr = heap.alloc_array(Value::UNIT, vec![Value::UNIT]);
    let HeapObject::Array(a) = heap.get_mut(arr).unwrap() else {
        panic!("expected Array");
    };
    a.elements[0] = Value::from_ptr(arr);

    heap.mark_object(arr);
    heap.sweep();

    assert!(heap.get(arr).is_some(), "cyclic array should survive");
}

// ── Minor GC (nursery) tests ─────────────────────────────────────────────────

#[test]
fn minor_collect_promotes_reachable() {
    let mut heap = Heap::new_with_nursery_capacity(4);
    let idx = heap.alloc_string("keep".into());
    assert!(is_nursery_idx(idx));

    let roots = vec![Value::from_ptr(idx)];
    heap.minor_collect(&roots);

    // fix_value resolves the forwarding pointer before clearing
    let fixed = heap.fix_value(Value::from_ptr(idx));
    assert!(!is_nursery_idx(fixed.as_ptr_idx()));

    heap.clear_nursery();

    let HeapObject::String(s) = heap.get(fixed.as_ptr_idx()).unwrap() else {
        panic!("expected String");
    };
    assert_eq!(s, "keep");
}

#[test]
fn minor_collect_frees_unreachable() {
    let mut heap = Heap::new_with_nursery_capacity(4);
    let _idx = heap.alloc_string("garbage".into());

    let roots: Vec<Value> = vec![];
    heap.minor_collect(&roots);
    heap.clear_nursery();

    assert_eq!(
        heap.live_count(),
        0,
        "unreachable nursery object should be freed"
    );
}

#[test]
fn minor_collect_fixes_forwarded_references() {
    let mut heap = Heap::new_with_nursery_capacity(4);
    let inner = heap.alloc_string("inner".into());
    let arr = heap.alloc_array(Value::UNIT, vec![Value::from_ptr(inner)]);

    let roots = vec![Value::from_ptr(arr)];
    heap.minor_collect(&roots);

    // fix_value while forwarding pointers still exist
    let fixed_arr = heap.fix_value(Value::from_ptr(arr));
    assert!(fixed_arr.is_ptr());
    let fixed_idx = fixed_arr.as_ptr_idx();
    assert!(
        !is_nursery_idx(fixed_idx),
        "promoted array should be in mature heap"
    );

    heap.clear_nursery();

    // The promoted array's element should also point to mature
    let HeapObject::Array(promoted_arr) = heap.get(fixed_idx).unwrap() else {
        panic!("expected Array");
    };
    assert!(promoted_arr.elements[0].is_ptr());
    assert!(
        !is_nursery_idx(promoted_arr.elements[0].as_ptr_idx()),
        "inner ref should be fixed to mature"
    );
}

#[test]
fn write_barrier_populates_remembered_set() {
    let mut heap = Heap::new_with_nursery_capacity(4);
    // Allocate in nursery first, then promote to get a mature object
    let mature_arr_nursery = heap.alloc_array(Value::UNIT, vec![Value::UNIT; 1]);
    let roots = vec![Value::from_ptr(mature_arr_nursery)];
    heap.minor_collect(&roots);
    let mature_arr = heap.fix_value(Value::from_ptr(mature_arr_nursery));
    let mature_idx = mature_arr.as_ptr_idx();
    heap.clear_nursery();

    // Store a nursery pointer into the mature array
    let nursery_str = heap.alloc_string("young".into());
    assert!(is_nursery_idx(nursery_str));

    // Update the mature array to point at the nursery string
    let HeapObject::Array(arr) = heap.get_mut(mature_idx).unwrap() else {
        panic!("expected Array");
    };
    arr.elements[0] = Value::from_ptr(nursery_str);

    // Write barrier: remember this mature object
    heap.remember(mature_idx);

    // Minor collect with no explicit roots -- nursery_str reachable via remembered set
    heap.minor_collect(&[]);
    heap.clear_nursery();

    // The mature array element should now point to a mature index
    let HeapObject::Array(arr) = heap.get(mature_idx).unwrap() else {
        panic!("expected Array");
    };
    assert!(arr.elements[0].is_ptr());
    assert!(
        !is_nursery_idx(arr.elements[0].as_ptr_idx()),
        "nursery ref should be promoted via remembered set"
    );
}

#[test]
fn nursery_overflow_falls_through_to_mature() {
    let mut heap = Heap::new_with_nursery_capacity(2);
    let a = heap.alloc_string("one".into());
    let b = heap.alloc_string("two".into());
    // Nursery is now full (capacity 2); next alloc goes to mature
    let c = heap.alloc_string("three".into());
    assert!(is_nursery_idx(a));
    assert!(is_nursery_idx(b));
    assert!(!is_nursery_idx(c), "overflow should go to mature heap");
}

#[test]
fn pin_nursery_object_survives_minor_collect() {
    let mut heap = Heap::new_with_nursery_capacity(4);
    let idx = heap.alloc_string("pinned_young".into());
    heap.pin(idx);

    // No roots, but pinned objects should survive
    heap.minor_collect(&[]);
    let fixed = heap.fix_value(Value::from_ptr(idx));
    heap.clear_nursery();

    assert!(!is_nursery_idx(fixed.as_ptr_idx()));
    let HeapObject::String(s) = heap.get(fixed.as_ptr_idx()).unwrap() else {
        panic!("expected String");
    };
    assert_eq!(s, "pinned_young");
}

#[test]
fn promote_all_nursery_before_major_gc() {
    let mut heap = Heap::new_with_nursery_capacity(4);
    let a = heap.alloc_string("a".into());
    let b = heap.alloc_string("b".into());
    assert!(is_nursery_idx(a));
    assert!(is_nursery_idx(b));

    heap.promote_all_nursery();

    let fixed_a = heap.fix_value(Value::from_ptr(a));
    let fixed_b = heap.fix_value(Value::from_ptr(b));
    heap.clear_nursery();

    assert!(!is_nursery_idx(fixed_a.as_ptr_idx()));
    assert!(!is_nursery_idx(fixed_b.as_ptr_idx()));

    let HeapObject::String(sa) = heap.get(fixed_a.as_ptr_idx()).unwrap() else {
        panic!("expected String");
    };
    assert_eq!(sa, "a");

    let HeapObject::String(sb) = heap.get(fixed_b.as_ptr_idx()).unwrap() else {
        panic!("expected String");
    };
    assert_eq!(sb, "b");
}

#[test]
fn live_count_includes_nursery() {
    let mut heap = Heap::new_with_nursery_capacity(4);
    let _a = heap.alloc_string("a".into());
    let _b = heap.alloc_string("b".into());
    assert_eq!(heap.live_count(), 2);
}
