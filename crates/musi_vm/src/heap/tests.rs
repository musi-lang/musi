#![allow(clippy::unwrap_used)]

use super::layout::{unpack_addr, BLOCK_CAPACITY, CELLS_PER_LINE};
use super::{Heap, HeapObject};
use crate::value::Value;

#[test]
fn alloc_and_get() {
    let mut heap = Heap::new();
    let idx = heap.alloc_closure(3, vec![Value::from_int(42)]);
    let HeapObject::Closure(cls) = heap.get(idx).unwrap() else {
        panic!("expected Closure");
    };
    assert_eq!(cls.method_idx, 3);
    assert_eq!(cls.upvalues.len(), 1);
    assert_eq!(cls.upvalues[0].as_int(), 42);
}

#[test]
fn alloc_returns_packed_address() {
    let mut heap = Heap::new();
    let idx = heap.alloc_string("test".into());
    let (block_idx, cell_idx) = unpack_addr(idx);
    assert_eq!(block_idx, 0, "first alloc should be in block 0");
    assert_eq!(cell_idx, 0, "first alloc should be at cell 0");

    let idx2 = heap.alloc_string("test2".into());
    let (block_idx2, cell_idx2) = unpack_addr(idx2);
    assert_eq!(block_idx2, 0);
    assert_eq!(cell_idx2, 1);
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
fn multiple_closures() {
    let mut heap = Heap::new();
    let a = heap.alloc_closure(1, vec![]);
    let b = heap.alloc_closure(2, vec![Value::TRUE]);
    let c = heap.alloc_closure(3, vec![]);
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

#[test]
fn block_fills_then_acquires_new() {
    let mut heap = Heap::new();
    for _ in 0..BLOCK_CAPACITY {
        let _ = heap.alloc_string("x".into());
    }
    let idx = heap.alloc_string("overflow".into());
    let (block_idx, _cell_idx) = unpack_addr(idx);
    assert!(block_idx > 0, "should have acquired a new block");
}

#[test]
fn gc_mark_and_sweep() {
    let mut heap = Heap::new();
    let a = heap.alloc_string("keep".into());
    let _b = heap.alloc_string("discard_1".into());
    let _c = heap.alloc_string("discard_2".into());

    let roots = vec![Value::from_ptr(a)];
    heap.collect_major(&roots);

    assert!(heap.get(a).is_some(), "marked object should survive");
}

#[test]
fn sweep_drops_unmarked_objects() {
    let mut heap = Heap::new();
    let a = heap.alloc_string("keep".into());
    for _ in 1..CELLS_PER_LINE {
        let _ = heap.alloc_string("filler".into());
    }
    let b = heap.alloc_string("discard".into());

    let roots = vec![Value::from_ptr(a)];
    heap.collect_major(&roots);

    assert!(heap.get(a).is_some(), "rooted object should survive");
    assert!(
        heap.get(b).is_none(),
        "unrooted object on different line should be freed"
    );
}

#[test]
fn gc_pin_survives_sweep() {
    let mut heap = Heap::new();
    let idx = heap.alloc_string("pinned".into());
    heap.pin(idx);

    heap.collect_major(&[]);
    assert!(
        heap.get(idx).is_some(),
        "pinned object should survive sweep"
    );

    heap.unpin(idx);
    heap.collect_major(&[]);
    assert!(
        heap.get(idx).is_none(),
        "unpinned object should be freed after sweep"
    );
}

#[test]
fn gc_traces_closure_upvalues() {
    let mut heap = Heap::new();
    let inner = heap.alloc_string("inner".into());
    let cls = heap.alloc_closure(0, vec![Value::from_ptr(inner)]);

    let roots = vec![Value::from_ptr(cls)];
    heap.collect_major(&roots);

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

    let roots = vec![Value::from_ptr(arr)];
    heap.collect_major(&roots);

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

    let roots = vec![Value::from_ptr(arr)];
    heap.collect_major(&roots);

    assert!(heap.get(arr).is_some(), "cyclic array should survive");
}

#[test]
fn sticky_mark_persists() {
    let mut heap = Heap::new();
    let a = heap.alloc_string("old".into());
    let _b = heap.alloc_string("young_garbage".into());

    let roots = vec![Value::from_ptr(a)];
    heap.collect_minor(&roots);
    assert!(heap.is_marked(a), "a should be marked (old) after minor GC");

    heap.collect_minor(&roots);
    assert!(
        heap.get(a).is_some(),
        "old object should survive multiple minor GCs"
    );
}

#[test]
fn major_gc_clears_all_marks() {
    let mut heap = Heap::new();
    let a = heap.alloc_string("will_become_old".into());

    let roots = vec![Value::from_ptr(a)];
    heap.collect_minor(&roots);
    assert!(heap.is_marked(a));

    heap.collect_major(&roots);
    assert!(heap.get(a).is_some());
}

#[test]
fn recyclable_blocks_reused() {
    let mut heap = Heap::new();
    let mut addrs = Vec::new();
    for _ in 0..BLOCK_CAPACITY {
        addrs.push(heap.alloc_string("fill".into()));
    }
    let keep_count = BLOCK_CAPACITY / 4;
    let roots: Vec<Value> = addrs[..keep_count]
        .iter()
        .map(|&a| Value::from_ptr(a))
        .collect();
    heap.collect_major(&roots);

    let new_idx = heap.alloc_string("reused".into());
    assert!(heap.get(new_idx).is_some());
}

#[test]
fn line_marks_set_during_trace() {
    let mut heap = Heap::new();
    let a = heap.alloc_string("test".into());

    let roots = vec![Value::from_ptr(a)];
    heap.collect_minor(&roots);
    assert!(heap.is_marked(a));
}

#[test]
fn write_barrier_records_old_to_young() {
    let mut heap = Heap::new();
    let old_arr = heap.alloc_array(Value::UNIT, vec![Value::UNIT]);

    let roots = vec![Value::from_ptr(old_arr)];
    heap.collect_minor(&roots);
    assert!(heap.is_marked(old_arr));

    let young_str = heap.alloc_string("young".into());
    assert!(!heap.is_marked(young_str));

    heap.remember(old_arr);
    let HeapObject::Array(arr) = heap.get_mut(old_arr).unwrap() else {
        panic!("expected Array");
    };
    arr.elements[0] = Value::from_ptr(young_str);

    heap.collect_minor(&[]);
    assert!(
        heap.get(young_str).is_some(),
        "young object should survive via remembered set"
    );
}

#[test]
fn live_count_accurate() {
    let mut heap = Heap::new();
    let _a = heap.alloc_string("a".into());
    let _b = heap.alloc_string("b".into());
    assert_eq!(heap.live_count(), 2);
}

#[test]
fn minor_collect_frees_unreachable() {
    let mut heap = Heap::new();
    let _idx = heap.alloc_string("garbage".into());

    heap.collect_minor(&[]);

    assert_eq!(
        heap.live_count(),
        0,
        "unreachable object should be freed by minor GC"
    );
}

#[test]
fn large_object_alloc_and_collect() {
    let mut heap = Heap::new();
    let large_idx = heap.alloc_large(HeapObject::String("large_obj".into()));
    assert!(heap.get(large_idx).is_some());

    let roots = vec![Value::from_ptr(large_idx)];
    heap.collect_major(&roots);
    assert!(
        heap.get(large_idx).is_some(),
        "rooted large object survives major GC"
    );

    heap.collect_major(&[]);
    assert!(
        heap.get(large_idx).is_none(),
        "unrooted large object freed by major GC"
    );
}
