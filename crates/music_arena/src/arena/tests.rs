#![allow(clippy::unwrap_used, clippy::panic)]

use crate::Idx;

use super::Arena;

#[test]
fn alloc_returns_sequential_indices() {
    let mut arena: Arena<i32> = Arena::new();
    let a = arena.alloc(10);
    let b = arena.alloc(20);
    let c = arena.alloc(30);

    assert_eq!(a.raw(), 0);
    assert_eq!(b.raw(), 1);
    assert_eq!(c.raw(), 2);
}

#[test]
fn get_retrieves_correct_value() {
    let mut arena = Arena::new();
    let idx = arena.alloc(42);
    assert_eq!(*arena.get(idx), 42);
}

#[test]
fn get_mut_allows_mutation_that_persists() {
    let mut arena = Arena::new();
    let idx = arena.alloc(1);

    *arena.get_mut(idx) = 99;

    assert_eq!(*arena.get(idx), 99);
}

#[test]
fn len_increases_with_alloc() {
    let mut arena: Arena<i32> = Arena::new();
    assert_eq!(arena.len(), 0);

    let _a = arena.alloc(1);
    assert_eq!(arena.len(), 1);

    let _b = arena.alloc(2);
    assert_eq!(arena.len(), 2);
}

#[test]
fn is_empty_when_new_and_not_after_alloc() {
    let mut arena: Arena<i32> = Arena::new();
    assert!(arena.is_empty());

    let _idx = arena.alloc(1);
    assert!(!arena.is_empty());
}

#[test]
fn with_capacity_does_not_affect_behavior() {
    let mut arena: Arena<i32> = Arena::with_capacity(100);
    assert!(arena.is_empty());
    assert_eq!(arena.len(), 0);

    let idx = arena.alloc(7);
    assert_eq!(*arena.get(idx), 7);
    assert_eq!(arena.len(), 1);
}

#[test]
fn iter_yields_all_pairs_in_order() {
    let mut arena = Arena::new();
    let _a = arena.alloc(10);
    let _b = arena.alloc(20);
    let _c = arena.alloc(30);

    let pairs: Vec<(u32, i32)> = arena.iter().map(|(idx, val)| (idx.raw(), *val)).collect();

    assert_eq!(pairs, vec![(0, 10), (1, 20), (2, 30)]);
}

#[test]
fn iter_mut_yields_all_pairs_and_mutations_persist() {
    let mut arena = Arena::new();
    let _a = arena.alloc(1);
    let _b = arena.alloc(2);
    let _c = arena.alloc(3);

    for (_idx, val) in &mut arena {
        *val *= 10;
    }

    let values: Vec<i32> = arena.iter().map(|(_, val)| *val).collect();
    assert_eq!(values, vec![10, 20, 30]);
}

#[test]
fn default_creates_empty_arena() {
    let arena: Arena<i32> = Arena::default();
    assert!(arena.is_empty());
    assert_eq!(arena.len(), 0);
}

#[test]
fn multiple_types_coexist() {
    let mut int_arena: Arena<i32> = Arena::new();
    let mut str_arena: Arena<String> = Arena::new();

    let int_idx = int_arena.alloc(42);
    let str_idx = str_arena.alloc(String::from("hello"));

    assert_eq!(*int_arena.get(int_idx), 42);
    assert_eq!(str_arena.get(str_idx), "hello");
}

#[test]
#[should_panic(expected = "out of bounds")]
fn get_out_of_bounds() {
    let arena: Arena<i32> = Arena::new();
    let _val = arena.get(Idx::from_raw(0));
}

#[test]
#[should_panic(expected = "out of bounds")]
fn get_mut_out_of_bounds() {
    let mut arena: Arena<i32> = Arena::new();
    let _val = arena.get_mut(Idx::from_raw(0));
}

#[test]
fn empty_arena_iteration() {
    let arena: Arena<i32> = Arena::new();
    assert_eq!(arena.iter().count(), 0);
}

#[test]
fn debug_format() {
    let mut arena = Arena::new();
    let _idx = arena.alloc(42);
    let debug_output = format!("{arena:?}");
    assert!(debug_output.contains("Arena"));
    assert!(debug_output.contains("42"));
}
