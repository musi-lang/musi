#![allow(clippy::let_underscore_must_use)]

use std::iter;

use crate::arena::{Arena, Idx, IdxRange};

#[test]
fn test_alloc_single_item_round_trips_through_index() {
    let mut arena: Arena<u32> = Arena::new();
    let idx = arena.alloc(42u32);
    assert_eq!(arena[idx], 42);
}

#[test]
fn test_alloc_multiple_items_indices_are_distinct() {
    let mut arena: Arena<u32> = Arena::new();
    let a = arena.alloc(1u32);
    let b = arena.alloc(2u32);
    let c = arena.alloc(3u32);
    assert_ne!(a, b);
    assert_ne!(b, c);
    assert_ne!(a, c);
    assert_eq!(arena[a], 1);
    assert_eq!(arena[b], 2);
    assert_eq!(arena[c], 3);
}

#[test]
fn test_alloc_iter_produces_correct_idx_range() {
    let mut arena: Arena<u32> = Arena::new();
    let range = arena.alloc_iter([10u32, 20, 30]);
    assert_eq!(range.len(), 3);
    assert_eq!(&arena[range], &[10, 20, 30]);
}

#[test]
fn test_alloc_iter_empty_produces_empty_range() {
    let mut arena: Arena<u32> = Arena::new();
    let range = arena.alloc_iter(iter::empty::<u32>());
    assert!(range.is_empty());
    assert_eq!(range.len(), 0);
    assert_eq!(&arena[range], <&[u32]>::default());
}

#[test]
fn test_alloc_iter_after_existing_items_range_starts_at_correct_offset() {
    let mut arena: Arena<u32> = Arena::new();
    let _ = arena.alloc(99u32);
    let range = arena.alloc_iter([1u32, 2, 3]);
    assert_eq!(range.len(), 3);
    assert_eq!(&arena[range], &[1, 2, 3]);
}

#[test]
fn test_idx_range_iterator_yields_correct_idx_sequence() {
    let mut arena: Arena<i32> = Arena::new();
    let range = arena.alloc_iter([10i32, 20, 30]);
    let values: Vec<i32> = range.into_iter().map(|idx| arena[idx]).collect();
    assert_eq!(values, vec![10, 20, 30]);
}

#[test]
fn test_idx_range_iterator_exact_size() {
    let mut arena: Arena<i32> = Arena::new();
    let range = arena.alloc_iter([1i32, 2, 3, 4]);
    let iter = range.into_iter();
    assert_eq!(iter.len(), 4);
}

#[test]
fn test_idx_is_copy() {
    let mut arena: Arena<u32> = Arena::new();
    let idx = arena.alloc(7u32);
    let idx2 = idx; // copy
    assert_eq!(idx, idx2);
    assert_eq!(arena[idx], arena[idx2]);
}

#[test]
fn test_idx_partial_eq_same_raw() {
    let a: Idx<u32> = Idx::from_raw(5);
    let b: Idx<u32> = Idx::from_raw(5);
    assert_eq!(a, b);
}

#[test]
fn test_idx_partial_eq_different_raw() {
    let a: Idx<u32> = Idx::from_raw(1);
    let b: Idx<u32> = Idx::from_raw(2);
    assert_ne!(a, b);
}

#[test]
fn test_idx_raw_round_trips() {
    let idx: Idx<u32> = Idx::from_raw(42);
    assert_eq!(idx.raw(), 42);
}

#[test]
fn test_arena_is_empty_on_new() {
    let arena: Arena<u32> = Arena::new();
    assert!(arena.is_empty());
    assert_eq!(arena.len(), 0);
}

#[test]
fn test_arena_len_increases_with_each_alloc() {
    let mut arena: Arena<u32> = Arena::new();
    assert_eq!(arena.len(), 0);
    let _ = arena.alloc(1u32);
    assert_eq!(arena.len(), 1);
    let _ = arena.alloc(2u32);
    assert_eq!(arena.len(), 2);
}

#[test]
fn test_arena_is_not_empty_after_alloc() {
    let mut arena: Arena<u32> = Arena::new();
    let _ = arena.alloc(0u32);
    assert!(!arena.is_empty());
}

#[test]
fn test_index_mut_allows_mutation_through_index() {
    let mut arena: Arena<u32> = Arena::new();
    let idx = arena.alloc(0u32);
    arena[idx] = 99;
    assert_eq!(arena[idx], 99);
}

#[test]
fn test_idx_range_is_copy() {
    let mut arena: Arena<u32> = Arena::new();
    let range = arena.alloc_iter([1u32, 2, 3]);
    let range2 = range; // copy
    assert_eq!(range, range2);
}

#[test]
fn test_idx_range_new_len() {
    let range: IdxRange<u32> = IdxRange::new(3..7);
    assert_eq!(range.len(), 4);
    assert!(!range.is_empty());
}

#[test]
fn test_idx_range_new_empty() {
    let range: IdxRange<u32> = IdxRange::new(5..5);
    assert_eq!(range.len(), 0);
    assert!(range.is_empty());
}
