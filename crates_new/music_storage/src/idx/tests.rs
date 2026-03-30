use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use super::Idx;

#[test]
fn test_from_raw_round_trip() {
    let idx: Idx<u64> = Idx::from_raw(42);
    assert_eq!(idx.raw(), 42);
}

#[test]
fn test_copy_semantics() {
    let a: Idx<u64> = Idx::from_raw(7);
    let b = a;
    assert_eq!(a.raw(), 7);
    assert_eq!(b.raw(), 7);
}

#[test]
fn test_partial_eq_same_raw() {
    let a: Idx<i32> = Idx::from_raw(10);
    let b: Idx<i32> = Idx::from_raw(10);
    assert_eq!(a, b);
}

#[test]
fn test_partial_eq_different_raw() {
    let a: Idx<i32> = Idx::from_raw(1);
    let b: Idx<i32> = Idx::from_raw(2);
    assert_ne!(a, b);
}

#[test]
fn test_hash_consistency() {
    let a: Idx<i32> = Idx::from_raw(99);
    let b: Idx<i32> = Idx::from_raw(99);

    let hash_of = |idx: &Idx<i32>| -> u64 {
        let mut hasher = DefaultHasher::new();
        idx.hash(&mut hasher);
        hasher.finish()
    };

    assert_eq!(hash_of(&a), hash_of(&b));
}

#[test]
fn test_ordering_matches_raw() {
    let a: Idx<i32> = Idx::from_raw(1);
    let b: Idx<i32> = Idx::from_raw(2);
    let c: Idx<i32> = Idx::from_raw(1);

    assert!(a < b);
    assert!(b > a);
    assert_eq!(a.cmp(&c), Ordering::Equal);
    assert_eq!(a.partial_cmp(&b), Some(Ordering::Less));
}

#[test]
fn test_display_shows_raw_number() {
    let idx: Idx<u64> = Idx::from_raw(42);
    assert_eq!(format!("{idx}"), "42");
}

#[test]
fn test_ty_safety_distinct_tys() {
    let int_idx: Idx<u32> = Idx::from_raw(0);
    let str_idx: Idx<String> = Idx::from_raw(0);

    assert_eq!(int_idx.raw(), 0);
    assert_eq!(str_idx.raw(), 0);
}

#[test]
fn test_debug_format() {
    let idx: Idx<i32> = Idx::from_raw(42);
    assert_eq!(format!("{idx:?}"), "Idx { raw: 42 }");
}
