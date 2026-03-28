use crate::pool::{ConstantEntry, ConstantPool};

#[test]
fn add_returns_sequential_indices() {
    let mut pool = ConstantPool::new();
    let i0 = pool.add(ConstantEntry::Int(42));
    let i1 = pool.add(ConstantEntry::Float(0x4009_21FB_5444_2D18));
    let i2 = pool.add(ConstantEntry::Str(String::from("hello")));
    assert_eq!(i0, 0);
    assert_eq!(i1, 1);
    assert_eq!(i2, 2);
    assert_eq!(pool.len(), 3);
}

#[test]
fn dedup_returns_same_index() {
    let mut pool = ConstantPool::new();
    let first = pool.add(ConstantEntry::Int(99));
    let second = pool.add(ConstantEntry::Int(99));
    assert_eq!(first, second);
    assert_eq!(pool.len(), 1);
}

#[test]
fn dedup_distinguishes_types() {
    let mut pool = ConstantPool::new();
    let int_idx = pool.add(ConstantEntry::Int(1));
    // Float with bits that happen to equal 1 as u64 -- distinct from Int(1)
    let float_idx = pool.add(ConstantEntry::Float(1));
    assert_ne!(int_idx, float_idx);
    assert_eq!(pool.len(), 2);
}

#[test]
fn entries_preserves_insertion_order() {
    let mut pool = ConstantPool::new();
    let _i0 = pool.add(ConstantEntry::Str(String::from("a")));
    let _i1 = pool.add(ConstantEntry::Int(10));
    let _i2 = pool.add(ConstantEntry::Str(String::from("b")));

    let entries = pool.entries();
    assert!(entries.len() > 2);
    assert_eq!(entries[0], ConstantEntry::Str(String::from("a")));
    assert_eq!(entries[1], ConstantEntry::Int(10));
    assert_eq!(entries[2], ConstantEntry::Str(String::from("b")));
}

#[test]
fn empty_pool() {
    let pool = ConstantPool::new();
    assert!(pool.is_empty());
    assert_eq!(pool.len(), 0);
    assert!(pool.entries().is_empty());
}

#[test]
fn default_is_empty() {
    let pool = ConstantPool::default();
    assert!(pool.is_empty());
}
