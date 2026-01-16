use super::*;

#[test]
fn test_intern_same_string() {
    let mut interner = Interner::new();
    let id1 = interner.intern("hello");
    let id2 = interner.intern("hello");
    assert_eq!(id1, id2);
}

#[test]
fn test_intern_different_strings() {
    let mut interner = Interner::new();
    let id1 = interner.intern("hello");
    let id2 = interner.intern("world");
    assert_ne!(id1, id2);
}

#[test]
fn test_resolve() {
    let mut interner = Interner::new();
    let id = interner.intern("hello");
    assert_eq!(interner.resolve(id), "hello");
}

#[test]
fn test_empty_interner() {
    let interner = Interner::new();
    assert!(interner.is_empty());
    assert_eq!(interner.len(), 0);
}
