use super::{Interner, Symbol};

#[test]
fn test_same_string_returns_same_symbol() {
    let mut interner = Interner::new();
    let a = interner.intern("hello");
    let b = interner.intern("hello");
    assert_eq!(a, b);
}

#[test]
fn test_different_strings_return_different_symbols() {
    let mut interner = Interner::new();
    let a = interner.intern("hello");
    let b = interner.intern("world");
    assert_ne!(a, b);
}

#[test]
fn test_resolve_roundtrip() {
    let mut interner = Interner::new();
    let cases = ["hello", "world", "musi", "foo bar"];
    for s in cases {
        let sym = interner.intern(s);
        assert_eq!(interner.resolve(sym), s);
    }
}

#[test]
fn test_empty_string_interns() {
    let mut interner = Interner::new();
    let sym = interner.intern("");
    assert_eq!(interner.resolve(sym), "");
}

#[test]
fn test_ordering_matches_insertion_order() {
    let mut interner = Interner::new();
    let a = interner.intern("alpha");
    let b = interner.intern("beta");
    let c = interner.intern("gamma");
    assert!(a < b);
    assert!(b < c);
    assert_eq!(a, Symbol(0));
    assert_eq!(b, Symbol(1));
    assert_eq!(c, Symbol(2));
}

#[test]
fn test_with_capacity_does_not_affect_correctness() {
    let mut interner = Interner::with_capacity(10);
    let a = interner.intern("hello");
    let b = interner.intern("hello");
    assert_eq!(a, b);
    assert_eq!(interner.resolve(a), "hello");
}

#[test]
fn test_len_tracks_unique_strings() {
    let mut interner = Interner::new();
    assert_eq!(interner.len(), 0);
    assert!(interner.is_empty());
    let _a = interner.intern("a");
    assert_eq!(interner.len(), 1);
    let _b = interner.intern("b");
    assert_eq!(interner.len(), 2);
    let _a2 = interner.intern("a");
    assert_eq!(interner.len(), 2); // deduped
}
