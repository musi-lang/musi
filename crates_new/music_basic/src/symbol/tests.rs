use super::{Interner, Symbol};

#[test]
fn intern_resolve_round_trip() {
    let mut interner = Interner::new();
    let sym = interner.intern("hello");
    assert_eq!(interner.resolve(sym), "hello");
}

#[test]
fn same_string_same_symbol() {
    let mut interner = Interner::new();
    let a = interner.intern("foo");
    let b = interner.intern("foo");
    assert_eq!(a, b);
}

#[test]
fn different_strings_different_symbols() {
    let mut interner = Interner::new();
    let a = interner.intern("foo");
    let b = interner.intern("bar");
    assert_ne!(a, b);
}

#[test]
fn len_tracks_unique_strings() {
    let mut interner = Interner::new();
    assert_eq!(interner.len(), 0);
    assert!(interner.is_empty());

    let _symbol = interner.intern("a");
    assert_eq!(interner.len(), 1);
    assert!(!interner.is_empty());

    let _symbol = interner.intern("a");
    assert_eq!(interner.len(), 1);

    let _symbol = interner.intern("b");
    assert_eq!(interner.len(), 2);
}

#[test]
fn display_shows_hash_format() {
    let sym = Symbol(42);
    assert_eq!(format!("{sym}"), "#42");
}

#[test]
fn raw_returns_inner_value() {
    let sym = Symbol(7);
    assert_eq!(sym.raw(), 7);
}

#[test]
fn default_interner_is_empty() {
    let interner = Interner::default();
    assert!(interner.is_empty());
    assert_eq!(interner.len(), 0);
}

#[test]
#[should_panic(expected = "symbol not found in interner")]
fn resolve_invalid_symbol_panics() {
    let interner = Interner::new();
    let bad = Symbol(999);
    let _resolved = interner.resolve(bad);
}

#[test]
fn with_capacity_behaves_like_new() {
    let mut interner = Interner::with_capacity(64);
    assert!(interner.is_empty());
    assert_eq!(interner.len(), 0);

    let sym = interner.intern("hello");
    assert_eq!(interner.resolve(sym), "hello");
    assert_eq!(interner.len(), 1);
}
