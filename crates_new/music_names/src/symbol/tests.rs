use super::*;

#[test]
fn test_interning_same_text_returns_same_symbol() {
    let mut interner = Interner::new();
    let first = interner.intern("alpha");
    let second = interner.intern("alpha");

    assert_eq!(first, second);
}

#[test]
fn test_interning_different_text_returns_different_symbols() {
    let mut interner = Interner::new();
    let first = interner.intern("alpha");
    let second = interner.intern("beta");

    assert_ne!(first, second);
}

#[test]
fn test_resolve_returns_original_text() {
    let mut interner = Interner::new();
    let symbol = interner.intern("hello");

    assert_eq!(interner.resolve(symbol), "hello");
}

#[test]
fn test_synthetic_preserves_raw_value() {
    let symbol = Symbol::synthetic(42);
    assert_eq!(symbol.raw(), 42);
}

#[test]
fn test_default_is_empty() {
    let interner = Interner::default();
    assert!(interner.is_empty());
}
