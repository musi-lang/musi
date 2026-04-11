use crate::Symbol;
use super::Interner;

#[test]
fn interns_same_string_to_same_symbol() {
    let mut interner = Interner::new();
    let a = interner.intern("foo");
    let b = interner.intern("foo");
    assert_eq!(a, b);
    assert_eq!(interner.resolve(a), "foo");
}

#[test]
fn resolves_unknown_symbol_returns_none_for_try() {
    let interner = Interner::new();
    assert!(interner.try_resolve(Symbol::from_raw(0)).is_none());
}

#[test]
#[should_panic(expected = "unknown symbol")]
fn resolve_unknown_symbol_panics() {
    let interner = Interner::new();
    let _s = interner.resolve(Symbol::from_raw(0));
}
