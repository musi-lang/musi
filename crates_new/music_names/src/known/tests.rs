use super::KnownSymbols;
use crate::Interner;

#[test]
fn known_symbols_are_interned_with_canonical_spelling() {
    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    assert_eq!(interner.resolve(known.type_), "Type");
    assert_eq!(interner.resolve(known.abort_op), "abort");
    assert_eq!(interner.resolve(known.none), "None");
}
