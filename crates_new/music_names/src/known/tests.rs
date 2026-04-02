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

#[test]
fn compiler_prelude_contains_expected_symbols() {
    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    let prelude = known.compiler_prelude();
    assert_eq!(prelude.len(), 13);
    assert!(prelude.contains(&known.type_));
    assert!(prelude.contains(&known.int_));
    assert!(prelude.contains(&known.abort));
}
