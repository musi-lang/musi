use super::KnownSymbols;
use crate::Interner;

#[test]
fn known_symbols_are_interned_with_canonical_spelling() {
    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    assert_eq!(interner.resolve(known.type_), "Type");
    assert_eq!(interner.resolve(known.abort_op), "abort");
    assert_eq!(interner.resolve(known.known), "known");
    assert_eq!(interner.resolve(known.intrinsic), "intrinsic");
    assert_eq!(interner.resolve(known.none), "None");
}

#[test]
fn compiler_prelude_contains_expected_symbols() {
    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    let prelude = known.compiler_prelude();
    assert_eq!(prelude.len(), 33);
    assert!(prelude.contains(&known.type_));
    assert!(prelude.contains(&known.array));
    assert!(prelude.contains(&known.nat));
    assert!(prelude.contains(&known.int_));
    assert!(prelude.contains(&known.int8));
    assert!(prelude.contains(&known.int16));
    assert!(prelude.contains(&known.int32));
    assert!(prelude.contains(&known.int64));
    assert!(prelude.contains(&known.nat8));
    assert!(prelude.contains(&known.nat16));
    assert!(prelude.contains(&known.nat32));
    assert!(prelude.contains(&known.nat64));
    assert!(prelude.contains(&known.float32));
    assert!(prelude.contains(&known.float64));
    assert!(prelude.contains(&known.rune));
    assert!(prelude.contains(&known.range));
    assert!(prelude.contains(&known.closed_range));
    assert!(prelude.contains(&known.partial_range_from));
    assert!(prelude.contains(&known.partial_range_up_to));
    assert!(prelude.contains(&known.partial_range_thru));
    assert!(prelude.contains(&known.rangeable));
    assert!(prelude.contains(&known.range_bounds));
    assert!(prelude.contains(&known.abort));
}
