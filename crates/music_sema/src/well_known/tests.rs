//! Tests for well-known type registration.

use music_shared::Interner;

use crate::def::{DefKind, DefTable};
use crate::scope::ScopeTree;
use crate::well_known::init_well_known;

#[test]
fn test_init_well_known_registers_all_types() {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let root = scopes.push_root();

    let wk = init_well_known(&mut interner, &mut defs, root, &mut scopes);

    // All well-known DefIds should be distinct.
    let all = [
        wk.int, wk.int8, wk.int16, wk.int32, wk.int64, wk.uint8, wk.uint16, wk.uint32, wk.uint64,
        wk.float32, wk.float64, wk.string, wk.rune, wk.bool, wk.unit, wk.any, wk.never, wk.option,
    ];
    for (i, a) in all.iter().enumerate() {
        for b in &all[i + 1..] {
            assert_ne!(a, b, "well-known DefIds must be unique");
        }
    }
}

#[test]
fn test_init_well_known_types_are_def_kind_type() {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let root = scopes.push_root();

    let wk = init_well_known(&mut interner, &mut defs, root, &mut scopes);

    assert_eq!(defs.get(wk.int).kind, DefKind::Type);
    assert_eq!(defs.get(wk.bool).kind, DefKind::Type);
    assert_eq!(defs.get(wk.string).kind, DefKind::Type);
    assert_eq!(defs.get(wk.never).kind, DefKind::Type);
}

#[test]
fn test_init_well_known_names_resolve_in_scope() {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let root = scopes.push_root();

    let wk = init_well_known(&mut interner, &mut defs, root, &mut scopes);

    // "Int" should be resolvable from the root scope.
    let sym_int = interner.get("Int").expect("Int should be interned");
    assert_eq!(scopes.lookup(root, sym_int), Some(wk.int));

    let sym_bool = interner.get("Bool").expect("Bool should be interned");
    assert_eq!(scopes.lookup(root, sym_bool), Some(wk.bool));
}
