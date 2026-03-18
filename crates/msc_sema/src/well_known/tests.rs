//! Tests for well-known type registration.

use msc_shared::Interner;

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
        wk.ints.int,
        wk.ints.int8,
        wk.ints.int16,
        wk.ints.int32,
        wk.ints.int64,
        wk.nats.nat,
        wk.nats.nat8,
        wk.nats.nat16,
        wk.nats.nat32,
        wk.nats.nat64,
        wk.floats.float32,
        wk.floats.float64,
        wk.ffi.c_string,
        wk.ffi.ptr,
        wk.effects.io,
        wk.effects.async_eff,
        wk.effects.state,
        wk.effects.throw,
        wk.primitives.isize_,
        wk.primitives.i8_,
        wk.primitives.i16_,
        wk.primitives.i32_,
        wk.primitives.i64_,
        wk.primitives.usize_,
        wk.primitives.u8_,
        wk.primitives.u16_,
        wk.primitives.u32_,
        wk.primitives.u64_,
        wk.primitives.fsize_,
        wk.primitives.f32_,
        wk.primitives.f64_,
        wk.primitives.bool_,
        wk.primitives.char_,
        wk.primitives.str_,
        wk.float,
        wk.string,
        wk.rune,
        wk.bool,
        wk.unit,
        wk.any,
        wk.never,
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

    assert_eq!(defs.get(wk.ints.int).kind, DefKind::Type);
    assert_eq!(defs.get(wk.bool).kind, DefKind::Type);
    assert_eq!(defs.get(wk.string).kind, DefKind::Type);
    assert_eq!(defs.get(wk.never).kind, DefKind::Type);
    assert_eq!(defs.get(wk.effects.io).kind, DefKind::Effect);
    assert_eq!(defs.get(wk.nats.nat).kind, DefKind::Type);
    assert_eq!(defs.get(wk.float).kind, DefKind::Type);
    assert_eq!(defs.get(wk.primitives.i32_).kind, DefKind::Primitive);
    assert_eq!(defs.get(wk.primitives.u64_).kind, DefKind::Primitive);
    assert_eq!(defs.get(wk.primitives.bool_).kind, DefKind::Primitive);
    assert_eq!(defs.get(wk.primitives.str_).kind, DefKind::Primitive);
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
    assert_eq!(scopes.lookup(root, sym_int), Some(wk.ints.int));

    let sym_bool = interner.get("Bool").expect("Bool should be interned");
    assert_eq!(scopes.lookup(root, sym_bool), Some(wk.bool));

    let sym_nat = interner.get("Nat").expect("Nat should be interned");
    assert_eq!(scopes.lookup(root, sym_nat), Some(wk.nats.nat));
}
