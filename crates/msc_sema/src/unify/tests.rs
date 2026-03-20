//! Tests for the unification table.

use msc_shared::{Arena, Interner, Span};

use crate::def::DefTable;
use crate::scope::ScopeTree;
use crate::types::{EffectRow, Type};
use crate::unify::UnifyTable;
use crate::well_known;
use crate::well_known::WellKnown;

fn make_well_known() -> (Interner, DefTable, ScopeTree, WellKnown) {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let root = scopes.push_root();
    let wk = well_known::init_well_known(&mut interner, &mut defs, root, &mut scopes);
    (interner, defs, scopes, wk)
}

#[test]
fn test_unify_fresh_var_binds() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let var = table.fresh(Span::DUMMY, &mut arena);
    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    assert!(table.unify(var, int_ty, &mut arena, &wk));
    let resolved = table.resolve(var, &arena);
    assert_eq!(resolved, int_ty);
}

#[test]
fn test_unify_same_named_succeeds() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let a = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let b = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    assert!(table.unify(a, b, &mut arena, &wk));
}

#[test]
fn test_unify_different_named_fails() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let a = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let b = arena.alloc(Type::Named {
        def: wk.bool,
        args: vec![],
    });

    assert!(!table.unify(a, b, &mut arena, &wk));
}

#[test]
fn test_unify_error_absorbs() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let err = arena.alloc(Type::Error);
    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    assert!(table.unify(err, int_ty, &mut arena, &wk));
    assert!(table.unify(int_ty, err, &mut arena, &wk));
}

#[test]
fn test_unify_occurs_check_fails() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let var = table.fresh(Span::DUMMY, &mut arena);
    // Create a function type that contains the variable itself.
    let fn_ty = arena.alloc(Type::Fn {
        params: vec![var],
        ret: var,
        effects: EffectRow::PURE,
    });

    assert!(!table.unify(var, fn_ty, &mut arena, &wk));
}

#[test]
fn test_unify_rigid_only_with_itself() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let (_, rigid) = table.fresh_rigid(Span::DUMMY, &mut arena);
    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    assert!(!table.unify(rigid, int_ty, &mut arena, &wk));
    // Rigid unifies with itself.
    assert!(table.unify(rigid, rigid, &mut arena, &wk));
}

#[test]
fn test_unify_fn_types_same_arity() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let bool_ty = arena.alloc(Type::Named {
        def: wk.bool,
        args: vec![],
    });

    let fn1 = arena.alloc(Type::Fn {
        params: vec![int_ty],
        ret: bool_ty,
        effects: EffectRow::PURE,
    });
    let fn2 = arena.alloc(Type::Fn {
        params: vec![int_ty],
        ret: bool_ty,
        effects: EffectRow::PURE,
    });

    assert!(table.unify(fn1, fn2, &mut arena, &wk));
}

#[test]
fn test_unify_fn_types_different_arity_fails() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    let fn1 = arena.alloc(Type::Fn {
        params: vec![int_ty],
        ret: int_ty,
        effects: EffectRow::PURE,
    });
    let fn2 = arena.alloc(Type::Fn {
        params: vec![int_ty, int_ty],
        ret: int_ty,
        effects: EffectRow::PURE,
    });

    assert!(!table.unify(fn1, fn2, &mut arena, &wk));
}

#[test]
fn test_unify_tuples() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let bool_ty = arena.alloc(Type::Named {
        def: wk.bool,
        args: vec![],
    });

    let tup1 = arena.alloc(Type::Tuple {
        elems: vec![int_ty, bool_ty],
    });
    let tup2 = arena.alloc(Type::Tuple {
        elems: vec![int_ty, bool_ty],
    });

    assert!(table.unify(tup1, tup2, &mut arena, &wk));
}

#[test]
fn test_freeze_replaces_solved_vars() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let var = table.fresh(Span::DUMMY, &mut arena);
    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    assert!(table.unify(var, int_ty, &mut arena, &wk));

    let frozen = table.freeze(var, &mut arena, wk.unknown);
    assert!(matches!(&arena[frozen], Type::Named { def, .. } if *def == wk.ints.int));
}

#[test]
fn test_freeze_unsolved_var_defaults_to_unknown() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let var = table.fresh(Span::DUMMY, &mut arena);
    let frozen = table.freeze(var, &mut arena, wk.unknown);
    assert!(
        matches!(&arena[frozen], Type::Named { def, args } if *def == wk.unknown && args.is_empty())
    );
}

#[test]
fn test_unify_any_does_not_unify_with_concrete() {
    // After the Any clean break, Any no longer silently unifies with everything.
    // Gradual typing boundaries are handled by consistency, not unification.
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let any_ty = arena.alloc(Type::Named {
        def: wk.any,
        args: vec![],
    });
    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    assert!(!table.unify(any_ty, int_ty, &mut arena, &wk));
}

#[test]
fn test_unify_anon_sum_same_types_succeeds() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let bool_ty = arena.alloc(Type::Named {
        def: wk.bool,
        args: vec![],
    });

    let sum1 = arena.alloc(Type::AnonSum {
        variants: vec![int_ty, bool_ty],
    });
    let sum2 = arena.alloc(Type::AnonSum {
        variants: vec![int_ty, bool_ty],
    });

    assert!(table.unify(sum1, sum2, &mut arena, &wk));
}

#[test]
fn test_unify_anon_sum_different_length_fails() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let bool_ty = arena.alloc(Type::Named {
        def: wk.bool,
        args: vec![],
    });

    let sum1 = arena.alloc(Type::AnonSum {
        variants: vec![int_ty, bool_ty],
    });
    let sum2 = arena.alloc(Type::AnonSum {
        variants: vec![int_ty],
    });

    assert!(!table.unify(sum1, sum2, &mut arena, &wk));
}

#[test]
fn test_unify_never_with_anything() {
    let (_, _, _, wk) = make_well_known();
    let mut table = UnifyTable::new();
    let mut arena = Arena::new();

    let never_ty = arena.alloc(Type::Named {
        def: wk.never,
        args: vec![],
    });
    let int_ty = arena.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    assert!(table.unify(never_ty, int_ty, &mut arena, &wk));
}
