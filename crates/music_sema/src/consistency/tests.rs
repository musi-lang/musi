//! Tests for the consistency relation.

use music_shared::{Arena, Symbol};

use crate::consistency::is_consistent;
use crate::def::DefTable;
use crate::scope::ScopeTree;
use crate::types::{EffectRow, RecordField, Type};
use crate::unify::UnifyTable;
use crate::well_known::{init_well_known, WellKnown};

fn setup() -> (Arena<Type>, UnifyTable, WellKnown) {
    let mut interner = music_shared::Interner::new();
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let root = scopes.push_root();
    let wk = init_well_known(&mut interner, &mut defs, root, &mut scopes);
    (Arena::new(), UnifyTable::new(), wk)
}

#[test]
fn any_consistent_with_all() {
    let (mut types, unify, wk) = setup();

    let any = types.alloc(Type::Named {
        def: wk.any,
        args: vec![],
    });
    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let bool_ty = types.alloc(Type::Named {
        def: wk.bool,
        args: vec![],
    });
    let fn_ty = types.alloc(Type::Fn {
        params: vec![int],
        ret: bool_ty,
        effects: EffectRow::PURE,
    });

    // Any ~ Int
    assert!(is_consistent(any, int, &types, &unify, &wk));
    // Int ~ Any
    assert!(is_consistent(int, any, &types, &unify, &wk));
    // Any ~ (Int) -> Bool
    assert!(is_consistent(any, fn_ty, &types, &unify, &wk));
    // (Int) -> Bool ~ Any
    assert!(is_consistent(fn_ty, any, &types, &unify, &wk));
}

#[test]
fn not_transitive() {
    let (mut types, unify, wk) = setup();

    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let string = types.alloc(Type::Named {
        def: wk.string,
        args: vec![],
    });
    let any = types.alloc(Type::Named {
        def: wk.any,
        args: vec![],
    });

    // Int ~ Any and Any ~ String hold individually...
    assert!(is_consistent(int, any, &types, &unify, &wk));
    assert!(is_consistent(any, string, &types, &unify, &wk));
    // ...but Int ~ String does NOT hold (consistency is NOT transitive)
    assert!(!is_consistent(int, string, &types, &unify, &wk));
}

#[test]
fn structural_consistency() {
    let (mut types, unify, wk) = setup();

    let any = types.alloc(Type::Named {
        def: wk.any,
        args: vec![],
    });
    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let bool_ty = types.alloc(Type::Named {
        def: wk.bool,
        args: vec![],
    });

    // (Any) -> Int ~ (Bool) -> Int — param positions are Any, so consistent
    let fn_any_int = types.alloc(Type::Fn {
        params: vec![any],
        ret: int,
        effects: EffectRow::PURE,
    });
    let fn_bool_int = types.alloc(Type::Fn {
        params: vec![bool_ty],
        ret: int,
        effects: EffectRow::PURE,
    });

    assert!(is_consistent(fn_any_int, fn_bool_int, &types, &unify, &wk));
    assert!(is_consistent(fn_bool_int, fn_any_int, &types, &unify, &wk));

    // (Int) -> Int is NOT consistent with (Bool) -> Int (neither is Any)
    let fn_int_int = types.alloc(Type::Fn {
        params: vec![int],
        ret: int,
        effects: EffectRow::PURE,
    });
    assert!(!is_consistent(fn_int_int, fn_bool_int, &types, &unify, &wk));
}

#[test]
fn record_consistency() {
    let (mut types, unify, wk) = setup();

    let any = types.alloc(Type::Named {
        def: wk.any,
        args: vec![],
    });
    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    let x = Symbol(0);

    // { x: Any } ~ { x: Int }
    let rec_any = types.alloc(Type::Record {
        fields: vec![RecordField {
            name: x,
            ty: any,
            ty_params: vec![],
            binding: None,
        }],
        rest: None,
    });
    let rec_int = types.alloc(Type::Record {
        fields: vec![RecordField {
            name: x,
            ty: int,
            ty_params: vec![],
            binding: None,
        }],
        rest: None,
    });

    assert!(is_consistent(rec_any, rec_int, &types, &unify, &wk));
    assert!(is_consistent(rec_int, rec_any, &types, &unify, &wk));
}

#[test]
fn error_absorbs() {
    let (mut types, unify, wk) = setup();

    let err = types.alloc(Type::Error);
    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    assert!(is_consistent(err, int, &types, &unify, &wk));
    assert!(is_consistent(int, err, &types, &unify, &wk));
}
