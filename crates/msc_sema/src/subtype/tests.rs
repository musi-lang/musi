//! Tests for the subtyping relation.

use msc_shared::{Arena, Interner, Symbol};

use crate::def::DefTable;
use crate::scope::ScopeTree;
use crate::subtype::is_subtype;
use crate::types::{EffectRow, RecordField, Type};
use crate::unify::UnifyTable;
use crate::well_known::{WellKnown, init_well_known};

fn setup() -> (Arena<Type>, UnifyTable, WellKnown) {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let root = scopes.push_root();
    let wk = init_well_known(&mut interner, &mut defs, root, &mut scopes);
    (Arena::new(), UnifyTable::new(), wk)
}

#[test]
fn never_is_bottom() {
    let (mut types, unify, wk) = setup();

    let never = types.alloc(Type::Named {
        def: wk.never,
        args: vec![],
    });
    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let string = types.alloc(Type::Named {
        def: wk.string,
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

    assert!(is_subtype(never, int, &types, &unify, &wk));
    assert!(is_subtype(never, string, &types, &unify, &wk));
    assert!(is_subtype(never, fn_ty, &types, &unify, &wk));
}

#[test]
fn unknown_is_top() {
    let (mut types, unify, wk) = setup();

    let unknown = types.alloc(Type::Named {
        def: wk.unknown,
        args: vec![],
    });
    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let string = types.alloc(Type::Named {
        def: wk.string,
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

    assert!(is_subtype(int, unknown, &types, &unify, &wk));
    assert!(is_subtype(string, unknown, &types, &unify, &wk));
    assert!(is_subtype(fn_ty, unknown, &types, &unify, &wk));
}

#[test]
fn any_not_in_lattice() {
    let (mut types, unify, wk) = setup();

    let any = types.alloc(Type::Named {
        def: wk.any,
        args: vec![],
    });
    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    // Any is NOT a subtype of Int.
    assert!(!is_subtype(any, int, &types, &unify, &wk));
    // Int is NOT a subtype of Any.
    assert!(!is_subtype(int, any, &types, &unify, &wk));
    // Any is NOT a subtype of itself (Any does not participate in the lattice).
    assert!(!is_subtype(any, any, &types, &unify, &wk));
}

#[test]
fn fn_variance() {
    let (mut types, unify, wk) = setup();

    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let string = types.alloc(Type::Named {
        def: wk.string,
        args: vec![],
    });
    let never = types.alloc(Type::Named {
        def: wk.never,
        args: vec![],
    });
    let unknown = types.alloc(Type::Named {
        def: wk.unknown,
        args: vec![],
    });

    // (Unknown) -> Never <: (Int) -> String
    // Contravariant param: Int <: Unknown (holds, Unknown is top)
    // Covariant return: Never <: String (holds, Never is bottom)
    let covariant_fn = types.alloc(Type::Fn {
        params: vec![unknown],
        ret: never,
        effects: EffectRow::PURE,
    });
    let contravariant_fn = types.alloc(Type::Fn {
        params: vec![int],
        ret: string,
        effects: EffectRow::PURE,
    });

    assert!(is_subtype(
        covariant_fn,
        contravariant_fn,
        &types,
        &unify,
        &wk
    ));
}

#[test]
fn fn_variance_fail() {
    let (mut types, unify, wk) = setup();

    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let string = types.alloc(Type::Named {
        def: wk.string,
        args: vec![],
    });
    let never = types.alloc(Type::Named {
        def: wk.never,
        args: vec![],
    });
    let unknown = types.alloc(Type::Named {
        def: wk.unknown,
        args: vec![],
    });

    // (Int) -> String is NOT a subtype of (Unknown) -> Never
    // Contravariant param check: Unknown <: Int fails (Unknown is top, not bottom)
    let fn_a = types.alloc(Type::Fn {
        params: vec![int],
        ret: string,
        effects: EffectRow::PURE,
    });
    let fn_b = types.alloc(Type::Fn {
        params: vec![unknown],
        ret: never,
        effects: EffectRow::PURE,
    });

    assert!(!is_subtype(fn_a, fn_b, &types, &unify, &wk));
}

#[test]
fn record_width_subtype() {
    let (mut types, unify, wk) = setup();

    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let string = types.alloc(Type::Named {
        def: wk.string,
        args: vec![],
    });

    // Use Symbol indices that are stable - we need real interned symbols.
    // Since we can't access the interner here, use the def symbols directly from
    // the arena. Instead we build records using manually created symbols.
    // We use msc_shared::Symbol directly.
    let x = Symbol(0);
    let y = Symbol(1);

    // sub: { x: Int, y: String }
    let wide_rec = types.alloc(Type::Record {
        fields: vec![
            RecordField {
                name: x,
                ty: int,
                ty_params: vec![],
                binding: None,
            },
            RecordField {
                name: y,
                ty: string,
                ty_params: vec![],
                binding: None,
            },
        ],
        rest: None,
    });

    // sup: { x: Int }
    let narrow_rec = types.alloc(Type::Record {
        fields: vec![RecordField {
            name: x,
            ty: int,
            ty_params: vec![],
            binding: None,
        }],
        rest: None,
    });

    assert!(is_subtype(wide_rec, narrow_rec, &types, &unify, &wk));
    // Width subtyping is one-directional: narrow is NOT a subtype of wide.
    assert!(!is_subtype(narrow_rec, wide_rec, &types, &unify, &wk));
}

#[test]
fn record_depth_subtype() {
    let (mut types, unify, wk) = setup();

    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let never = types.alloc(Type::Named {
        def: wk.never,
        args: vec![],
    });

    let x = Symbol(0);

    // sub: { x: Never }
    let never_rec = types.alloc(Type::Record {
        fields: vec![RecordField {
            name: x,
            ty: never,
            ty_params: vec![],
            binding: None,
        }],
        rest: None,
    });

    // sup: { x: Int }
    let int_rec = types.alloc(Type::Record {
        fields: vec![RecordField {
            name: x,
            ty: int,
            ty_params: vec![],
            binding: None,
        }],
        rest: None,
    });

    // Never <: Int holds, so { x: Never } <: { x: Int }.
    assert!(is_subtype(never_rec, int_rec, &types, &unify, &wk));
}

#[test]
fn ref_invariant() {
    let (mut types, unify, wk) = setup();

    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });
    let never = types.alloc(Type::Named {
        def: wk.never,
        args: vec![],
    });

    let ref_int_a = types.alloc(Type::Ref { inner: int });
    let ref_int_b = types.alloc(Type::Ref { inner: int });
    let ref_never = types.alloc(Type::Ref { inner: never });

    // Ref(Int) <: Ref(Int) - same inner after resolve.
    assert!(is_subtype(ref_int_a, ref_int_b, &types, &unify, &wk));
    // Ref(Never) is NOT <: Ref(Int) - Ref is invariant.
    assert!(!is_subtype(ref_never, ref_int_a, &types, &unify, &wk));
}

#[test]
fn universe_subtype() {
    let (mut types, unify, wk) = setup();

    let u0 = types.alloc(Type::Universe { level: 0 });
    let u1 = types.alloc(Type::Universe { level: 1 });
    let u2 = types.alloc(Type::Universe { level: 2 });

    assert!(is_subtype(u0, u1, &types, &unify, &wk));
    assert!(is_subtype(u0, u2, &types, &unify, &wk));
    assert!(is_subtype(u1, u2, &types, &unify, &wk));
    assert!(!is_subtype(u1, u0, &types, &unify, &wk));
    assert!(!is_subtype(u2, u0, &types, &unify, &wk));
}

#[test]
fn error_absorbs() {
    let (mut types, unify, wk) = setup();

    let err = types.alloc(Type::Error);
    let int = types.alloc(Type::Named {
        def: wk.ints.int,
        args: vec![],
    });

    assert!(is_subtype(err, int, &types, &unify, &wk));
    assert!(is_subtype(int, err, &types, &unify, &wk));
}
