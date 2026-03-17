//! Tests for the type representation.

use music_shared::{Arena, Interner, Span};

use crate::def::{DefId, DefKind, DefTable};
use crate::types::{EffectRow, RecordField, TyVarId, Type, fmt_type};

#[test]
fn test_type_named_no_args() {
    let mut arena = Arena::new();
    let ty = arena.alloc(Type::Named {
        def: DefId(0),
        args: vec![],
    });
    assert!(matches!(&arena[ty], Type::Named { def: DefId(0), args } if args.is_empty()));
}

#[test]
fn test_type_fn_pure() {
    let mut arena = Arena::new();
    let int_ty = arena.alloc(Type::Named {
        def: DefId(0),
        args: vec![],
    });
    let fn_ty = arena.alloc(Type::Fn {
        params: vec![int_ty],
        ret: int_ty,
        effects: EffectRow::PURE,
    });
    assert!(
        matches!(&arena[fn_ty], Type::Fn { params, effects, .. } if params.len() == 1 && effects.is_pure())
    );
}

#[test]
fn test_type_tuple() {
    let mut arena = Arena::new();
    let a = arena.alloc(Type::Named {
        def: DefId(0),
        args: vec![],
    });
    let b = arena.alloc(Type::Named {
        def: DefId(1),
        args: vec![],
    });
    let tup = arena.alloc(Type::Tuple { elems: vec![a, b] });
    assert!(matches!(&arena[tup], Type::Tuple { elems } if elems.len() == 2));
}

#[test]
fn test_type_record_fields() {
    let mut interner = Interner::new();
    let mut arena = Arena::new();

    let sym_x = interner.intern("x");
    let int_ty = arena.alloc(Type::Named {
        def: DefId(0),
        args: vec![],
    });
    let rec = arena.alloc(Type::Record {
        fields: vec![RecordField {
            name: sym_x,
            ty: int_ty,
            ty_params: vec![],
        }],
        rest: None,
    });
    assert!(
        matches!(&arena[rec], Type::Record { fields, rest } if fields.len() == 1 && fields[0].name == sym_x && rest.is_none())
    );
}

#[test]
fn test_effect_row_pure() {
    let row = EffectRow::PURE;
    assert!(row.is_pure());
}

#[test]
fn test_effect_row_with_row_var_not_pure() {
    let row = EffectRow {
        effects: vec![],
        row_var: Some(TyVarId(0)),
    };
    assert!(!row.is_pure());
}

#[test]
fn test_type_error_absorbs() {
    let mut arena = Arena::new();
    let err = arena.alloc(Type::Error);
    assert!(matches!(&arena[err], Type::Error));
}

#[test]
fn test_type_var_and_rigid_distinct() {
    let mut arena = Arena::new();
    let var = arena.alloc(Type::Var(TyVarId(0)));
    let rigid = arena.alloc(Type::Rigid(TyVarId(0)));
    // Same TyVarId, but different Type variants.
    assert_ne!(&arena[var], &arena[rigid]);
}

#[test]
fn test_type_anon_sum() {
    let mut arena = Arena::new();
    let a = arena.alloc(Type::Named {
        def: DefId(0),
        args: vec![],
    });
    let b = arena.alloc(Type::Named {
        def: DefId(1),
        args: vec![],
    });
    let sum = arena.alloc(Type::AnonSum {
        variants: vec![a, b],
    });
    assert!(matches!(&arena[sum], Type::AnonSum { variants } if variants.len() == 2));
}

#[test]
fn test_fmt_type_anon_sum() {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();
    let mut arena = Arena::new();

    let sym_int = interner.intern("Int");
    let sym_str = interner.intern("String");
    let def_int = defs.alloc(sym_int, DefKind::Type, Span::DUMMY);
    let def_str = defs.alloc(sym_str, DefKind::Type, Span::DUMMY);
    let int_ty = arena.alloc(Type::Named {
        def: def_int,
        args: vec![],
    });
    let str_ty = arena.alloc(Type::Named {
        def: def_str,
        args: vec![],
    });
    let sum = arena.alloc(Type::AnonSum {
        variants: vec![int_ty, str_ty],
    });

    let defs_vec: Vec<_> = defs.iter().cloned().collect();
    let result = fmt_type(sum, &arena, &defs_vec, &interner, None);
    assert_eq!(&*result, "Int + String");
}

#[test]
fn test_fmt_type_named() {
    let mut interner = Interner::new();
    let mut defs = DefTable::new();
    let mut arena = Arena::new();

    let sym = interner.intern("Int");
    let def_id = defs.alloc(sym, DefKind::Type, Span::DUMMY);
    let ty = arena.alloc(Type::Named {
        def: def_id,
        args: vec![],
    });

    let defs_vec: Vec<_> = defs.iter().cloned().collect();
    let result = fmt_type(ty, &arena, &defs_vec, &interner, None);
    assert_eq!(&*result, "Int");
}
