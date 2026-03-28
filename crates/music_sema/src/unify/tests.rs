use music_builtins::types::BuiltinType;
use music_shared::{Interner, Span};

use crate::env::TypeEnv;
use crate::errors::SemaErrorKind;
use crate::types::{NominalKey, Ty};
use crate::unify::unify;

const SPAN: Span = Span::DUMMY;

fn seeded_env() -> TypeEnv {
    let mut env = TypeEnv::new();
    env.seed_builtins();
    env
}

#[test]
fn same_type_unifies() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let result = unify(&mut env, int, int, SPAN).unwrap();
    assert_eq!(result, int);
}

#[test]
fn var_binds_to_concrete() {
    let mut env = seeded_env();
    let var = env.fresh_var();
    let int = env.builtin(BuiltinType::Int);
    let result = unify(&mut env, var, int, SPAN).unwrap();
    assert_eq!(result, int);
    assert_eq!(env.resolve_var(var), int);
}

#[test]
fn var_binds_to_var_chain() {
    let mut env = seeded_env();
    let v1 = env.fresh_var();
    let v2 = env.fresh_var();
    let int = env.builtin(BuiltinType::Int);

    let _r1 = unify(&mut env, v1, v2, SPAN).unwrap();
    let _r2 = unify(&mut env, v2, int, SPAN).unwrap();

    assert_eq!(env.resolve_var(v1), int);
    assert_eq!(env.resolve_var(v2), int);
}

#[test]
fn any_consistent_with_anything() {
    let mut env = seeded_env();
    let any = env.intern(Ty::Any);
    let int = env.builtin(BuiltinType::Int);

    let r1 = unify(&mut env, any, int, SPAN).unwrap();
    assert_eq!(r1, int);

    let r2 = unify(&mut env, int, any, SPAN).unwrap();
    assert_eq!(r2, int);
}

#[test]
fn never_subtype_of_anything() {
    let mut env = seeded_env();
    let empty = env.intern(Ty::Empty);
    let int = env.builtin(BuiltinType::Int);

    let result = unify(&mut env, empty, int, SPAN).unwrap();
    assert_eq!(result, int);
}

#[test]
fn anything_subtype_of_unknown() {
    let mut env = seeded_env();
    let unknown = env.intern(Ty::Unknown);
    let int = env.builtin(BuiltinType::Int);

    let result = unify(&mut env, int, unknown, SPAN).unwrap();
    assert_eq!(result, int);
}

#[test]
fn builtin_mismatch_is_error() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let float = env.builtin(BuiltinType::Float);

    let err = unify(&mut env, int, float, SPAN).unwrap_err();
    assert!(matches!(
        err.kind,
        SemaErrorKind::TypeMismatch {
            expected: _,
            found: _
        }
    ));
}

#[test]
fn arrow_unification_contravariant_params() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let bool_ty = env.builtin(BuiltinType::Bool);
    let var = env.fresh_var();

    let arrow1 = env.intern(Ty::Arrow {
        param: int,
        ret: bool_ty,
    });
    let arrow2 = env.intern(Ty::Arrow {
        param: var,
        ret: bool_ty,
    });

    let result = unify(&mut env, arrow1, arrow2, SPAN).unwrap();
    let result_ty = env.types.get(result);
    match result_ty {
        Ty::Arrow { param, ret } => {
            // param should be unified: var was bound to int (contravariant:
            // unify(p2=var, p1=int) binds var to int)
            assert_eq!(env.resolve_var(*param), int);
            assert_eq!(*ret, bool_ty);
        }
        other => panic!("expected Arrow, got {other:?}"),
    }
}

#[test]
fn tuple_length_mismatch_is_error() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);

    let t1 = env.intern(Ty::Tuple(vec![int, int]));
    let t2 = env.intern(Ty::Tuple(vec![int]));

    let err = unify(&mut env, t1, t2, SPAN).unwrap_err();
    assert!(matches!(
        err.kind,
        SemaErrorKind::ArityMismatch {
            expected: 2,
            found: 1
        }
    ));
}

#[test]
fn tuple_pairwise_unification() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let var = env.fresh_var();

    let t1 = env.intern(Ty::Tuple(vec![int, int]));
    let t2 = env.intern(Ty::Tuple(vec![var, int]));

    let result = unify(&mut env, t1, t2, SPAN).unwrap();
    match env.types.get(result) {
        Ty::Tuple(elems) => {
            assert_eq!(elems.len(), 2);
            assert_eq!(env.resolve_var(elems[0]), int);
        }
        other => panic!("expected Tuple, got {other:?}"),
    }
}

#[test]
fn occurs_check_prevents_infinite_type() {
    let mut env = seeded_env();
    let var = env.fresh_var();

    // Try to unify var with Arrow(var, Int) -- should fail occurs check
    let int = env.builtin(BuiltinType::Int);
    let arrow = env.intern(Ty::Arrow {
        param: var,
        ret: int,
    });

    let err = unify(&mut env, var, arrow, SPAN).unwrap_err();
    assert!(matches!(err.kind, SemaErrorKind::OccursCheck { var: 0 }));
}

#[test]
fn record_width_subtyping() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let int = env.builtin(BuiltinType::Int);
    let bool_ty = env.builtin(BuiltinType::Bool);
    let x_sym = interner.intern("x");
    let y_sym = interner.intern("y");

    // {x: Int, y: Bool} should unify with {x: Int} (wider has more fields)
    let wide = env.intern(Ty::Record {
        fields: vec![(x_sym, int), (y_sym, bool_ty)],
    });
    let narrow = env.intern(Ty::Record {
        fields: vec![(x_sym, int)],
    });

    let result = unify(&mut env, wide, narrow, SPAN).unwrap();
    match env.types.get(result) {
        Ty::Record { fields } => {
            assert_eq!(fields.len(), 2);
        }
        other => panic!("expected Record, got {other:?}"),
    }
}

#[test]
fn record_missing_field_is_error() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let int = env.builtin(BuiltinType::Int);
    let x_sym = interner.intern("x");
    let y_sym = interner.intern("y");

    let r1 = env.intern(Ty::Record {
        fields: vec![(x_sym, int)],
    });
    let r2 = env.intern(Ty::Record {
        fields: vec![(y_sym, int)],
    });

    let err = unify(&mut env, r1, r2, SPAN).unwrap_err();
    assert!(matches!(err.kind, SemaErrorKind::UndefinedField { .. }));
}

#[test]
fn mut_invariant_unification() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let float = env.builtin(BuiltinType::Float);

    let m1 = env.intern(Ty::Mut(int));
    let m2 = env.intern(Ty::Mut(int));

    let result = unify(&mut env, m1, m2, SPAN).unwrap();
    match env.types.get(result) {
        Ty::Mut(inner) => assert_eq!(env.resolve_var(*inner), int),
        other => panic!("expected Mut, got {other:?}"),
    }

    let m3 = env.intern(Ty::Mut(float));
    let err = unify(&mut env, m1, m3, SPAN).unwrap_err();
    assert!(matches!(err.kind, SemaErrorKind::TypeMismatch { .. }));
}

#[test]
fn param_same_name_unifies() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let t = interner.intern("T");

    let p1 = env.intern(Ty::Param(t));
    let p2 = env.intern(Ty::Param(t));

    let result = unify(&mut env, p1, p2, SPAN).unwrap();
    assert_eq!(*env.types.get(result), Ty::Param(t));
}

#[test]
fn param_different_name_is_error() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let t = interner.intern("T");
    let u = interner.intern("U");

    let p1 = env.intern(Ty::Param(t));
    let p2 = env.intern(Ty::Param(u));

    let err = unify(&mut env, p1, p2, SPAN).unwrap_err();
    assert!(matches!(err.kind, SemaErrorKind::CannotUnify { .. }));
}

#[test]
fn named_types_with_same_symbol_unify() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let option = interner.intern("Option");

    let lhs = env.intern(Ty::Named(NominalKey {
        module_name: Some("std/option".to_owned()),
        name: option,
    }));
    let rhs = env.intern(Ty::Named(NominalKey {
        module_name: Some("std/option".to_owned()),
        name: option,
    }));

    let result = unify(&mut env, lhs, rhs, SPAN).unwrap();
    assert_eq!(
        *env.types.get(result),
        Ty::Named(NominalKey {
            module_name: Some("std/option".to_owned()),
            name: option,
        })
    );
}

#[test]
fn named_types_from_different_modules_do_not_unify() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let node = interner.intern("Node");

    let lhs = env.intern(Ty::Named(NominalKey {
        module_name: Some("/tmp/a.ms".to_owned()),
        name: node,
    }));
    let rhs = env.intern(Ty::Named(NominalKey {
        module_name: Some("/tmp/b.ms".to_owned()),
        name: node,
    }));

    let err = unify(&mut env, lhs, rhs, SPAN).unwrap_err();
    assert!(matches!(err.kind, SemaErrorKind::CannotUnify { .. }));
}

#[test]
fn type_applications_unify_pairwise() {
    let mut env = seeded_env();
    let mut interner = Interner::new();
    let result = interner.intern("Result");
    let base = env.intern(Ty::Named(NominalKey {
        module_name: Some("std/result".to_owned()),
        name: result,
    }));
    let int = env.builtin(BuiltinType::Int);
    let err = env.builtin(BuiltinType::String);
    let var = env.fresh_var();

    let lhs = env.intern(Ty::App(base, vec![int, err]));
    let rhs = env.intern(Ty::App(base, vec![var, err]));

    let unified = unify(&mut env, lhs, rhs, SPAN).unwrap();
    match env.types.get(unified) {
        Ty::App(unified_base, args) => {
            assert_eq!(
                *env.types.get(*unified_base),
                Ty::Named(NominalKey {
                    module_name: Some("std/result".to_owned()),
                    name: result,
                })
            );
            assert_eq!(args.len(), 2);
            assert_eq!(env.resolve_var(args[0]), int);
            assert_eq!(env.resolve_var(args[1]), err);
        }
        other => panic!("expected App, got {other:?}"),
    }
}

#[test]
fn unit_unifies_with_unit() {
    let mut env = seeded_env();
    let u1 = env.intern(Ty::Unit);
    let u2 = env.intern(Ty::Unit);

    let result = unify(&mut env, u1, u2, SPAN).unwrap();
    assert_eq!(*env.types.get(result), Ty::Unit);
}

#[test]
fn array_element_unification() {
    let mut env = seeded_env();
    let int = env.builtin(BuiltinType::Int);
    let var = env.fresh_var();

    let a1 = env.intern(Ty::Array(int));
    let a2 = env.intern(Ty::Array(var));

    let result = unify(&mut env, a1, a2, SPAN).unwrap();
    match env.types.get(result) {
        Ty::Array(inner) => assert_eq!(env.resolve_var(*inner), int),
        other => panic!("expected Array, got {other:?}"),
    }
}

#[test]
fn incompatible_types_cannot_unify() {
    let mut env = seeded_env();
    let unit = env.intern(Ty::Unit);
    let int = env.builtin(BuiltinType::Int);

    let err = unify(&mut env, unit, int, SPAN).unwrap_err();
    assert!(matches!(err.kind, SemaErrorKind::CannotUnify { .. }));
}
