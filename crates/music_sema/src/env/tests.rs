use music_builtins::types::BuiltinType;

use crate::env::TypeEnv;
use crate::types::Ty;

#[test]
fn seed_builtins_creates_entries_for_all() {
    let mut env = TypeEnv::new();
    env.seed_builtins();
    for &bt in BuiltinType::ALL {
        let id = env.builtin(bt);
        assert_eq!(*env.types.get(id), Ty::Builtin(bt));
    }
    assert_eq!(env.builtin_types.len(), BuiltinType::ALL.len());
}

#[test]
fn intern_and_get_round_trip() {
    let mut env = TypeEnv::new();
    let id = env.intern(Ty::Unit);
    assert_eq!(*env.types.get(id), Ty::Unit);
}

#[test]
fn intern_preserves_distinct_types() {
    let mut env = TypeEnv::new();
    let a = env.intern(Ty::Any);
    let b = env.intern(Ty::Empty);
    assert_ne!(a, b);
    assert_eq!(*env.types.get(a), Ty::Any);
    assert_eq!(*env.types.get(b), Ty::Empty);
}

#[test]
fn fresh_var_creates_distinct_variables() {
    let mut env = TypeEnv::new();
    let v1 = env.fresh_var();
    let v2 = env.fresh_var();
    assert_ne!(v1, v2);

    match env.types.get(v1) {
        Ty::Var(0) => {}
        other => panic!("expected Var(0), got {other:?}"),
    }
    match env.types.get(v2) {
        Ty::Var(1) => {}
        other => panic!("expected Var(1), got {other:?}"),
    }
}

#[test]
fn fresh_var_starts_unbound() {
    let mut env = TypeEnv::new();
    let v = env.fresh_var();
    let resolved = env.resolve_var(v);
    assert_eq!(resolved, v);
}

#[test]
fn resolve_var_follows_chain() {
    let mut env = TypeEnv::new();
    let v1 = env.fresh_var(); // Var(0)
    let v2 = env.fresh_var(); // Var(1)
    let concrete = env.intern(Ty::Builtin(BuiltinType::Int));

    env.bind_var(0, v2);
    env.bind_var(1, concrete);

    let resolved = env.resolve_var(v1);
    assert_eq!(resolved, concrete);
    assert_eq!(*env.types.get(resolved), Ty::Builtin(BuiltinType::Int));
}

#[test]
fn resolve_var_on_non_var_returns_self() {
    let mut env = TypeEnv::new();
    let id = env.intern(Ty::Unit);
    assert_eq!(env.resolve_var(id), id);
}

#[test]
fn bind_var_updates_variable() {
    let mut env = TypeEnv::new();
    let v = env.fresh_var(); // Var(0)
    let target = env.intern(Ty::Builtin(BuiltinType::Bool));

    env.bind_var(0, target);

    let resolved = env.resolve_var(v);
    assert_eq!(resolved, target);
}

#[test]
fn default_impl_matches_new() {
    let env = TypeEnv::default();
    assert!(env.types.is_empty());
    assert!(env.vars.is_empty());
    assert!(env.type_map.is_empty());
}
