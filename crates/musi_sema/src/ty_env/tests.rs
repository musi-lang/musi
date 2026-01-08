use musi_core::Symbol;
use musi_core::span::Span;

use crate::ty::TyArena;
use crate::ty::TyKind;
use crate::ty_env::TyEnv;

fn dummy_symbol(id: u32) -> Symbol {
    Symbol::new(id, Span::DUMMY)
}

#[test]
fn test_scope_lookup() {
    let mut arena = TyArena::new();
    let mut env = TyEnv::new();
    let sym1 = dummy_symbol(1);
    let ty1 = arena.alloc(TyKind::Int, Span::DUMMY);
    let ty2 = arena.alloc(TyKind::Bool, Span::DUMMY);

    env.bind_value(sym1, ty1, false, Span::DUMMY);
    let entry = env.lookup_value(sym1).expect("should have value");
    assert_eq!(entry.ty, ty1);
    assert!(!entry.mutable);

    env.enter_scope();

    env.bind_value(sym1, ty2, true, Span::DUMMY);
    let entry = env.lookup_value(sym1).expect("should have shadowed value");
    assert_eq!(entry.ty, ty2);
    assert!(entry.mutable);

    env.exit_scope(Span::DUMMY).expect("failed to exit scope");
    assert_eq!(env.lookup_value(sym1).unwrap().ty, ty1);
}

#[test]
fn test_namespaces() {
    let mut arena = TyArena::new();
    let mut env = TyEnv::new();
    let sym = dummy_symbol(1);
    let val_ty = arena.alloc(TyKind::Int, Span::DUMMY);
    let ty_ty = arena.alloc(TyKind::Bool, Span::DUMMY);

    env.bind_value(sym, val_ty, false, Span::DUMMY);
    env.bind_ty(sym, ty_ty);

    let v_entry = env.lookup_value(sym).unwrap();
    assert_eq!(v_entry.ty, val_ty);

    let t_id = env.lookup_ty(sym).unwrap();
    assert_eq!(t_id, ty_ty);
}

#[test]
fn test_exit_global_error() {
    let mut env = TyEnv::new();
    assert!(env.exit_scope(Span::DUMMY).is_err());
}
