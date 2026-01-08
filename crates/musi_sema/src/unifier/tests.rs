use musi_core::Span;

use crate::table::UnificationTable;
use crate::ty::{TyArena, TyKind};
use crate::unifier::Unifier;

#[test]
fn test_unify_primitives() {
    let mut arena = TyArena::new();
    let mut table = UnificationTable::new();
    let int_t = arena.alloc(TyKind::Int, Span::DUMMY);
    let bool_t = arena.alloc(TyKind::Bool, Span::DUMMY);

    let mut unifier = Unifier::new(&arena, &mut table);

    assert!(unifier.unify(int_t, int_t, Span::DUMMY).is_ok());
    assert!(unifier.unify(int_t, bool_t, Span::DUMMY).is_err());
}

#[test]
fn test_unify_vars() {
    let mut arena = TyArena::new();
    let mut table = UnificationTable::new();

    let v1 = table.new_key();
    let v2 = table.new_key();

    let t_v1 = arena.alloc(TyKind::Var(v1), Span::DUMMY);
    let t_v2 = arena.alloc(TyKind::Var(v2), Span::DUMMY);
    let int_t = arena.alloc(TyKind::Int, Span::DUMMY);
    let bool_t = arena.alloc(TyKind::Bool, Span::DUMMY);

    {
        let mut unifier = Unifier::new(&arena, &mut table);
        unifier
            .unify(t_v1, t_v2, Span::DUMMY)
            .expect("Vars should unify");
        unifier
            .unify(t_v1, int_t, Span::DUMMY)
            .expect("Var should unify with Int");
    }

    let mut unifier = Unifier::new(&arena, &mut table);
    assert!(unifier.unify(t_v2, bool_t, Span::DUMMY).is_err());
}
