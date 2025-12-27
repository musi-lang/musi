use crate::ty_repr::IntWidth;
use crate::ty_repr::TyRepr;
use crate::unifier::Unifier;

#[test]
fn unify_same_types() {
    let mut unifier = Unifier::new();
    let int32 = TyRepr::int(IntWidth::I32);
    assert!(unifier.unify(&int32, &int32).is_ok());
}

#[test]
fn unify_var_with_concrete() {
    let mut unifier = Unifier::new();
    let var = unifier.fresh_var();
    let int32 = TyRepr::int(IntWidth::I32);

    assert!(unifier.unify(&var, &int32).is_ok());
    let resolved = unifier.apply(&var);
    assert_eq!(resolved, int32);
}

#[test]
fn unify_mismatch() {
    let mut unifier = Unifier::new();
    let int32 = TyRepr::int(IntWidth::I32);
    let bool_ty = TyRepr::bool();

    assert!(unifier.unify(&int32, &bool_ty).is_err());
}

#[test]
fn unify_any_with_anything() {
    let mut unifier = Unifier::new();
    let any = TyRepr::any();
    let int32 = TyRepr::int(IntWidth::I32);

    assert!(unifier.unify(&any, &int32).is_ok());
}
