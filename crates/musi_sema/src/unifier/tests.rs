use crate::ty_repr::{IntWidth, TyRepr};
use crate::unifier::Unifier;
use crate::{TyParamId, TyVarId};

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

#[test]
fn unify_tuple_tys() {
    let mut unifier = Unifier::new();
    let v1 = unifier.fresh_var();
    let v2 = unifier.fresh_var();
    let tuple1 = TyRepr::tuple(vec![v1.clone(), v2.clone()]);
    let tuple2 = TyRepr::tuple(vec![TyRepr::int(IntWidth::I32), TyRepr::bool()]);

    assert!(unifier.unify(&tuple1, &tuple2).is_ok());
    assert_eq!(unifier.apply(&v1), TyRepr::int(IntWidth::I32));
    assert_eq!(unifier.apply(&v2), TyRepr::bool());
}

#[test]
fn unify_tuple_mismatch() {
    let mut unifier = Unifier::new();
    let tuple1 = TyRepr::tuple(vec![TyRepr::int(IntWidth::I32)]);
    let tuple2 = TyRepr::tuple(vec![TyRepr::int(IntWidth::I32), TyRepr::bool()]);

    assert!(unifier.unify(&tuple1, &tuple2).is_err());
}

#[test]
fn unify_array_tys() {
    let mut unifier = Unifier::new();
    let v1 = unifier.fresh_var();
    let arr1 = TyRepr::array(v1.clone(), Some(10));
    let arr2 = TyRepr::array(TyRepr::int(IntWidth::I32), Some(10));

    assert!(unifier.unify(&arr1, &arr2).is_ok());
    assert_eq!(unifier.apply(&v1), TyRepr::int(IntWidth::I32));
}

#[test]
fn unify_array_size_mismatch() {
    let mut unifier = Unifier::new();
    let arr1 = TyRepr::array(TyRepr::int(IntWidth::I32), Some(10));
    let arr2 = TyRepr::array(TyRepr::int(IntWidth::I32), Some(20));

    assert!(unifier.unify(&arr1, &arr2).is_err());
}

#[test]
fn unify_fn_tys() {
    let mut unifier = Unifier::new();
    let v_param = unifier.fresh_var();
    let v_ret = unifier.fresh_var();
    let fn1 = TyRepr::func(vec![v_param.clone()], v_ret.clone());
    let fn2 = TyRepr::func(vec![TyRepr::int(IntWidth::I32)], TyRepr::bool());

    assert!(unifier.unify(&fn1, &fn2).is_ok());
    assert_eq!(unifier.apply(&v_param), TyRepr::int(IntWidth::I32));
    assert_eq!(unifier.apply(&v_ret), TyRepr::bool());
}

#[test]
fn unify_fn_arity_mismatch() {
    let mut unifier = Unifier::new();
    let fn1 = TyRepr::func(vec![TyRepr::int(IntWidth::I32)], TyRepr::bool());
    let fn2 = TyRepr::func(
        vec![TyRepr::int(IntWidth::I32), TyRepr::bool()],
        TyRepr::bool(),
    );

    assert!(unifier.unify(&fn1, &fn2).is_err());
}

#[test]
fn unify_optional_ty() {
    let mut unifier = Unifier::new();
    let v = unifier.fresh_var();
    let opt1 = TyRepr::optional(v.clone());
    let opt2 = TyRepr::optional(TyRepr::int(IntWidth::I32));

    assert!(unifier.unify(&opt1, &opt2).is_ok());
    assert_eq!(unifier.apply(&v), TyRepr::int(IntWidth::I32));
}

#[test]
fn unify_ptr_ty() {
    let mut unifier = Unifier::new();
    let v = unifier.fresh_var();
    let ptr1 = TyRepr::ptr(v.clone());
    let ptr2 = TyRepr::ptr(TyRepr::int(IntWidth::I32));

    assert!(unifier.unify(&ptr1, &ptr2).is_ok());
    assert_eq!(unifier.apply(&v), TyRepr::int(IntWidth::I32));
}

#[test]
fn unify_transitive_vars() {
    let mut unifier = Unifier::new();
    let v1 = unifier.fresh_var();
    let v2 = unifier.fresh_var();

    assert!(unifier.unify(&v1, &v2).is_ok());
    assert!(unifier.unify(&v2, &TyRepr::int(IntWidth::I32)).is_ok());
    assert_eq!(unifier.apply(&v1), TyRepr::int(IntWidth::I32));
}

#[test]
fn finalize_unbound_var() {
    let unifier = Unifier::new();
    let v = TyRepr::var(TyVarId::new(0));

    assert_eq!(unifier.finalize(&v), TyRepr::any());
}

#[test]
fn poly_display_formatting() {
    let param = TyParamId::new(0);
    let body = TyRepr::func(vec![TyRepr::type_param(param)], TyRepr::type_param(param));
    let poly = TyRepr::poly(vec![param], body);

    assert_eq!(format!("{poly}"), "forall[T0]. (T0) -> T0");
}

#[test]
fn type_param_display() {
    let param = TyRepr::type_param(TyParamId::new(2));
    assert_eq!(format!("{param}"), "T2");
}

#[test]
fn poly_apply_preserves_structure() {
    let unifier = Unifier::new();
    let param = TyParamId::new(0);
    let body = TyRepr::func(vec![TyRepr::type_param(param)], TyRepr::type_param(param));
    let poly = TyRepr::poly(vec![param], body.clone());

    let applied = unifier.apply(&poly);
    assert_eq!(applied, poly);
}

#[test]
fn poly_finalize_preserves_type_params() {
    let unifier = Unifier::new();
    let param = TyParamId::new(0);
    let body = TyRepr::func(vec![TyRepr::type_param(param)], TyRepr::type_param(param));
    let poly = TyRepr::poly(vec![param], body);

    let finalized = unifier.finalize(&poly);
    assert_eq!(format!("{finalized}"), "forall[T0]. (T0) -> T0");
}

#[test]
fn generalize_no_free_vars() {
    let unifier = Unifier::new();
    let concrete = TyRepr::func(vec![TyRepr::int(IntWidth::I32)], TyRepr::bool());

    let generalized = unifier.generalize(&concrete);
    assert_eq!(format!("{generalized}"), "(Int32) -> Bool");
}

#[test]
fn generalize_single_free_var() {
    let unifier = Unifier::new();
    let var = unifier.fresh_var();
    let fn_ty = TyRepr::func(vec![var.clone()], var);

    let generalized = unifier.generalize(&fn_ty);
    assert_eq!(format!("{generalized}"), "forall[T0]. (T0) -> T0");
}

#[test]
fn generalize_multiple_free_vars() {
    let unifier = Unifier::new();
    let var_a = unifier.fresh_var();
    let var_b = unifier.fresh_var();
    let fn_ty = TyRepr::func(vec![var_a], var_b);

    let generalized = unifier.generalize(&fn_ty);
    assert_eq!(format!("{generalized}"), "forall[T0, T1]. (T0) -> T1");
}

#[test]
fn generalize_bound_var_not_generalized() {
    let mut unifier = Unifier::new();
    let var = unifier.fresh_var();
    let fn_ty = TyRepr::func(vec![var.clone()], var.clone());

    unifier.unify(&var, &TyRepr::int(IntWidth::I32)).unwrap();

    let generalized = unifier.generalize(&fn_ty);
    assert_eq!(format!("{generalized}"), "(Int32) -> Int32");
}
