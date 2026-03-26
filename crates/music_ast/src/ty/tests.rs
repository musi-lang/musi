use music_found::{Ident, Interner, Span};

use super::*;

fn test_ident() -> (Interner, Ident) {
    let mut interner = Interner::new();
    let sym = interner.intern("x");
    (interner, Ident::new(sym, Span::DUMMY))
}

fn dummy_ty_id() -> TyId {
    TyId::from_raw(0)
}

#[test]
fn named_no_args() {
    let (_i, ident) = test_ident();
    let t = TyKind::Named {
        name: ident,
        args: vec![],
    };
    assert!(matches!(t, TyKind::Named { ref args, .. } if args.is_empty()));
}

#[test]
fn named_with_args() {
    let (_i, ident) = test_ident();
    let t = TyKind::Named {
        name: ident,
        args: vec![dummy_ty_id()],
    };
    assert!(matches!(t, TyKind::Named { ref args, .. } if args.len() == 1));
}

#[test]
fn arrow() {
    let t = TyKind::Arrow {
        from: dummy_ty_id(),
        to: dummy_ty_id(),
    };
    assert!(matches!(t, TyKind::Arrow { .. }));
}

#[test]
fn effect_arrow() {
    let t = TyKind::EffectArrow {
        from: dummy_ty_id(),
        to: dummy_ty_id(),
    };
    assert!(matches!(t, TyKind::EffectArrow { .. }));
}

#[test]
fn sum() {
    let t = TyKind::Sum(vec![dummy_ty_id(), dummy_ty_id()]);
    assert!(matches!(t, TyKind::Sum(ref tys) if tys.len() == 2));
}

#[test]
fn product() {
    let t = TyKind::Product(vec![dummy_ty_id(), dummy_ty_id()]);
    assert!(matches!(t, TyKind::Product(ref tys) if tys.len() == 2));
}

#[test]
fn mut_ty() {
    let t = TyKind::Mut(dummy_ty_id());
    assert!(matches!(t, TyKind::Mut(_)));
}

#[test]
fn option_ty() {
    let t = TyKind::Option(dummy_ty_id());
    assert!(matches!(t, TyKind::Option(_)));
}

#[test]
fn pi() {
    let (_i, ident) = test_ident();
    let t = TyKind::Pi {
        name: ident,
        param_ty: dummy_ty_id(),
        ret_ty: dummy_ty_id(),
    };
    assert!(matches!(t, TyKind::Pi { .. }));
}

#[test]
fn tuple() {
    let t = TyKind::Tuple(vec![dummy_ty_id()]);
    assert!(matches!(t, TyKind::Tuple(ref tys) if tys.len() == 1));
}

#[test]
fn array_with_dims() {
    let t = TyKind::Array {
        dims: vec![Dim::Lit(3), Dim::Inferred],
        elem: dummy_ty_id(),
    };
    assert!(matches!(t, TyKind::Array { ref dims, .. } if dims.len() == 2));
}

#[test]
fn dim_is_copy() {
    let d = Dim::Lit(5);
    let copied = d;
    assert_eq!(d, copied);
}

#[test]
fn dim_var() {
    let (_i, ident) = test_ident();
    let d = Dim::Var(ident);
    assert!(matches!(d, Dim::Var(_)));
}

#[test]
fn dim_inferred() {
    let d = Dim::Inferred;
    assert!(matches!(d, Dim::Inferred));
}

#[test]
fn ty_kind_clone_eq() {
    let t = TyKind::Mut(dummy_ty_id());
    let cloned = t.clone();
    assert_eq!(t, cloned);
}
