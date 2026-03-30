use music_names::Interner;

use super::{IrExprTy, IrScalarTy, IrTypeRef};

#[test]
fn test_expr_ty_as_type_ref_scalar() {
    assert_eq!(
        IrExprTy::Scalar(IrScalarTy::Int).as_type_ref(),
        IrTypeRef::Scalar(IrScalarTy::Int)
    );
}

#[test]
fn test_expr_ty_as_type_ref_named() {
    let mut interner = Interner::new();
    let sym = interner.intern("Foo");
    assert_eq!(IrExprTy::Named(sym).as_type_ref(), IrTypeRef::Named(sym));
}
