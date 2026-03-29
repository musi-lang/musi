use super::*;

#[test]
fn test_ty_error_is_constructible() {
    let ty = HirTy {
        origin: HirOrigin::dummy(),
        kind: HirTyKind::Error,
    };
    assert!(matches!(ty.kind, HirTyKind::Error));
}
