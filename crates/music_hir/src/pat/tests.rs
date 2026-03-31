use crate::{HirStore, HirTy, HirTyKind};

use super::*;

#[test]
fn test_pat_error_is_typed() {
    let mut store = HirStore::new();
    let ty_error = store.tys.alloc(HirTy {
        origin: HirOrigin::dummy(),
        kind: HirTyKind::Error,
    });
    let pat = HirPat {
        origin: HirOrigin::dummy(),
        ty: ty_error,
        kind: HirPatKind::Error,
    };
    assert!(matches!(pat.kind, HirPatKind::Error));
}
