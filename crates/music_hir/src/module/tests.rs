use music_storage::Idx;

use crate::{HirExprKind, HirOrigin, HirTyKind};

use super::*;

#[test]
fn test_can_allocate_minimal_module_graph() {
    let mut sources = music_basic::SourceMap::new();
    let source_id = sources.add("<test>", "");

    let mut store = HirStore::new();
    let ty_error = store.tys.alloc(HirTy {
        origin: HirOrigin::dummy(),
        kind: HirTyKind::Error,
    });
    let expr_error = store.exprs.alloc(HirExpr {
        origin: HirOrigin::dummy(),
        ty: ty_error,
        kind: HirExprKind::Error,
    });

    let module = HirModule::new(source_id, store, expr_error);
    assert_eq!(module.root, expr_error);

    // Sanity: Idx is Copy and stable.
    let _copy: Idx<HirExpr> = module.root;
}
