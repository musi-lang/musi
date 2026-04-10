use music_base::{SourceId, Span};

use crate::{HirExpr, HirExprKind, HirLit, HirLitKind, HirMods, HirModule, HirOrigin, HirStore};

#[test]
fn store_allocates_and_round_trips_ids() {
    let mut store = HirStore::new();
    let origin = HirOrigin::new(SourceId::from_raw(1), Span::new(0, 1));
    let lit = store.alloc_lit(HirLit {
        origin,
        kind: HirLitKind::Int { raw: "1".into() },
    });
    let expr = store.alloc_expr(HirExpr {
        origin,
        mods: HirMods::EMPTY,
        kind: HirExprKind::Lit { lit },
    });

    assert_eq!(store.exprs.get(expr).origin, origin);
}

#[test]
fn module_wraps_store_and_root() {
    let mut store = HirStore::new();
    let origin = HirOrigin::dummy();
    let root = store.alloc_expr(HirExpr {
        origin,
        mods: HirMods::EMPTY,
        kind: HirExprKind::Error,
    });
    let module = HirModule::new(SourceId::from_raw(9), store, root);
    assert_eq!(module.source_id, SourceId::from_raw(9));
}
