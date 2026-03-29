use music_basic::Span;

use crate::{HirStore, HirTy, HirTyKind};

use super::*;

#[test]
fn test_literal_expr_is_constructible() {
    let mut store = HirStore::new();
    let ty_error = store.tys.alloc(HirTy {
        origin: HirOrigin::dummy(),
        kind: HirTyKind::Error,
    });

    let expr = HirExpr {
        origin: HirOrigin::dummy(),
        ty: ty_error,
        kind: HirExprKind::Lit {
            lit: HirLit {
                kind: HirLitKind::Int {
                    span: Span::new(0, 2),
                    syntax: None,
                },
            },
        },
    };
    assert!(matches!(expr.kind, HirExprKind::Lit { .. }));
}

#[test]
fn test_decl_mods_flags_are_plain_data() {
    let mods = HirDeclMods {
        attrs: Box::new([]),
        exported: true,
        opaque: false,
        external_abi: None,
    };
    assert!(mods.exported);
    assert!(!mods.opaque);
}
