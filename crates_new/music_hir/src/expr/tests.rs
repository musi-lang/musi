use music_basic::Span;
use music_names::{Ident, Symbol};

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
        is_foreign: false,
        external_abi: None,
    };
    assert!(mods.exported);
    assert!(!mods.opaque);
}

#[test]
fn test_symbolic_binary_op_carries_ident() {
    let op = Ident::new(Symbol::synthetic(7), Span::new(10, 13));
    let carried = HirBinaryOp::Symbolic(op);
    assert!(matches!(carried, HirBinaryOp::Symbolic(id) if id == op));
}

#[test]
fn test_fstring_parts_can_reference_typed_exprs() {
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

    let lit = HirLit {
        kind: HirLitKind::FString {
            span: Span::new(0, 10),
            syntax: None,
            parts: Box::new([
                HirFStringPart::Literal {
                    span: Span::new(1, 5),
                },
                HirFStringPart::Expr {
                    origin: HirOrigin::new(Span::new(5, 9), None),
                    expr: expr_error,
                },
            ]),
        },
    };

    assert!(matches!(lit.kind, HirLitKind::FString { .. }));
}
