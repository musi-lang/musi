use music_shared::{Interner, Literal, Span, Spanned};

use super::*;

#[test]
fn new_creates_empty_arenas() {
    let data = AstData::new();
    assert!(data.exprs.is_empty());
    assert!(data.pats.is_empty());
    assert!(data.types.is_empty());
    assert!(data.attrs.is_empty());
    assert!(data.root.is_empty());
}

#[test]
fn default_creates_empty_arenas() {
    let data = AstData::default();
    assert!(data.exprs.is_empty());
    assert!(data.pats.is_empty());
    assert!(data.types.is_empty());
    assert!(data.attrs.is_empty());
    assert!(data.root.is_empty());
}

#[test]
fn alloc_get_roundtrip_expr() {
    let mut data = AstData::new();
    let expr = Spanned::new(ExprKind::Lit(Literal::Int(42)), Span::new(0, 2));
    let id = data.exprs.alloc(expr);
    let retrieved = data.exprs.get(id);
    assert_eq!(retrieved.kind, ExprKind::Lit(Literal::Int(42)));
    assert_eq!(retrieved.span, Span::new(0, 2));
}

#[test]
fn alloc_get_roundtrip_pat() {
    use crate::pat::PatKind;

    let mut data = AstData::new();
    let pat = Spanned::new(PatKind::Wildcard, Span::new(5, 6));
    let id = data.pats.alloc(pat);
    let retrieved = data.pats.get(id);
    assert_eq!(retrieved.kind, PatKind::Wildcard);
    assert_eq!(retrieved.span, Span::new(5, 6));
}

#[test]
fn alloc_get_roundtrip_ty() {
    use music_shared::Ident;

    use crate::ty::TyKind;

    let mut interner = Interner::new();
    let sym = interner.intern("Int");
    let mut data = AstData::new();
    let ty = Spanned::new(
        TyKind::Named {
            name: Ident::new(sym, Span::DUMMY),
            args: vec![],
        },
        Span::new(10, 13),
    );
    let id = data.types.alloc(ty);
    let retrieved = data.types.get(id);
    assert!(matches!(retrieved.kind, TyKind::Named { ref args, .. } if args.is_empty()));
}

#[test]
fn alloc_get_roundtrip_attr() {
    use music_shared::Ident;

    let mut interner = Interner::new();
    let sym = interner.intern("inline");
    let mut data = AstData::new();
    let attr = Attr {
        name: Ident::new(sym, Span::DUMMY),
        args: vec![],
    };
    let id = data.attrs.alloc(Spanned::new(attr, Span::new(0, 7)));
    let retrieved = data.attrs.get(id);
    assert!(retrieved.kind.args.is_empty());
    assert_eq!(retrieved.span, Span::new(0, 7));
}

#[test]
fn multiple_exprs() {
    let mut data = AstData::new();
    let id1 = data
        .exprs
        .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(1))));
    let id2 = data
        .exprs
        .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(2))));
    assert_eq!(data.exprs.len(), 2);
    assert_eq!(data.exprs.get(id1).kind, ExprKind::Lit(Literal::Int(1)));
    assert_eq!(data.exprs.get(id2).kind, ExprKind::Lit(Literal::Int(2)));
}

#[test]
fn root_tracks_top_level_exprs() {
    let mut data = AstData::new();
    let id = data
        .exprs
        .alloc(Spanned::dummy(ExprKind::Lit(Literal::Int(0))));
    data.root.push(id);
    assert_eq!(data.root.len(), 1);
    assert_eq!(data.root[0], id);
}
