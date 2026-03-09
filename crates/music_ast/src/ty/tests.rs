use music_shared::{Span, Symbol};

use crate::AstArenas;
use crate::expr::{Arrow, Expr};
use crate::ty::Ty;

#[test]
fn test_fn_type_params_and_ret_accessible() {
    let mut arenas = AstArenas::new();
    let param_ty = arenas.tys.alloc(Ty::Named {
        name: Symbol(0),
        args: vec![],
        span: Span::new(0, 3),
    });
    let ret_ty = arenas.tys.alloc(Ty::Named {
        name: Symbol(1),
        args: vec![],
        span: Span::new(7, 3),
    });
    let idx = arenas.tys.alloc(Ty::Fn {
        params: vec![param_ty],
        ret: ret_ty,
        arrow: Arrow::Pure,
        effects: None,
        span: Span::new(0, 10),
    });
    let Ty::Fn {
        params, ret, arrow, ..
    } = &arenas.tys[idx]
    else {
        panic!("expected Fn");
    };
    assert_eq!(params.len(), 1);
    assert_eq!(params[0], param_ty);
    assert_eq!(*ret, ret_ty);
    assert_eq!(*arrow, Arrow::Pure);
}

#[test]
fn test_option_sugar_wraps_inner() {
    let mut arenas = AstArenas::new();
    let inner = arenas.tys.alloc(Ty::Named {
        name: Symbol(0),
        args: vec![],
        span: Span::new(1, 3),
    });
    let idx = arenas.tys.alloc(Ty::Option {
        inner,
        span: Span::new(0, 4),
    });
    let Ty::Option { inner: stored, .. } = &arenas.tys[idx] else {
        panic!("expected Option");
    };
    assert_eq!(*stored, inner);
}

#[test]
fn test_refine_type_holds_expr_predicate() {
    let mut arenas = AstArenas::new();
    let base = arenas.tys.alloc(Ty::Named {
        name: Symbol(0),
        args: vec![],
        span: Span::new(0, 3),
    });
    let pred = arenas.exprs.alloc(Expr::Name {
        ident: Symbol(1),
        span: Span::new(6, 4),
    });
    let idx = arenas.tys.alloc(Ty::Refine {
        base,
        pred,
        span: Span::new(0, 10),
    });
    let Ty::Refine {
        base: b, pred: p, ..
    } = &arenas.tys[idx]
    else {
        panic!("expected Refine");
    };
    assert_eq!(*b, base);
    assert_eq!(*p, pred);
}
