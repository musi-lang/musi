#![allow(clippy::panic)]

use music_shared::{Span, Symbol};

use crate::expr::Arrow;
use crate::ty::Ty;
use crate::{AstArenas, NameRef};

#[test]
fn test_fn_type_params_and_ret_accessible() {
    let mut arenas = AstArenas::new();
    let param_name_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(0),
        span: Span::new(0, 3),
    });
    let param_ty = arenas.tys.alloc(Ty::Named {
        name_ref: param_name_ref,
        args: vec![],
        span: Span::new(0, 3),
    });
    let ret_name_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(1),
        span: Span::new(7, 3),
    });
    let ret_ty = arenas.tys.alloc(Ty::Named {
        name_ref: ret_name_ref,
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
    let inner_name_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(0),
        span: Span::new(1, 3),
    });
    let inner = arenas.tys.alloc(Ty::Named {
        name_ref: inner_name_ref,
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
