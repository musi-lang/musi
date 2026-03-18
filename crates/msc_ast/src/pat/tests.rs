#![allow(clippy::panic)]

use msc_shared::{Span, Symbol};

use crate::AstArenas;
use crate::expr::BindKind;
use crate::pat::{Pat, PatRecField};

#[test]
fn test_or_pattern_both_arms_accessible() {
    let mut arenas = AstArenas::new();
    let left = arenas.pats.alloc(Pat::Wild {
        span: Span::new(0, 1),
    });
    let right = arenas.pats.alloc(Pat::Wild {
        span: Span::new(4, 1),
    });
    let idx = arenas.pats.alloc(Pat::Or {
        left,
        right,
        span: Span::new(0, 5),
    });
    let Pat::Or {
        left: l, right: r, ..
    } = &arenas.pats[idx]
    else {
        panic!("expected Or");
    };
    assert_eq!(*l, left);
    assert_eq!(*r, right);
}

#[test]
fn test_bind_with_inner_pattern() {
    let mut arenas = AstArenas::new();
    let inner = arenas.pats.alloc(Pat::Wild {
        span: Span::new(4, 1),
    });
    let idx = arenas.pats.alloc(Pat::Bind {
        kind: BindKind::Immut,
        name: Symbol(0),
        inner: Some(inner),
        span: Span::new(0, 5),
    });
    let Pat::Bind {
        inner: stored,
        name,
        ..
    } = &arenas.pats[idx]
    else {
        panic!("expected Bind");
    };
    assert_eq!(*stored, Some(inner));
    assert_eq!(*name, Symbol(0));
}

#[test]
fn test_record_pattern_fields_round_trip() {
    let mut arenas = AstArenas::new();
    let sub = arenas.pats.alloc(Pat::Wild {
        span: Span::new(6, 1),
    });
    let field = PatRecField {
        kind: BindKind::Immut,
        name: Symbol(0),
        pat: Some(sub),
        span: Span::new(0, 8),
    };
    let idx = arenas.pats.alloc(Pat::Record {
        fields: vec![field],
        span: Span::new(0, 10),
    });
    let Pat::Record { fields, .. } = &arenas.pats[idx] else {
        panic!("expected Record");
    };
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].pat, Some(sub));
}
