#![allow(clippy::panic)]

use msc_shared::{Arena, Span, Symbol};

use crate::expr::Expr;
use crate::lit::{FStrPart, Lit};
use crate::{AstArenas, NameRef};

use std::f64;

#[test]
fn test_lit_int_round_trip_through_arena() {
    let mut arena: Arena<Lit> = Arena::new();
    let lit = Lit::Int {
        value: 42,
        span: Span::new(0, 2),
    };
    let idx = arena.alloc(lit);
    assert_eq!(arena[idx].span(), Span::new(0, 2));
    let Lit::Int { value, .. } = &arena[idx] else {
        panic!("expected Int");
    };
    assert_eq!(*value, 42);
}

#[test]
fn test_lit_float_round_trip_through_arena() {
    let mut arena: Arena<Lit> = Arena::new();
    let lit = Lit::Float {
        value: f64::consts::PI,
        span: Span::new(0, 4),
    };
    let idx = arena.alloc(lit);
    let Lit::Float { value, .. } = &arena[idx] else {
        panic!("expected Float");
    };
    assert!((*value - f64::consts::PI).abs() < f64::EPSILON);
}

#[test]
fn test_lit_str_round_trip_through_arena() {
    let mut arena: Arena<Lit> = Arena::new();
    let sym = Symbol(0);
    let lit = Lit::Str {
        value: sym,
        span: Span::new(0, 5),
    };
    let idx = arena.alloc(lit);
    let Lit::Str { value, .. } = &arena[idx] else {
        panic!("expected Str");
    };
    assert_eq!(*value, sym);
}

#[test]
fn test_lit_rune_round_trip_through_arena() {
    let mut arena: Arena<Lit> = Arena::new();
    let lit = Lit::Rune {
        codepoint: 0x41,
        span: Span::new(0, 3),
    };
    let idx = arena.alloc(lit);
    let Lit::Rune { codepoint, .. } = &arena[idx] else {
        panic!("expected Rune");
    };
    assert_eq!(*codepoint, 0x41);
}

#[test]
fn test_lit_unit_round_trip_through_arena() {
    let mut arena: Arena<Lit> = Arena::new();
    let lit = Lit::Unit {
        span: Span::new(0, 2),
    };
    let idx = arena.alloc(lit);
    assert!(matches!(&arena[idx], Lit::Unit { .. }));
}

#[test]
fn test_lit_fstr_with_interpolation() {
    let mut arenas = AstArenas::new();
    let name_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(1),
        span: Span::new(2, 3),
    });
    let expr_idx = arenas.exprs.alloc(Expr::Name {
        name_ref,
        span: Span::new(2, 3),
    });

    let lit = Lit::FStr {
        parts: vec![
            FStrPart::Text {
                raw: Symbol(0),
                span: Span::new(0, 2),
            },
            FStrPart::Interpolated {
                expr: expr_idx,
                span: Span::new(2, 3),
            },
        ],
        span: Span::new(0, 5),
    };
    assert_eq!(lit.span(), Span::new(0, 5));
    let Lit::FStr { parts, .. } = &lit else {
        panic!("expected FStr");
    };
    assert_eq!(parts.len(), 2);
}
