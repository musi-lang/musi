use crate::test_utils::TestContext;
use musi_ast::{LitKind, PatKind};

#[test]
fn test_pat_wild() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_pat("_");
    assert!(matches!(ctx.pat(id).kind, PatKind::Wild));
}

#[test]
fn test_pat_ident() {
    let mut ctx = TestContext::new();
    let x = ctx.intern("x");
    let id = ctx.parse_pat("x");
    assert!(matches!(ctx.pat(id).kind, PatKind::Ident(i) if i == x));
}

#[test]
fn test_pat_lit_int() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_pat("42");
    assert!(matches!(ctx.pat(id).kind, PatKind::Lit(LitKind::Int(_))));
}

#[test]
fn test_pat_tuple() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_pat("(x, y)");
    if let PatKind::Tuple(elems) = &ctx.pat(id).kind {
        assert_eq!(elems.len(), 2);
    } else {
        panic!("expected tuple pattern");
    }
}

#[test]
fn test_pat_array() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_pat("[a, b, c]");
    if let PatKind::Array(elems) = &ctx.pat(id).kind {
        assert_eq!(elems.len(), 3);
    } else {
        panic!("expected array pattern");
    }
}

#[test]
fn test_pat_variant() {
    let mut ctx = TestContext::new();
    let some = ctx.intern("Some");
    let id = ctx.parse_pat("Some(x)");
    if let PatKind::Choice { name, args, .. } = &ctx.pat(id).kind {
        assert_eq!(*name, some);
        assert_eq!(args.len(), 1);
    } else {
        panic!("expected variant pattern");
    }
}

#[test]
fn test_pat_cons() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_pat("head :: tail");
    if let PatKind::Cons(parts) = &ctx.pat(id).kind {
        assert_eq!(parts.len(), 2);
    } else {
        panic!("expected cons pattern");
    }
}

#[test]
fn test_pat_or() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_pat("1 | 2 | 3");
    if let PatKind::Or(alts) = &ctx.pat(id).kind {
        assert_eq!(alts.len(), 3);
    } else {
        panic!("expected or pattern");
    }
}

#[test]
fn test_pat_record() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_pat("Point.{x, y}");
    if let PatKind::Record { base, fields } = &ctx.pat(id).kind {
        assert!(base.is_some());
        assert_eq!(fields.len(), 2);
    } else {
        panic!("expected record pattern");
    }
}
