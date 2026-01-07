use crate::test_ctx::TestCtx;
use musi_ast::{LitKind, PatKind};

#[test]
fn test_pat_wild() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_pat("_");
    assert!(matches!(ctx.pat(id).kind, PatKind::Wild));
}

#[test]
fn test_pat_ident() {
    let mut ctx = TestCtx::new();
    let x = ctx.intern("x");
    let id = ctx.parse_pat("x");
    assert!(matches!(ctx.pat(id).kind, PatKind::Ident(i) if i == x));
}

#[test]
fn test_pat_lit_int() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_pat("42");
    assert!(matches!(ctx.pat(id).kind, PatKind::Lit(LitKind::Int(_))));
}

#[test]
fn test_pat_tuple() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_pat("(x, y)");
    let kind = &ctx.pat(id).kind;
    assert!(matches!(kind, PatKind::Tuple(elems) if elems.len() == 2));
}

#[test]
fn test_pat_array() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_pat("[a, b, c]");
    let kind = &ctx.pat(id).kind;
    assert!(matches!(kind, PatKind::Array(elems) if elems.len() == 3));
}

#[test]
fn test_pat_variant() {
    let mut ctx = TestCtx::new();
    let some = ctx.intern("Some");
    let id = ctx.parse_pat("Some(x)");
    let kind = &ctx.pat(id).kind;
    assert!(
        matches!(kind, PatKind::Variant { name, args, .. } if *name == some && args.len() == 1)
    );
}

#[test]
fn test_pat_cons() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_pat("head :: tail");
    let kind = &ctx.pat(id).kind;
    assert!(matches!(kind, PatKind::Cons(parts) if parts.len() == 2));
}

#[test]
fn test_pat_or() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_pat("1 | 2 | 3");
    let kind = &ctx.pat(id).kind;
    assert!(matches!(kind, PatKind::Or(alts) if alts.len() == 3));
}

#[test]
fn test_pat_record() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_pat("Point.{x, y}");
    let kind = &ctx.pat(id).kind;
    assert!(
        matches!(kind, PatKind::Record { base, fields } if base.is_some() && fields.len() == 2)
    );
}
