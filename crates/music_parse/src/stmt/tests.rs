//! Declaration parsing tests.

use music_ast::expr::Expr;
use music_lex::lex as lex_source;
use music_lex::token::Token;
use music_shared::{DiagnosticBag, FileId, Interner, SourceDb};

use crate::parse;

fn lex(src: &str) -> (Vec<Token>, Interner, FileId) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let file_id = source_db.add("test.mu", src);
    let mut lex_diags = DiagnosticBag::new();
    let lexed = lex_source(src, file_id, &mut interner, &mut lex_diags);
    (lexed.tokens, interner, file_id)
}

fn parse_single(src: &str) -> (Expr, DiagnosticBag) {
    let (tokens, interner, file_id) = lex(src);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &interner);
    assert_eq!(module.stmts.len(), 1, "expected one stmt for: {src}");
    let expr = module.arenas.exprs[module.stmts[0].expr].clone();
    (expr, diags)
}

#[test]
fn test_parse_let_simple() {
    let (expr, diags) = parse_single("let x := 42;");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Let { .. }));
}

#[test]
fn test_parse_var_simple() {
    let (expr, diags) = parse_single("let x := 42;");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Let { .. }));
}

#[test]
fn test_parse_let_with_type_annotation() {
    let (expr, diags) = parse_single("let x : Int := 42;");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Let { .. }), "expected Let");
    let Expr::Let { fields, .. } = expr else {
        return;
    };
    assert!(fields.ty.is_some());
}

#[test]
fn test_parse_let_in_scoped() {
    let (expr, diags) = parse_single("let x := 1 in (x);");
    assert!(!diags.has_errors());
    assert!(matches!(expr, Expr::Let { .. }), "expected Let");
    let Expr::Let { body, .. } = expr else { return };
    assert!(body.is_some());
}

#[test]
fn test_parse_class_declaration() {
    let src = "class Eq over 'T { let (=)(a, b) : Bool; };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(
        matches!(expr, Expr::Class { .. }),
        "expected Class, got {expr:?}"
    );
    let Expr::Class { members, .. } = expr else {
        return;
    };
    assert_eq!(members.len(), 1);
}

#[test]
fn test_parse_given_declaration() {
    let src = "given Eq of Int { let (=)(a, b) : Bool := a; };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(matches!(expr, Expr::Given { .. }));
}

#[test]
fn test_parse_export_let() {
    let src = "export let x := 42;";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors());
    assert!(
        matches!(expr, Expr::Binding { .. }),
        "expected Binding, got {expr:?}"
    );
    let Expr::Binding { exported, .. } = expr else {
        return;
    };
    assert!(exported);
}

#[test]
fn test_parse_effect_no_params_single_op() {
    let src = "effect Async { suspend : () -> (); };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(
        matches!(expr, Expr::Effect { .. }),
        "expected Effect, got {expr:?}"
    );
    let Expr::Effect { params, ops, .. } = expr else {
        return;
    };
    assert!(params.is_empty());
    assert_eq!(ops.len(), 1);
}

#[test]
fn test_parse_effect_with_params_multiple_ops() {
    let src = "effect State of 'S { get : () -> 'S; put : 'S -> (); };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(
        matches!(expr, Expr::Effect { .. }),
        "expected Effect, got {expr:?}"
    );
    let Expr::Effect { params, ops, .. } = expr else {
        return;
    };
    assert_eq!(params.len(), 1);
    assert_eq!(ops.len(), 2);
}

#[test]
fn test_parse_effect_throw_parameterized() {
    let src = "effect Throw of 'E { raise : 'E -> 'E; };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    let Expr::Effect { params, ops, .. } = expr else {
        return;
    };
    assert_eq!(params.len(), 1);
    assert_eq!(ops.len(), 1);
}

#[test]
fn test_parse_given_generic_with_over() {
    let src = "given Eq of List over 'T where 'T <: Eq { let (=)(xs, ys) : Bool; };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(
        matches!(expr, Expr::Given { .. }),
        "expected Given, got {expr:?}"
    );
    let Expr::Given {
        params,
        constraints,
        ..
    } = expr
    else {
        return;
    };
    assert_eq!(params.len(), 1);
    assert_eq!(constraints.len(), 1);
}

#[test]
fn test_parse_class_no_params() {
    let src = "class Marker { };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(
        matches!(expr, Expr::Class { .. }),
        "expected Class, got {expr:?}"
    );
    let Expr::Class { params, .. } = expr else {
        return;
    };
    assert!(params.is_empty());
}
