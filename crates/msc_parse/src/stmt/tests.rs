//! Declaration parsing tests.

use msc_ast::expr::Expr;
use msc_lex::lex as lex_source;
use msc_lex::token::Token;
use msc_shared::{DiagnosticBag, FileId, Interner, SourceDb};

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
    let (tokens, mut interner, file_id) = lex(src);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &mut interner);
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
    let src = "class Eq ['T] { let (=)(a, b) : Bool; };";
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
    let src = "instance Eq of Int { let (=)(a, b) : Bool := a; };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(matches!(expr, Expr::Instance { .. }));
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
    let src = "effect State ['S] { get : () -> 'S; put : 'S -> (); };";
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
    let src = "effect Throw ['E] { raise : 'E -> 'E; };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    let Expr::Effect { params, ops, .. } = expr else {
        return;
    };
    assert_eq!(params.len(), 1);
    assert_eq!(ops.len(), 1);
}

#[test]
fn test_parse_instance_generic_with_bracket_params() {
    let src = "instance ['T] where 'T <: Eq Eq of List { let (=)(xs, ys) : Bool; };";
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(
        matches!(expr, Expr::Instance { .. }),
        "expected Instance, got {expr:?}"
    );
    let Expr::Instance {
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

#[test]
fn test_export_foreign_block_parses() {
    let src = r#"export foreign "C" ( let malloc : Int ~> Int; );"#;
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(
        matches!(expr, Expr::Foreign { exported: true, .. }),
        "expected exported Foreign, got {expr:?}"
    );
    let Expr::Foreign { decls, .. } = &expr else {
        return;
    };
    assert_eq!(decls.len(), 1);
}

#[test]
fn test_export_foreign_single_parses() {
    let src = r#"export foreign "C" let exit : Int ~> Int;"#;
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(
        matches!(expr, Expr::Foreign { exported: true, .. }),
        "expected exported Foreign, got {expr:?}"
    );
}

#[test]
fn test_export_foreign_with_attrs_parses() {
    let src = r#"#[link := "m"] export foreign "C" ( let sqrt : Float64 ~> Float64; );"#;
    let (expr, diags) = parse_single(src);
    assert!(!diags.has_errors(), "diags: {diags:?}");
    assert!(
        matches!(expr, Expr::Annotated { .. }),
        "expected Annotated, got {expr:?}"
    );
}
