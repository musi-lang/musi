//! Type parsing tests.

use music_ast::expr::Expr;
use music_ast::ty::Ty;
use music_lex::lex as lex_source;
use music_lex::token::Token;
use music_shared::{DiagnosticBag, FileId, Interner, SourceDb, Span};

use crate::parse;

fn lex(src: &str) -> (Vec<Token>, Interner, FileId) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let file_id = source_db.add("test.mu", src);
    let mut lex_diags = DiagnosticBag::new();
    let lexed = lex_source(src, file_id, &mut interner, &mut lex_diags);
    (lexed.tokens, interner, file_id)
}

/// Parse `let x : <type> := 0;` and extract the type from the binding.
fn parse_ty_from_let(ty_src: &str) -> (Ty, DiagnosticBag) {
    let src = format!("let x : {ty_src} := 0;");
    let (tokens, interner, file_id) = lex(&src);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &interner);
    assert_eq!(module.stmts.len(), 1);
    let expr = &module.arenas.exprs[module.stmts[0].expr];
    assert!(matches!(expr, Expr::Let { .. }), "expected Let");
    let Expr::Let { fields, .. } = expr else {
        return (
            Ty::Error {
                span: Span::default(),
            },
            diags,
        );
    };
    let ty_idx = fields.ty.expect("expected type annotation");
    let ty = module.arenas.tys[ty_idx].clone();
    (ty, diags)
}

#[test]
fn test_parse_named_type() {
    let (ty, diags) = parse_ty_from_let("Int");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Ty::Named { .. }));
}

#[test]
fn test_parse_named_type_with_args() {
    let (ty, diags) = parse_ty_from_let("List of Int");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Ty::Named { .. }), "expected Named");
    let Ty::Named { args, .. } = ty else { return };
    assert_eq!(args.len(), 1);
}

#[test]
fn test_parse_option_type() {
    let (ty, diags) = parse_ty_from_let("?Int");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Ty::Option { .. }));
}

#[test]
fn test_parse_type_variable() {
    let (ty, diags) = parse_ty_from_let("'T");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Ty::Var { .. }));
}

#[test]
fn test_parse_sum_type() {
    let (ty, diags) = parse_ty_from_let("Int + String");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Ty::Sum { .. }), "expected Sum");
    let Ty::Sum { variants, .. } = ty else { return };
    assert_eq!(variants.len(), 2);
}

#[test]
fn test_parse_product_type() {
    let (ty, diags) = parse_ty_from_let("Int * String");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Ty::Product { .. }), "expected Product");
    let Ty::Product { fields, .. } = ty else {
        return;
    };
    assert_eq!(fields.len(), 2);
}

#[test]
fn test_parse_array_type() {
    let (ty, diags) = parse_ty_from_let("[10] Int");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Ty::Array { .. }), "expected Array");
    let Ty::Array { len, .. } = ty else { return };
    assert_eq!(len, Some(10));
}

#[test]
fn test_parse_fn_type_pure() {
    let (ty, diags) = parse_ty_from_let("Int -> String");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Ty::Fn { .. }));
}
