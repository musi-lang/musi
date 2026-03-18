//! Type parsing tests.

use music_ast::expr::{Arrow, Expr};
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

/// Parse `let x : <type> := 0;` and extract the type expression from the binding.
fn parse_ty_from_let(ty_src: &str) -> (Expr, DiagnosticBag) {
    let src = format!("let x : {ty_src} := 0;");
    let (tokens, mut interner, file_id) = lex(&src);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &mut interner);
    assert_eq!(module.stmts.len(), 1);
    let expr = &module.arenas.exprs[module.stmts[0].expr];
    assert!(matches!(expr, Expr::Let { .. }), "expected Let");
    let Expr::Let { fields, .. } = expr else {
        return (
            Expr::Error {
                span: Span::default(),
            },
            diags,
        );
    };
    let ty_idx = fields.ty.expect("expected type annotation");
    let ty = module.arenas.exprs[ty_idx].clone();
    (ty, diags)
}

#[test]
fn test_parse_named_type() {
    let (ty, diags) = parse_ty_from_let("Int");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Expr::Name { .. }));
}

#[test]
fn test_parse_named_type_with_args() {
    let (ty, diags) = parse_ty_from_let("List of Int");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Expr::TypeApp { .. }), "expected TypeApp");
    let Expr::TypeApp { args, .. } = ty else {
        return;
    };
    assert_eq!(args.len(), 1);
}

#[test]
fn test_parse_option_type() {
    let (ty, diags) = parse_ty_from_let("?Int");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Expr::OptionType { .. }));
}

#[test]
fn test_parse_type_variable() {
    let (ty, diags) = parse_ty_from_let("'T");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Expr::Name { .. }));
}

#[test]
fn test_parse_sum_type() {
    let (ty, diags) = parse_ty_from_let("Int + String");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Expr::SumType { .. }), "expected SumType");
    let Expr::SumType { variants, .. } = ty else {
        return;
    };
    assert_eq!(variants.len(), 2);
}

#[test]
fn test_parse_product_type() {
    let (ty, diags) = parse_ty_from_let("Int * String");
    assert!(!diags.has_errors());
    assert!(
        matches!(ty, Expr::ProductType { .. }),
        "expected ProductType"
    );
    let Expr::ProductType { fields, .. } = ty else {
        return;
    };
    assert_eq!(fields.len(), 2);
}

#[test]
fn test_parse_array_type() {
    let (ty, diags) = parse_ty_from_let("[10] Int");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Expr::ArrayType { .. }), "expected ArrayType");
    let Expr::ArrayType { len, .. } = ty else {
        return;
    };
    assert_eq!(len, Some(10));
}

#[test]
fn test_parse_fn_type_pure() {
    let (ty, diags) = parse_ty_from_let("Int -> String");
    assert!(!diags.has_errors());
    assert!(matches!(ty, Expr::FnType { .. }));
}

#[test]
fn test_parse_ty_fn_effectful_with_populates_effects() {
    let (ty, diags) = parse_ty_from_let("Int ~> String with { IO }");
    assert!(!diags.has_errors());
    assert!(
        matches!(
            &ty,
            Expr::FnType { arrow: Arrow::Effectful, effects: Some(eff), .. }
            if eff.effects.len() == 1
        ),
        "expected effectful FnType with one effect"
    );
}

#[test]
fn test_parse_ty_fn_pure_arrow_has_no_effects() {
    let (ty, diags) = parse_ty_from_let("Int -> String");
    assert!(!diags.has_errors());
    assert!(
        matches!(
            &ty,
            Expr::FnType {
                arrow: Arrow::Pure,
                effects: None,
                ..
            }
        ),
        "expected pure FnType with no effects"
    );
}

#[test]
fn test_parse_ty_fn_chain_effects_attach_to_inner() {
    // `A -> B ~> C with { IO }` should produce:
    // FnType { params: [A], ret: FnType { params: [B], ret: C, effects: Some({IO}) }, effects: None }
    let src = "let x : Int -> Bool ~> String with { IO } := 0;";
    let (tokens, mut interner, file_id) = lex(src);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &mut interner);
    assert!(!diags.has_errors());

    let expr = &module.arenas.exprs[module.stmts[0].expr];
    let Expr::Let { fields, .. } = expr else {
        return;
    };
    let ty_idx = fields.ty.expect("type annotation");
    let outer = &module.arenas.exprs[ty_idx];

    // Outer: pure arrow, no effects.
    assert!(
        matches!(
            outer,
            Expr::FnType {
                arrow: Arrow::Pure,
                effects: None,
                ..
            }
        ),
        "outer arrow should be pure with no effects"
    );

    // Inner: effectful arrow with one effect.
    let Expr::FnType { ret, .. } = outer else {
        return;
    };
    let inner = &module.arenas.exprs[*ret];
    assert!(
        matches!(
            inner,
            Expr::FnType { arrow: Arrow::Effectful, effects: Some(eff), .. }
            if eff.effects.len() == 1
        ),
        "inner arrow should be effectful with one effect"
    );
}
