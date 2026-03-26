use music_ast::data::AstData;
use music_ast::expr::ExprKind;
use music_ast::pat::PatKind;
use music_lex::Lexer;
use music_shared::{Interner, Literal};

use crate::ParseError;

fn parse_pat_from(source: &str) -> (AstData, Vec<ParseError>) {
    let full = String::from("case x of (") + source + " => 0);";
    let (tokens, lex_errors) = Lexer::new(&full).lex();
    assert!(
        lex_errors.is_empty(),
        "unexpected lex errors: {lex_errors:?}"
    );
    let mut interner = Interner::new();
    crate::parse(&tokens, &full, &mut interner)
}

fn extract_pat_kind(ast: &AstData) -> &PatKind {
    assert_eq!(ast.root.len(), 1);
    match &ast.exprs.get(ast.root[0]).kind {
        ExprKind::Case(data) => {
            assert!(!data.arms.is_empty());
            &ast.pats.get(data.arms[0].pat).kind
        }
        other => panic!("expected Case, got {other:?}"),
    }
}

#[test]
fn wildcard_pat() {
    let (ast, errors) = parse_pat_from("_");
    assert!(errors.is_empty());
    assert!(matches!(extract_pat_kind(&ast), PatKind::Wildcard));
}

#[test]
fn literal_pat() {
    let (ast, errors) = parse_pat_from("42");
    assert!(errors.is_empty());
    assert!(matches!(
        extract_pat_kind(&ast),
        PatKind::Lit(Literal::Int(42))
    ));
}

#[test]
fn bind_pat() {
    let (ast, errors) = parse_pat_from("x");
    assert!(errors.is_empty());
    assert!(matches!(extract_pat_kind(&ast), PatKind::Bind(_)));
}

#[test]
fn as_pat() {
    let (ast, errors) = parse_pat_from("x as y");
    assert!(errors.is_empty());
    assert!(matches!(extract_pat_kind(&ast), PatKind::As { .. }));
}

#[test]
fn variant_pat() {
    let (ast, errors) = parse_pat_from(".Some(a)");
    assert!(errors.is_empty());
    assert!(matches!(extract_pat_kind(&ast), PatKind::Variant { fields, .. } if fields.len() == 1));
}

#[test]
fn record_pat() {
    let (ast, errors) = parse_pat_from(".{ x, y }");
    assert!(errors.is_empty());
    assert!(matches!(extract_pat_kind(&ast), PatKind::Record(fields) if fields.len() == 2));
}

#[test]
fn tuple_pat() {
    let (ast, errors) = parse_pat_from("(a, b)");
    assert!(errors.is_empty());
    assert!(matches!(extract_pat_kind(&ast), PatKind::Tuple(pats) if pats.len() == 2));
}

#[test]
fn array_pat() {
    let (ast, errors) = parse_pat_from("[a, b]");
    assert!(errors.is_empty());
    assert!(matches!(extract_pat_kind(&ast), PatKind::Array(pats) if pats.len() == 2));
}

#[test]
fn or_pat() {
    let (ast, errors) = parse_pat_from("a or b");
    assert!(errors.is_empty());
    assert!(matches!(extract_pat_kind(&ast), PatKind::Or(alts) if alts.len() == 2));
}
