use music_ast::data::AstData;
use music_ast::expr::ExprKind;
use music_ast::ty::TyKind;
use music_found::Interner;
use music_lex::Lexer;

use crate::ParseError;

fn parse_ty_from(source: &str) -> (AstData, Vec<ParseError>) {
    let full = String::from("let x : ") + source + " := 0;";
    let (tokens, lex_errors) = Lexer::new(&full).lex();
    assert!(
        lex_errors.is_empty(),
        "unexpected lex errors: {lex_errors:?}"
    );
    let mut interner = Interner::new();
    crate::parse(&tokens, &full, &mut interner)
}

fn extract_ty_kind(ast: &AstData) -> &TyKind {
    assert_eq!(ast.root.len(), 1);
    match &ast.exprs.get(ast.root[0]).kind {
        ExprKind::Let(binding) => {
            let sig = binding.sig.as_ref().expect("has signature");
            let ty_id = sig.ret_ty.expect("has return type");
            &ast.types.get(ty_id).kind
        }
        other => panic!("expected Let, got {other:?}"),
    }
}

#[test]
fn named_type() {
    let (ast, errors) = parse_ty_from("Int");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::Named { args, .. } if args.is_empty()));
}

#[test]
fn named_with_args() {
    let (ast, errors) = parse_ty_from("Option of Int");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::Named { args, .. } if args.len() == 1));
}

#[test]
fn arrow_type() {
    let (ast, errors) = parse_ty_from("Int -> String");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::Arrow { .. }));
}

#[test]
fn effect_arrow_type() {
    let (ast, errors) = parse_ty_from("Int ~> String");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::EffectArrow { .. }));
}

#[test]
fn sum_type() {
    let (ast, errors) = parse_ty_from("Int + String");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::Sum(members) if members.len() == 2));
}

#[test]
fn product_type() {
    let (ast, errors) = parse_ty_from("Int * String");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::Product(members) if members.len() == 2));
}

#[test]
fn mut_type() {
    let (ast, errors) = parse_ty_from("mut Int");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::Mut(_)));
}

#[test]
fn option_type() {
    let (ast, errors) = parse_ty_from("?Int");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::Option(_)));
}

#[test]
fn tuple_type() {
    let (ast, errors) = parse_ty_from("(Int, String)");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::Tuple(items) if items.len() == 2));
}

#[test]
fn array_type() {
    let (ast, errors) = parse_ty_from("[3]Int");
    assert!(errors.is_empty());
    assert!(matches!(extract_ty_kind(&ast), TyKind::Array { dims, .. } if dims.len() == 1));
}

#[test]
fn ty_args_trailing_comma() {
    let (ast, errors) = parse_ty_from("Map of (Int, String,)");
    assert!(errors.is_empty(), "errors: {errors:?}");
    assert!(matches!(extract_ty_kind(&ast), TyKind::Named { args, .. } if args.len() == 1));
}
