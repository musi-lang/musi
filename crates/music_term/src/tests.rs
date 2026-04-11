use crate::{SyntaxShape, SyntaxTerm, TypeTerm, TypeTermKind, parse_type_term};

#[test]
fn parses_expr_and_module_syntax_terms() {
    let expr = SyntaxTerm::parse(SyntaxShape::Expr, "40 + 2").unwrap();
    let module = SyntaxTerm::parse(SyntaxShape::Module, "export let answer : Int := 42;").unwrap();
    assert_eq!(expr.text(), "40 + 2");
    assert_eq!(module.shape(), SyntaxShape::Module);
}

#[test]
fn parses_and_formats_type_terms() {
    let term = parse_type_term("(Int, String) -> Bool").unwrap();
    assert_eq!(term.to_string(), "(Int, String) -> Bool");
}

#[test]
fn roundtrips_type_term_json() {
    let term = TypeTerm::new(TypeTermKind::Named {
        module: None,
        name: "Array".into(),
        args: vec![TypeTerm::new(TypeTermKind::Int)].into_boxed_slice(),
    });
    let json = term.to_json();
    let reparsed = TypeTerm::from_json(&json).unwrap();
    assert_eq!(reparsed, term);
}
