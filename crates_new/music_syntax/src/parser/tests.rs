use crate::{Lexer, Program, SyntaxNodeKind, parse};

#[test]
fn parses_simple_let_statement() {
    let parsed = parse(Lexer::new("let x := 1;").lex());
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
    let program = Program::cast(parsed.tree().root()).expect("root should cast");
    let stmt = program.statements().next().expect("statement expected");
    let expr = stmt.expression().expect("expression expected");
    assert_eq!(expr.syntax().kind(), SyntaxNodeKind::LetExpr);
}

#[test]
fn parses_apply_and_index_chain() {
    let parsed = parse(Lexer::new("foo[Bar].[0];").lex());
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
}

#[test]
fn parses_instance_expr() {
    let parsed = parse(Lexer::new("instance Eq[Int] { };").lex());
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
}
