use music_ast::data::AstData;
use music_found::Interner;
use music_lex::Lexer;

use crate::ParseError;
use crate::parse;

fn parse_from(source: &str) -> (AstData, Vec<ParseError>) {
    let (tokens, lex_errors) = Lexer::new(source).lex();
    assert!(
        lex_errors.is_empty(),
        "unexpected lex errors: {lex_errors:?}"
    );
    let mut interner = Interner::new();
    parse(&tokens, source, &mut interner)
}

#[test]
fn empty_source_produces_empty_root() {
    let (ast, errors) = parse_from("");
    assert!(errors.is_empty());
    assert!(ast.root.is_empty());
}

#[test]
fn single_statement_produces_one_root_node() {
    let (ast, errors) = parse_from("42;");
    assert!(errors.is_empty());
    assert_eq!(ast.root.len(), 1);
}

#[test]
fn multiple_statements() {
    let (ast, errors) = parse_from("1; 2; 3;");
    assert!(errors.is_empty());
    assert_eq!(ast.root.len(), 3);
}

#[test]
fn missing_semicolon_recovers() {
    let (ast, errors) = parse_from("1 2;");
    // First statement fails (missing ;), second parses
    assert!(!errors.is_empty());
    // Recovery should have consumed past the error
    assert!(!ast.root.is_empty());
}

#[test]
fn error_recovery_continues_after_bad_token() {
    let (ast, errors) = parse_from("); 42;");
    assert!(!errors.is_empty());
    // Should recover and parse 42
    assert!(!ast.root.is_empty());
}
