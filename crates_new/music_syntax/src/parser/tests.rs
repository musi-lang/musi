use crate::{Lexer, ParseErrorKind, Program, SyntaxNodeKind, parse};

fn parse_kinds(text: &str) -> Vec<SyntaxNodeKind> {
    let parsed = parse(Lexer::new(text).lex());
    let mut out = Vec::new();
    for stmt in Program::cast(parsed.tree().root())
        .expect("root should cast")
        .statements()
    {
        if let Some(expr) = stmt.expression() {
            out.push(expr.syntax().kind());
        }
    }
    out
}

fn assert_has_parse_error(text: &str, predicate: impl Fn(ParseErrorKind) -> bool) {
    let parsed = parse(Lexer::new(text).lex());
    assert!(
        parsed.errors().iter().any(|e| predicate(e.kind)),
        "expected parse error for input:\n{text}\nerrors: {:?}",
        parsed.errors()
    );
}

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

#[test]
fn parses_all_atom_forms_smoke() {
    let kinds = parse_kinds(
        r#"
let x := 1;
import "std/io";
resume x;
perform x;
handle x with h of (| x => x);
case x of (| _ => 0);
foreign "c" let puts (msg : CString) : Int;
export let y := 2;
let Option[T] := data { | Some : T | None };
let Console := effect { let write (text : String) : Unit; };
let Eq[T] := class { let (=) (a : T, b : T) : Bool; };
instance Eq[Int] { };
quote (x + 1);
quote { x; };
@link(name := "c") foreign "c" let puts (msg : CString) : Int;
`hello ${x}`;
{ x := 1 };
.Some(1);
[1, 2, 3];
[1]x;
(x);
(x; y;);
"#,
    );
    assert!(!kinds.is_empty());
}

#[test]
fn error_expected_token_semicolon() {
    assert_has_parse_error("let x := 1", |k| {
        matches!(
            k,
            ParseErrorKind::ExpectedToken {
                expected: crate::TokenKind::Semicolon,
                ..
            }
        )
    });
}

#[test]
fn error_expected_expression() {
    assert_has_parse_error(";", |k| matches!(k, ParseErrorKind::ExpectedExpression { .. }));
}

#[test]
fn error_expected_pattern() {
    assert_has_parse_error("let := 1;", |k| matches!(k, ParseErrorKind::ExpectedPattern { .. }));
}

#[test]
fn error_expected_member() {
    assert_has_parse_error("effect { 1 };", |k| matches!(k, ParseErrorKind::ExpectedMember { .. }));
}

#[test]
fn error_expected_identifier() {
    assert_has_parse_error("@; 1;", |k| matches!(k, ParseErrorKind::ExpectedIdentifier { .. }));
}

#[test]
fn error_expected_splice_target() {
    assert_has_parse_error("quote (#);", |k| {
        matches!(k, ParseErrorKind::ExpectedSpliceTarget { .. })
    });
}

#[test]
fn error_expected_operator_member_name() {
    assert_has_parse_error("effect { let 1; };", |k| {
        matches!(k, ParseErrorKind::ExpectedOperatorMemberName { .. })
    });
}

#[test]
fn error_expected_array_dimension() {
    assert_has_parse_error("[;]x;", |k| {
        matches!(k, ParseErrorKind::ExpectedArrayDimension { .. })
    });
}

#[test]
fn error_expected_field_target() {
    assert_has_parse_error("x.;", |k| matches!(k, ParseErrorKind::ExpectedFieldTarget { .. }));
}

#[test]
fn error_expected_constraint_operator() {
    assert_has_parse_error("let x where Eq = Int := 1;", |k| {
        matches!(k, ParseErrorKind::ExpectedConstraintOperator { .. })
    });
}

#[test]
fn error_expected_attr_value() {
    assert_has_parse_error("@a(; ) 1;", |k| matches!(k, ParseErrorKind::ExpectedAttrValue { .. }));
}

#[test]
fn error_splice_outside_quote_is_reported() {
    assert_has_parse_error("#x;", |k| matches!(k, ParseErrorKind::SpliceOutsideQuote));
}

#[test]
fn error_non_associative_chain_is_reported() {
    assert_has_parse_error("a < b < c;", |k| matches!(k, ParseErrorKind::NonAssociativeChain));
}

#[test]
fn parses_in_membership_expr() {
    let parsed = parse(Lexer::new("a in b;").lex());
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
}

#[test]
fn error_non_associative_chain_with_in_is_reported() {
    assert_has_parse_error("a in b in c;", |k| matches!(k, ParseErrorKind::NonAssociativeChain));
}
