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
fn parses_receiver_prefixed_let_statement() {
    let parsed = parse(Lexer::new("let (self : Int).abs () : Int := self;").lex());
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
}

#[test]
fn parses_mut_receiver_prefixed_let_statement() {
    let parsed = parse(Lexer::new("let (mut self : Buffer).push (value : Int) := self;").lex());
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
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
fn parses_instance_expr_with_target_then_where() {
    let parsed = parse(Lexer::new("instance Eq[Int] where Int : Show { };").lex());
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
	handle x using h { x => x; };
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
    assert_has_parse_error(";", |k| {
        matches!(k, ParseErrorKind::ExpectedExpression { .. })
    });
}

#[test]
fn error_expected_pattern() {
    assert_has_parse_error("let := 1;", |k| {
        matches!(k, ParseErrorKind::ExpectedPattern { .. })
    });
}

#[test]
fn error_expected_member() {
    assert_has_parse_error("effect { 1 };", |k| {
        matches!(k, ParseErrorKind::ExpectedMember { .. })
    });
}

#[test]
fn error_expected_identifier() {
    assert_has_parse_error("@; 1;", |k| {
        matches!(k, ParseErrorKind::ExpectedIdentifier { .. })
    });
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
fn error_expected_field_target() {
    assert_has_parse_error("x.;", |k| {
        matches!(k, ParseErrorKind::ExpectedFieldTarget { .. })
    });
}

#[test]
fn error_expected_constraint_operator() {
    assert_has_parse_error("let x where Eq = Int = 1;", |k| {
        matches!(k, ParseErrorKind::ExpectedConstraintOperator { .. })
    });
}

#[test]
fn error_expected_attr_value() {
    assert_has_parse_error("@a(; ) 1;", |k| {
        matches!(k, ParseErrorKind::ExpectedAttrValue { .. })
    });
}

#[test]
fn error_splice_outside_quote_is_reported() {
    assert_has_parse_error("#x;", |k| matches!(k, ParseErrorKind::SpliceOutsideQuote));
}

#[test]
fn error_non_associative_chain_is_reported() {
    assert_has_parse_error("a < b < c;", |k| {
        matches!(k, ParseErrorKind::NonAssociativeChain)
    });
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
    assert_has_parse_error("a in b in c;", |k| {
        matches!(k, ParseErrorKind::NonAssociativeChain)
    });
}

#[test]
fn parses_case_and_handle_with_trailing_pipe() {
    let parsed =
        parse(Lexer::new("case x of (| _ => 0 |); handle x using h { op(a, b) => a; };").lex());
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
}

#[test]
fn parses_new_signature_order_and_array_type_syntax() {
    let parsed = parse(
        Lexer::new("let f[T] (xs : []Int) : [2]Int where T : Eq using { Console } := xs;").lex(),
    );
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
}

#[test]
fn parses_handler_type_annotation() {
    let parsed = parse(
        Lexer::new(
            r"
            let Console := effect { let readln () : Int; };
            let h : using Console (Int -> Int) := using Console { value => value; readln(k) => resume 41; };
        ",
        )
        .lex(),
    );
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
}

#[test]
fn parses_attr_values_and_patterns_with_trailing_commas() {
    let parsed =
        parse(Lexer::new("@a(.Tag(1,), [1,], {x := 1,}) let (.Some(x,), [y,]) := z;").lex());
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
}

#[test]
fn parses_attr_record_with_repeated_trailing_commas() {
    let parsed = parse(Lexer::new("@a({x := 1,,}) let y := z;").lex());
    assert!(
        parsed.errors().is_empty(),
        "unexpected errors: {:?}",
        parsed.errors()
    );
}
