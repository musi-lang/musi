#![allow(unused_imports)]

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

mod success {
    use super::*;

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
    fn parses_known_prefix_expr() {
        let parsed = parse(Lexer::new("let x := known (1 + 2);").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_known_value_param() {
        let parsed = parse(Lexer::new("let scale (known n : Int, x : Int) : Int := x * n;").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_existential_and_opaque_capability_types() {
        let parsed = parse(
            Lexer::new(
                "let writeAny (writer : any Writer) : Int := 0; let writeSome (writer : some Writer) : Int := 0;",
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
    fn parses_receiver_method_let_head() {
        let parsed = parse(Lexer::new("let (self : Int).abs () : Int := self;").lex());
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
    fn old_surface_words_parse_as_identifiers() {
        for word in ["class", "instance", "via", "using", "with", "provide"] {
            let parsed = parse(Lexer::new(&format!("let {word} := 1;")).lex());
            assert!(
                parsed.errors().is_empty(),
                "{word} produced parse errors: {:?}",
                parsed.errors()
            );
        }
    }

    #[test]
    fn parses_compound_optional_tokens() {
        let parsed = parse(Lexer::new("a?.b; a!.b; a ?? b;").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_mathematical_range_forms() {
        let parsed = parse(
            Lexer::new(
                r"
                a .. b;
                a ..< b;
                a <.. b;
                a <..< b;
                a ..;
                a <..;
                .. a;
                ..< a;
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
    fn parses_all_atom_forms_smoke() {
        let kinds = parse_kinds(
            r#"
	let x := 1;
	import "std/io";
	resume x;
	ask x;
	handle x answer h;
	match x (| _ => 0);
	native "c" let puts (msg : CString) : Int;
	export let y := 2;
	let Option[T] := data { | Some(T) | None };
	let Console := effect { let write (text : String) : Unit; };
	let Write := shape { let write (text : String) : Unit; };
	given Write { let write (text : String) : Unit := (); };
	answer Console { value => value; };
	answer x;
	given x;
	a catch b;
	quote (x + 1);
	quote { x; };
	@link(name := "c") native "c" let puts (msg : CString) : Int;
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
    fn parses_export_block_as_grouping_sugar() {
        let parsed = parse(
            Lexer::new(
                r"
            export (
              let x := 1;
              let y := 2;
            );
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
    fn parses_import_block_and_bound_tuple_import_block() {
        let parsed = parse(
            Lexer::new(
                r#"
            import (
              "std/io";
              "std/cmp";
            );
            let (IO, Cmp) := import (
              "std/io";
              "std/cmp";
            );
        "#,
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
    fn parses_import_aliasing_through_let_and_of_identifier() {
        let parsed = parse(Lexer::new(r#"let mod := import "./mod"; let of := 1;"#).lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_as_for_pattern_and_type_test_aliases() {
        let parsed = parse(
            Lexer::new(
                r"
            match value (
              | .Some(x) as whole => whole
            );
            value :? T as refined;
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
    fn rejects_binder_position_mut() {
        assert!(
            !parse(Lexer::new("let mut x := 1;").lex())
                .errors()
                .is_empty()
        );
        assert!(
            !parse(Lexer::new("let f(mut x : Int) : Int := x;").lex())
                .errors()
                .is_empty()
        );
    }

    #[test]
    fn parses_backslash_lambda_expr() {
        let kinds = parse_kinds(r"\(x : Int) : Int => x;");
        assert_eq!(kinds, vec![SyntaxNodeKind::LambdaExpr]);
    }

    #[test]
    fn parses_some_and_any_as_type_modifiers_only() {
        let parsed = parse(
            Lexer::new(
                "let writeAny (writer : any Writer) : Int := 0; let writeSome (writer : some Writer) : Int := 0;",
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
    fn parses_named_variant_payload_definitions_and_uses() {
        let parsed = parse(
            Lexer::new(
                r"
            let Port := data {
              | Configured(port : Int, secure : Bool)
              | Default
            };
            let port : Port := .Configured(secure := 0 = 0, port := 8080);
            match port (
              | .Configured(port, secure := _) => port
              | .Default => 0
            );
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
    fn parses_named_call_arguments() {
        let parsed = parse(
            Lexer::new(
                r"
            let render (port : Int, secure : Bool) : Int := port;
            render(port := 8080, secure := 0 = 0);
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
    fn parses_in_membership_expr() {
        let parsed = parse(Lexer::new("a in b;").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_case_and_handle_with_trailing_pipe() {
        let parsed = parse(Lexer::new("match x (| _ => 0 |); handle x answer h;").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_new_signature_order_and_array_type_syntax() {
        let parsed = parse(Lexer::new("let f[T] (xs : []Int) : [2]Int where T : Eq := xs;").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_tuple_and_array_destructuring_let_patterns() {
        let parsed = parse(
            Lexer::new(
                "let pair := (1, 2); let items := [3, 4]; let (a, b) := pair; let [c, d] := items;",
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
    fn parses_handler_type_annotation() {
        let parsed = parse(
            Lexer::new(
                r"
            let Console := effect { let readLine () : Int; };
            let h : answer Console (Int -> Int) := answer Console;
            handle x answer h;
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

    #[test]
    fn parses_unsafe_block_expr() {
        let parsed = parse(
            Lexer::new(
                r#"
            native "c" let clock () : Int;
            let value := unsafe { clock(); };
        "#,
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
    fn parses_pin_expr_inside_unsafe_block() {
        let parsed = parse(
            Lexer::new(
                r"
            let xs := [1, 2];
            let value := unsafe { pin xs as pinned in 1; };
        ",
            )
            .lex(),
        );
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
        let kinds = parse_kinds("pin xs as pinned in 1;");
        assert_eq!(kinds, vec![SyntaxNodeKind::PinExpr]);
    }

    #[test]
    fn parses_partial_modifier_on_let() {
        let parsed = parse(Lexer::new("partial let parseInt(text : String) : Int := 0;").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
        assert_eq!(
            parse_kinds("partial let x := 1;"),
            vec![SyntaxNodeKind::AttributedExpr]
        );
    }

    #[test]
    fn parses_type_equality_operator() {
        let parsed = parse(Lexer::new("let ok : Bool := T ~= U;").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_indexed_variant_result_clause() {
        let parsed = parse(Lexer::new("let Vec[T, n] := data { | Nil() -> Vec[T, 0] | Cons(head : T, tail : Vec[T, n]) -> Vec[T, n + 1] };").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_type_equality_constraint() {
        let parsed =
            parse(Lexer::new("let same[A, B] (value : A) : A where A ~= B := value;").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }

    #[test]
    fn parses_given_and_answer_prefix_forms() {
        let parsed = parse(Lexer::new("given x; answer x;").lex());
        assert!(
            parsed.errors().is_empty(),
            "unexpected errors: {:?}",
            parsed.errors()
        );
    }
}

mod failure {
    use super::*;

    #[test]
    fn rejects_bare_paren_lambda_expr() {
        assert_has_parse_error("(x : Int) => x;", |kind| {
            matches!(kind, ParseErrorKind::ExpectedToken { .. })
        });
    }

    #[test]
    fn rejects_reserved_keyword_binding_names() {
        assert_has_parse_error("let some [T] (value : T) : T := value;", |kind| {
            matches!(
                kind,
                ParseErrorKind::ReservedKeywordIdentifier {
                    keyword: crate::TokenKind::KwSome
                }
            )
        });
        assert_has_parse_error("let any := 1;", |kind| {
            matches!(
                kind,
                ParseErrorKind::ReservedKeywordIdentifier {
                    keyword: crate::TokenKind::KwAny
                }
            )
        });
        assert_has_parse_error("let value := { some := 1 };", |kind| {
            matches!(
                kind,
                ParseErrorKind::ReservedKeywordIdentifier {
                    keyword: crate::TokenKind::KwSome
                }
            )
        });
    }

    #[test]
    fn rejects_import_as_alias() {
        assert_has_parse_error(r#"import "./mod" as mod;"#, |kind| {
            matches!(kind, ParseErrorKind::ExpectedToken { .. })
        });
        assert_has_parse_error(r#"let mod := import "./mod" as mod;"#, |kind| {
            matches!(kind, ParseErrorKind::ExpectedToken { .. })
        });
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
    fn error_non_associative_chain_with_in_is_reported() {
        assert_has_parse_error("a in b in c;", |k| {
            matches!(k, ParseErrorKind::NonAssociativeChain)
        });
    }

    #[test]
    fn error_mut_parenthesized_dot_let_head_is_rejected() {
        assert_has_parse_error("let (mut self : Buffer).push (value : Int) := self;", |k| {
            matches!(
                k,
                ParseErrorKind::ExpectedToken { .. }
                    | ParseErrorKind::ExpectedPattern { .. }
                    | ParseErrorKind::ReservedKeywordIdentifier { .. }
            )
        });
    }

    #[test]
    fn error_partial_modifier_requires_let() {
        assert_has_parse_error("partial x;", |k| {
            matches!(
                k,
                ParseErrorKind::ExpectedToken {
                    expected: crate::TokenKind::KwLet,
                    ..
                }
            )
        });
    }

    #[test]
    fn error_custom_symbolic_infix_is_rejected() {
        assert_has_parse_error("a == b;", |k| {
            matches!(k, ParseErrorKind::ExpectedToken { .. })
        });
    }
}
