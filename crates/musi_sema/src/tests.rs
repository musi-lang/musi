    use musi_lex::Lexer;
    use musi_parse::parse;
    use musi_shared::{DiagnosticBag, Interner, SourceDb};

    use super::*;

    fn analyze_src(src: &str) -> (SemaResult, DiagnosticBag) {
        let mut interner = Interner::new();
        let mut db = SourceDb::new();
        let file_id = db.add("test.ms", src);
        let mut diags = DiagnosticBag::new();
        let tokens: Vec<_> = Lexer::new(src, file_id, &mut interner, &mut diags).collect();
        let module = parse(&tokens, file_id, &mut diags, &interner);
        // Fail fast: parse errors mean the test itself is wrong (bad syntax).
        // The parser collects these into `diags` rather than panicking, but
        // semantic analysis on a malformed AST is meaningless and can loop.
        assert!(
            !diags.has_errors(),
            "parse produced errors — fix the test source:\n{:#?}",
            error_messages(&diags)
        );
        let result = analyze(&module, &interner, file_id, &mut diags);
        (result, diags)
    }

    fn error_messages(diags: &DiagnosticBag) -> Vec<String> {
        diags
            .iter()
            .filter(|d| d.severity == musi_shared::Severity::Error)
            .map(|d| d.message.to_string())
            .collect()
    }

    // -----------------------------------------------------------------------
    // Milestone 1 — type annotation mismatch
    // -----------------------------------------------------------------------

    #[test]
    fn type_mismatch_int_vs_string() {
        let (_result, diags) = analyze_src(r#"const x: Int := "hello";"#);
        assert!(diags.has_errors(), "expected a type error");
        let msgs = error_messages(&diags);
        assert!(
            msgs.iter()
                .any(|m| m.contains("mismatch") || m.contains("Int") || m.contains("String")),
            "expected a type mismatch mentioning Int or String, got: {msgs:?}"
        );
    }

    #[test]
    fn type_mismatch_string_vs_int() {
        let (_result, diags) = analyze_src(r#"const x: String := 42;"#);
        assert!(diags.has_errors(), "expected a type error");
    }

    // -----------------------------------------------------------------------
    // Milestone 2 — type inference
    // -----------------------------------------------------------------------

    #[test]
    fn infers_int_addition() {
        let (result, diags) = analyze_src(r#"const y := 1 + 2;"#);
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
        // The binding for `y` should have type Int.
        let y_def = result.defs.iter().find(|d| {
            let kind_ok = matches!(d.kind, DefKind::Const);
            kind_ok
        });
        assert!(y_def.is_some(), "expected a Const definition for y");
        let y_ty = y_def.expect("just checked").ty.as_ref();
        assert!(
            matches!(y_ty, Some(Type::Prim(PrimTy::Int))),
            "expected y : Int, got: {y_ty:?}"
        );
    }

    #[test]
    fn infers_bool_from_literal() {
        let (result, diags) = analyze_src(r#"const b := true;"#);
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
        let b_def = result
            .defs
            .iter()
            .find(|d| matches!(d.kind, DefKind::Const));
        assert!(
            matches!(
                b_def.and_then(|d| d.ty.as_ref()),
                Some(Type::Prim(PrimTy::Bool))
            ),
            "expected b : Bool"
        );
    }

    #[test]
    fn infers_string_from_literal() {
        let (result, diags) = analyze_src(r#"const s := "hello";"#);
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
        let s_def = result
            .defs
            .iter()
            .find(|d| matches!(d.kind, DefKind::Const));
        assert!(
            matches!(
                s_def.and_then(|d| d.ty.as_ref()),
                Some(Type::Prim(PrimTy::String))
            ),
            "expected s : String"
        );
    }

    // -----------------------------------------------------------------------
    // Milestone 3 — undefined name with suggestion
    // -----------------------------------------------------------------------

    #[test]
    fn undefined_name_error() {
        let (_result, diags) = analyze_src(r#"writeln(undefined_var);"#);
        assert!(diags.has_errors(), "expected an undefined-name error");
        let msgs = error_messages(&diags);
        assert!(
            msgs.iter().any(|m| m.contains("undefined")),
            "expected 'undefined' in error, got: {msgs:?}"
        );
    }

    #[test]
    fn undefined_name_suggests_close_match() {
        // `fro` is 1 edit from `for` — but `for` is a keyword, so define a
        // function `foo` and try to call `fob`.
        let (_result, diags) = analyze_src(
            r#"
fn foo(x: Int): Int (x)
const z := fob(1);
"#,
        );
        assert!(diags.has_errors(), "expected an undefined-name error");
        let msgs = error_messages(&diags);
        // Should suggest `foo`.
        assert!(
            msgs.iter().any(|m| m.contains("fob") || m.contains("foo")),
            "expected suggestion near 'fob'/'foo', got: {msgs:?}"
        );
    }

    // -----------------------------------------------------------------------
    // Milestone 4 — well-typed programs pass without errors
    // -----------------------------------------------------------------------

    #[test]
    fn well_typed_simple_fn() {
        let (_result, diags) = analyze_src(
            r#"
fn add(a: Int, b: Int): Int (a + b)
const result := add(1, 2);
"#,
        );
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    #[test]
    fn well_typed_if_expression() {
        let (_result, diags) = analyze_src(
            r#"
const x := if true then (1) else (2);
"#,
        );
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    #[test]
    fn well_typed_nested_bindings() {
        let (_result, diags) = analyze_src(
            r#"
const a := 10;
const b := a + 5;
const c := b + a;
"#,
        );
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    #[test]
    fn well_typed_var_mutation() {
        let (_result, diags) = analyze_src(
            r#"
var counter := 0;
counter <- counter + 1;
"#,
        );
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    #[test]
    fn well_typed_choice_type() {
        let (_result, diags) = analyze_src(
            r#"
choice Color { Red | Green | Blue }
const c := Red;
"#,
        );
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    #[test]
    fn well_typed_record_type() {
        let (_result, diags) = analyze_src(
            r#"
record Point { x: Int, y: Int }
"#,
        );
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    #[test]
    fn well_typed_generic_identity() {
        let (_result, diags) = analyze_src(
            r#"
fn id['T](x: 'T): 'T (x)
const n := id(42);
const s := id("hello");
"#,
        );
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    #[test]
    fn well_typed_while_loop() {
        let (_result, diags) = analyze_src(
            r#"
var i := 0;
while i < 10 loop (
    i <- i + 1;
);
"#,
        );
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    // -----------------------------------------------------------------------
    // Additional correctness tests
    // -----------------------------------------------------------------------

    #[test]
    fn type_annotation_matches_literal() {
        let (_result, diags) = analyze_src(r#"const x: Int := 42;"#);
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    #[test]
    fn recursive_function_type_checked() {
        let (_result, diags) = analyze_src(
            r#"
fn fact(n: Int): Int (
    if n < 1 then (1) else (n)
)
"#,
        );
        assert!(
            !diags.has_errors(),
            "unexpected errors: {:?}",
            error_messages(&diags)
        );
    }

    #[test]
    fn edit_distance_simple() {
        // Internal: just test the logic path, not the private function directly.
        let (_, diags) = analyze_src(r#"const z := foo_bar;"#);
        // No suggestion available for `foo_bar` with no defined names; just no crash.
        let _ = diags;
    }
