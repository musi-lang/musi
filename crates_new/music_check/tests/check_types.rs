use music_basic::SourceMap;
use music_check::{SemaErrorKind, SemaErrorKinds, SemaErrors, analyze_module};
use music_lex::Lexer;
use music_names::Interner;
use music_parse::parse;
use music_resolve::ResolveOptions;

fn analyze_text(text: &str) -> SemaErrorKinds {
    analyze_text_full(text)
        .into_iter()
        .map(|e| e.kind)
        .collect()
}

fn analyze_text_full(text: &str) -> SemaErrors {
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", text);

    let lexed = Lexer::new(text).lex();
    let parsed = parse(source_id, &lexed);
    assert!(parsed.errors().is_empty(), "test inputs must parse");

    let mut interner = Interner::default();
    let analyzed = analyze_module(
        parsed.tree(),
        &sources,
        &mut interner,
        ResolveOptions::default(),
    );
    assert!(
        analyzed.resolve_errors.is_empty(),
        "test inputs must resolve, got {:#?}",
        analyzed.resolve_errors
    );
    analyzed.check_errors
}

#[test]
fn test_typechecks_simple_let_annot() {
    let kinds = analyze_text("let x : Int := 1; x;");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_reports_type_mismatch() {
    let kinds = analyze_text("let x : Int := \"hi\"; x;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TypeMismatch { .. }))
    );
}

#[test]
fn test_fstring_interpolation_requires_string_parts() {
    let kinds = analyze_text("let x := 1; f\"{x}\";");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TypeMismatch { .. }))
    );
}

#[test]
fn test_calling_pure_fn_is_ok() {
    let kinds = analyze_text("let f () : Int := 1; f();");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_reports_missing_with_clause_for_effectful_fn() {
    let kinds = analyze_text(
        r#"
let Console := effect { let writeln (msg : String) : Unit; };
let f () : Unit := perform Console.writeln("x");
f();
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::MissingWithClause))
    );
}

#[test]
fn test_lambda_effects_are_latent_and_propagate_thru_aliases() {
    let kinds = analyze_text(
        r#"
let Console := effect { let writeln (msg : String) : Unit; };
let g () : Unit := (
  let f := () => perform Console.writeln("x");
  f()
);
g();
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::MissingWithClause))
    );
}

#[test]
fn function_alias_preserves_declared_effects() {
    let kinds = analyze_text(
        r#"
let Console := effect { let writeln (msg : String) : Unit; };
let f () with { Console } : Unit := perform Console.writeln("x");
let g := f;
let h () : Unit := g();
h();
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::MissingWithClause))
    );
}

#[test]
fn allows_effect_when_declared_in_with_clause() {
    let kinds = analyze_text(
        r#"
let Console := effect { let writeln (msg : String) : Unit; };
let f () with { Console } : Unit := perform Console.writeln("x");
f();
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn allows_effect_when_row_is_open() {
    let kinds = analyze_text(
        r#"
let Console := effect { let writeln (msg : String) : Unit; };
let IO := effect { let readln () : String; };
let f () with { Console, ...r } : Unit := (
  perform IO.readln();
  perform Console.writeln("x");
);
f();
"#,
    );
    assert!(
        !kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::EffectNotDeclared { .. })),
        "expected no EffectNotDeclared errors, got {kinds:?}"
    );
}

#[test]
fn typechecks_handle_value_and_op_clauses() {
    let errors = analyze_text_full(
        r#"
let Console := effect { let writeln (msg : String) : Unit; };
let work () with { Console } : Int := (
  perform Console.writeln("x");
  1
);

handle work() with Console of (
| v => v
| writeln(msg, k) => resume ()
);
"#,
    );
    assert!(errors.is_empty(), "expected no errors, got {errors:?}");
}

#[test]
fn reports_resume_outside_op_clause() {
    let kinds = analyze_text("resume ();");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::ResumeOutsideOpClause))
    );
}

#[test]
fn optional_chain_requires_option_lang_item() {
    let kinds = analyze_text(
        r#"
let Opt[T] := data { Some : T | None };
let P := data { x : Int };
let v : Opt[P] := .Some({ x := 1 });
v?.x;
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::OptionLangItemRequired))
    );
}

#[test]
fn optional_chain_typechecks_with_option_lang_item() {
    let errors = analyze_text_full(
        r#"
@musi.lang(name := "Option")
let Opt[T] := data { Some : T | None };
let P := data { x : Int };
let v : Opt[P] := .Some({ x := 1 });
v?.x;
"#,
    );
    assert!(errors.is_empty(), "expected no errors, got {errors:?}");
}

#[test]
fn forced_chain_introduces_abort_effect() {
    let kinds = analyze_text(
        r#"
@musi.lang(name := "Option")
let Opt[T] := data { Some : T | None };
let P := data { x : Int };
let f () : Int := (
  let v : Opt[P] := .Some({ x := 1 });
  v!.x
);
f();
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::MissingWithClause))
    );
}

#[test]
fn forced_chain_allowed_when_abort_declared() {
    let errors = analyze_text_full(
        r#"
@musi.lang(name := "Option")
let Opt[T] := data { Some : T | None };
let P := data { x : Int };
let f () with { Abort } : Int := (
  let v : Opt[P] := .Some({ x := 1 });
  v!.x
);
f();
"#,
    );
    assert!(errors.is_empty(), "expected no errors, got {errors:?}");
}

#[test]
fn symbolic_infix_typechecks_like_call() {
    let kinds = analyze_text(
        r#"
let (++) (a : Int, b : Int) : Int := a + b;
1 ++ 2;
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn tuple_projection_dot_int_typechecks() {
    let kinds = analyze_text("let t := (1, 2); t.0;");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn tuple_projection_dot_int_reports_out_of_range() {
    let kinds = analyze_text("let t := (1, 2); t.2;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TupleIndexOutOfRange { .. }))
    );
}

#[test]
fn test_tuple_index_brackets_typechecks() {
    let kinds = analyze_text("let t := (1, 2); t.[1];");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_record_computed_field_typechecks() {
    let kinds = analyze_text("let r := { x := 1, y := 2 }; r.[\"x\"];");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_data_computed_field_typechecks() {
    let kinds = analyze_text(
        r#"
let P := data { x : Int; y : Int; };
let p : P := { x := 1, y := 2 };
p.["x"];
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_computed_field_reports_missing() {
    let kinds = analyze_text("let r := { x := 1 }; r.[\"y\"];");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::FieldNotFound { .. }))
    );
}

#[test]
fn test_multi_index_reports_exceeds_array_nesting() {
    let kinds = analyze_text("let xs := [1, 2]; xs.[0, 0];");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::IndexExceedsArrayNesting))
    );
}

#[test]
fn test_int_literals_support_bases_and_underscore_separators() {
    let kinds = analyze_text(
        r#"
let a : Int := 0x2a;
let b : Int := 0b1010_0101;
let c : Int := 1_000_000;
(a; b; c);
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_float_literals_support_exponent() {
    let kinds = analyze_text(
        r#"
let a : Float := 1e3;
let b : Float := 3.14e-2;
(a; b);
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_rune_literals_typecheck_as_int() {
    let kinds = analyze_text("let r : Int := 'a'; r;");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_float_int_mismatch_reports_error() {
    let kinds = analyze_text("let x : Int := 1e3; x;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TypeMismatch { .. }))
    );
}

#[test]
fn test_multi_arg_call_typechecks() {
    let kinds = analyze_text(
        r#"
let add (a : Int, b : Int) : Int := a + b;
add(1, 2);
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_multi_arg_call_reports_mismatch() {
    let kinds = analyze_text(
        r#"
let add (a : Int, b : Int) : Int := a + b;
add(1, "x");
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TypeMismatch { .. }))
    );
}
