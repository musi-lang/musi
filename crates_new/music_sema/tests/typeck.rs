use music_basic::SourceMap;
use music_lex::Lexer;
use music_names::Interner;
use music_parse::parse;
use music_resolve::ResolveOptions;
use music_sema::{SemaError, SemaErrorKind, SemaErrorKinds, analyze_module};

fn analyze_text(text: &str) -> SemaErrorKinds {
    analyze_text_full(text).into_iter().map(|e| e.kind).collect()
}

fn analyze_text_full(text: &str) -> Vec<SemaError> {
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
    assert!(kinds.iter().any(|k| matches!(k, SemaErrorKind::TypeMismatch { .. })));
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
    assert!(kinds.iter().any(|k| matches!(k, SemaErrorKind::MissingWithClause)));
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
    assert!(kinds.iter().any(|k| matches!(k, SemaErrorKind::MissingWithClause)));
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
    assert!(kinds.iter().any(|k| matches!(k, SemaErrorKind::MissingWithClause)));
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
        !kinds.iter().any(|k| matches!(k, SemaErrorKind::EffectNotDeclared { .. })),
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
    assert!(kinds.iter().any(|k| matches!(k, SemaErrorKind::ResumeOutsideOpClause)));
}
