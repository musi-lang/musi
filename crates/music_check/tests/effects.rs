mod support;

use music_check::SemaErrorKind;

use support::analyze_text;

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
fn test_function_alias_preserves_declared_effects() {
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
fn test_allows_effect_when_declared_in_with_clause() {
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
fn test_allows_effect_when_row_is_open() {
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
fn test_reports_resume_outside_op_clause() {
    let kinds = analyze_text("resume ();");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::ResumeOutsideOpClause))
    );
}

#[test]
fn test_perform_reports_arg_count_mismatch() {
    let kinds = analyze_text(
        r#"
let Console := effect { let writeln (msg : String) : Unit; };
let f () with { Console } : Unit := perform Console.writeln("x", "y");
f();
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::PerformArgCountMismatch { .. }))
    );
}

#[test]
fn test_handle_reports_missing_op_clause() {
    let kinds = analyze_text(
        r#"
let Console := effect { let writeln (msg : String) : Unit; let readln () : String; };
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
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::HandleClauseMissingOp { .. }))
    );
}

#[test]
fn test_handle_reports_clause_param_count_mismatch() {
    let kinds = analyze_text(
        r#"
let Console := effect { let writeln (msg : String) : Unit; };
let work () with { Console } : Int := (
  perform Console.writeln("x");
  1
);
handle work() with Console of (
| v => v
| writeln(k) => resume ()
);
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::HandleClauseParamCountMismatch { .. }))
    );
}
