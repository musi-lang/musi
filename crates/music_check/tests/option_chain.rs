mod support;

use music_check::SemaErrorKind;

use support::{analyze_text, analyze_text_full};

#[test]
fn test_optional_chain_requires_option_lang_item() {
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
fn test_optional_chain_typechecks_with_option_lang_item() {
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
fn test_forced_chain_introduces_abort_effect() {
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
fn test_forced_chain_allowed_when_abort_declared() {
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
