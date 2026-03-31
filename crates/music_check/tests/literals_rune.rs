mod support;

use support::analyze_text;

#[test]
fn test_rune_literals_typecheck_as_int() {
    let kinds = analyze_text("let r : Int := 'a'; r;");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}
