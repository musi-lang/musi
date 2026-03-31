mod support;

use support::analyze_text;

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
