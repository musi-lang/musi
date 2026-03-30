use music_basic::SourceMap;
use music_lex::Lexer;
use music_parse::{ParseErrorKind, parse};

#[test]
fn test_attr_values_allow_data_only_subset() {
    let src = r#"
@when(os := ["linux", "mac"], arch := "x86_64")
let x := 1;

@musi.lang(name := "Option")
let Opt[T] := data { Some : T | None };

@meta({ a := 1, b := "x" })
let y := 2;

@meta(.Some(["a", "b"]))
let z := 3;
"#;

    let mut sources = SourceMap::default();
    let source_id = sources.add("attrs.ms", src);
    let lexed = Lexer::new(src).lex();
    let parsed = parse(source_id, &lexed);

    assert!(
        parsed.errors().is_empty(),
        "expected no errors: {:?}",
        parsed.errors()
    );
}

#[test]
fn test_attr_value_rejects_name_expr() {
    let src = r#"
@when(os := linux)
let x := 1;
"#;

    let mut sources = SourceMap::default();
    let source_id = sources.add("attrs_bad.ms", src);
    let lexed = Lexer::new(src).lex();
    let parsed = parse(source_id, &lexed);

    assert!(
        parsed
            .errors()
            .iter()
            .any(|e| matches!(e.kind, ParseErrorKind::ExpectedAttrValue { .. })),
        "expected ExpectedAttrValue error, got {:?}",
        parsed.errors()
    );
}

#[test]
fn test_attr_value_rejects_operators() {
    let src = r#"
@link(name := "c" + "m", symbol := "puts")
foreign "c" let puts (msg : String) : Int;
"#;

    let mut sources = SourceMap::default();
    let source_id = sources.add("attrs_bad2.ms", src);
    let lexed = Lexer::new(src).lex();
    let parsed = parse(source_id, &lexed);

    assert!(
        parsed
            .errors()
            .iter()
            .any(|e| matches!(e.kind, ParseErrorKind::ExpectedToken { .. })),
        "expected parse error, got {:?}",
        parsed.errors()
    );
}
