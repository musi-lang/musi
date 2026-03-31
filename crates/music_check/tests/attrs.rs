mod support;

use music_check::SemaErrorKind;

use support::analyze_text;

#[test]
fn test_link_args_required() {
    let kinds = analyze_text(r#"@link() foreign "c" let puts (msg : String) : Int;"#);
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::AttrArgsRequired { .. }))
    );
}

#[test]
fn test_link_name_required() {
    let kinds =
        analyze_text(r#"@link(symbol := "puts") foreign "c" let puts (msg : String) : Int;"#);
    assert!(kinds.iter().any(|k| {
        matches!(
            k,
            SemaErrorKind::AttrArgRequired { attr, name }
                if attr == "link" && name == "name"
        )
    }));
}

#[test]
fn test_link_name_requires_string() {
    let kinds = analyze_text(r#"@link(name := 1) foreign "c" let puts (msg : String) : Int;"#);
    assert!(kinds.iter().any(|k| {
        matches!(
            k,
            SemaErrorKind::AttrArgStringRequired { attr, name }
                if attr == "link" && name == "name"
        )
    }));
}

#[test]
fn test_link_positional_arg_count_invalid() {
    let kinds =
        analyze_text(r#"@link("c", "puts", "extra") foreign "c" let puts (msg : String) : Int;"#);
    assert!(kinds.iter().any(|k| {
        matches!(
            k,
            SemaErrorKind::AttrArgCountInvalid { attr, expected, .. }
                if attr == "link" && *expected == 2
        )
    }));
}

#[test]
fn test_repr_kind_required() {
    let kinds = analyze_text(r#"@repr() let P := data { x : Int; };"#);
    assert!(kinds.iter().any(|k| {
        matches!(
            k,
            SemaErrorKind::AttrArgsRequired { attr }
                if attr == "repr"
        )
    }));
}

#[test]
fn test_repr_kind_requires_string() {
    let kinds = analyze_text(r#"@repr(kind := 1) let P := data { x : Int; };"#);
    assert!(kinds.iter().any(|k| {
        matches!(
            k,
            SemaErrorKind::AttrArgStringRequired { attr, name }
                if attr == "repr" && name == "kind"
        )
    }));
}

#[test]
fn test_layout_align_requires_int() {
    let kinds = analyze_text(r#"@layout(align := "8") let P := data { x : Int; };"#);
    assert!(kinds.iter().any(|k| {
        matches!(
            k,
            SemaErrorKind::AttrArgIntRequired { attr, name }
                if attr == "layout" && name == "align"
        )
    }));
}

#[test]
fn test_when_os_requires_string() {
    let kinds = analyze_text(r#"@when(os := 1) foreign "c" let puts (msg : String) : Int;"#);
    assert!(kinds.iter().any(|k| {
        matches!(
            k,
            SemaErrorKind::AttrArgStringRequired { attr, name }
                if attr == "when" && name == "os"
        )
    }));
}

#[test]
fn test_when_duplicate_os_rejected() {
    let kinds = analyze_text(
        r#"
@when(os := "linux", os := "mac")
foreign "c" let puts (msg : String) : Int;
"#,
    );
    assert!(kinds.iter().any(|k| {
        matches!(
            k,
            SemaErrorKind::AttrDuplicateArg { attr, name }
                if attr == "when" && name == "os"
        )
    }));
}

#[test]
fn test_when_os_allows_string_array() {
    let kinds = analyze_text(
        r#"
@when(os := ["linux", "mac"])
foreign "c" let puts (msg : String) : Int;
"#,
    );
    assert!(
        !kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::AttrArgStringRequired { .. })),
        "expected no AttrArgStringRequired errors, got {kinds:?}"
    );
}

#[test]
fn test_diag_named_args_not_allowed() {
    let kinds = analyze_text(r#"@diag.allow(code := "ms4023") let x := 1;"#);
    assert!(kinds.iter().any(|k| {
        matches!(
            k,
            SemaErrorKind::AttrNamedArgsNotAllowed { attr }
                if attr == "diag.allow"
        )
    }));
}
