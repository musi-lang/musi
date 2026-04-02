use super::{
    StringLitErrorKind, decode_rune_lit, decode_string_lit, decode_template_head,
    decode_template_lit, decode_template_middle, decode_template_no_subst, decode_template_tail,
};

#[test]
fn decode_string_escapes() {
    let s = decode_string_lit(r#""a\nb\tc\"""#).unwrap();
    assert_eq!(s, "a\nb\tc\"");
}

#[test]
fn decode_unicode_and_hex() {
    let s = decode_string_lit(r#""\x41\u0042\u000043""#).unwrap();
    assert_eq!(s, "ABC");
}

#[test]
fn decode_template_no_subst_is_alias() {
    let a = decode_template_lit("`a\\n\\t\\x41`").unwrap();
    let b = decode_template_no_subst("`a\\n\\t\\x41`").unwrap();
    assert_eq!(a, b);
}

#[test]
fn decode_template_chunks() {
    let head = decode_template_head("`a${").unwrap();
    let middle = decode_template_middle("}b${").unwrap();
    let tail = decode_template_tail("}c`").unwrap();
    assert_eq!((head, middle, tail), ("a".into(), "b".into(), "c".into()));
}

#[test]
fn rune_must_be_one_char() {
    let v = decode_rune_lit("'a'").unwrap();
    assert_eq!(v, u32::from('a'));

    let err = decode_rune_lit("'ab'").unwrap_err();
    assert_eq!(err.kind, StringLitErrorKind::RuneCharCount);
}
