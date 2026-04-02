use super::decode_string_lit;

#[test]
fn decodes_basic_escapes() {
    let s = decode_string_lit(r#""a\nb\tc\"""#).unwrap();
    assert_eq!(s, "a\nb\tc\"");
}

#[test]
fn decodes_hex_and_unicode() {
    let s = decode_string_lit(r#""\x41\u0042\u000043""#).unwrap();
    assert_eq!(s, "ABC");
}
