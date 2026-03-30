use crate::string_lit;

#[test]
fn decode_unquoted_is_verbatim() {
    assert_eq!(string_lit::decode("x"), "x");
}

#[test]
fn decode_simple_string() {
    assert_eq!(string_lit::decode("\"hello\""), "hello");
}

#[test]
fn decode_escapes() {
    assert_eq!(
        string_lit::decode("\"a\\\\b\\n\\t\\r\\\"\""),
        "a\\b\n\t\r\""
    );
}

#[test]
fn decode_hex_escape() {
    assert_eq!(string_lit::decode("\"\\x41\""), "A");
}
