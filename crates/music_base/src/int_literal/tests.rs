use super::{
    NumericSuffixClass, parse_i64_literal, parse_u32_literal, parse_u64_literal,
    split_numeric_suffix,
};

#[test]
fn split_numeric_suffix_parses_supported_forms() {
    let (raw, suffix) = split_numeric_suffix("123_n16");
    assert_eq!(raw, "123");
    let suffix = suffix.expect("suffix");
    assert_eq!(suffix.class, NumericSuffixClass::N);
    assert_eq!(suffix.width, Some(16));

    let (raw, suffix) = split_numeric_suffix("42z64");
    assert_eq!(raw, "42");
    let suffix = suffix.expect("suffix");
    assert_eq!(suffix.class, NumericSuffixClass::Z);
    assert_eq!(suffix.width, Some(64));

    let (raw, suffix) = split_numeric_suffix("1.5_f32");
    assert_eq!(raw, "1.5");
    let suffix = suffix.expect("suffix");
    assert_eq!(suffix.class, NumericSuffixClass::F);
    assert_eq!(suffix.width, Some(32));
}

#[test]
fn split_numeric_suffix_does_not_consume_hex_fraction_digits() {
    let (raw, suffix) = split_numeric_suffix("0xff");
    assert_eq!(raw, "0xff");
    assert!(suffix.is_none());
}

#[test]
fn integer_parsers_ignore_numeric_suffixes() {
    assert_eq!(parse_i64_literal("1_024_z32"), Some(1024));
    assert_eq!(parse_u64_literal("0xff_n16"), Some(255));
    assert_eq!(parse_u32_literal("255_n8"), Some(255));
}
