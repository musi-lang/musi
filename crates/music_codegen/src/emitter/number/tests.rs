use super::{parse_float, parse_int};

#[test]
fn test_parse_int_allows_digit_separators() {
    assert_eq!(parse_int("123_456"), Some(123_456));
}

#[test]
fn test_parse_float_allows_digit_separators() {
    let got = parse_float("123.4_5e1").unwrap_or(0.0);
    assert_eq!(got.to_bits(), 1234.5_f64.to_bits());
}
