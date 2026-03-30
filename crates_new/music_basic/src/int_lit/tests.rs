use super::parse_u64;

#[test]
fn test_parse_u64_dec() {
    assert_eq!(parse_u64("0"), Some(0));
    assert_eq!(parse_u64("42"), Some(42));
    assert_eq!(parse_u64("10_000"), Some(10_000));
}

#[test]
fn test_parse_u64_hex() {
    assert_eq!(parse_u64("0xFF"), Some(255));
    assert_eq!(parse_u64("0xDEAD_BEEF"), Some(0xDEAD_BEEF));
}

#[test]
fn test_parse_u64_oct() {
    assert_eq!(parse_u64("0o755"), Some(0o755));
    assert_eq!(parse_u64("0O7_55"), Some(0o755));
}

#[test]
fn test_parse_u64_bin() {
    assert_eq!(parse_u64("0b1010"), Some(10));
    assert_eq!(parse_u64("0B1010_0110"), Some(0b1010_0110));
}

#[test]
fn test_parse_u64_rejects_empty() {
    assert_eq!(parse_u64(""), None);
    assert_eq!(parse_u64("0x"), None);
    assert_eq!(parse_u64("0b"), None);
    assert_eq!(parse_u64("0o"), None);
}
