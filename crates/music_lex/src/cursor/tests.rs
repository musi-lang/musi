use music_basic::Span;

use super::Cursor;

#[test]
fn test_new_starts_at_zero() {
    let cursor = Cursor::new("abc");
    assert_eq!(cursor.pos(), 0);
    assert!(!cursor.is_eof());
}

#[test]
fn test_peek_and_peek_next_follow_utf8_boundaries() {
    let cursor = Cursor::new("aé€");
    assert_eq!(cursor.peek(), Some('a'));
    assert_eq!(cursor.peek_next(), Some('é'));
}

#[test]
fn test_advance_moves_across_ascii_and_utf8() {
    let mut cursor = Cursor::new("aé€");
    assert_eq!(cursor.advance(), Some('a'));
    assert_eq!(cursor.pos(), 1);
    assert_eq!(cursor.advance(), Some('é'));
    assert_eq!(cursor.pos(), 3);
    assert_eq!(cursor.advance(), Some('€'));
    assert_eq!(cursor.pos(), 6);
    assert!(cursor.is_eof());
}

#[test]
fn test_eat_while_ascii_consumes_matching_bytes() {
    let mut cursor = Cursor::new("abc123");
    let consumed = cursor.eat_while_ascii(|byte| byte.is_ascii_alphabetic());

    assert_eq!(consumed, 3);
    assert_eq!(cursor.peek(), Some('1'));
}

#[test]
fn test_slice_and_span_from_are_consistent() {
    let mut cursor = Cursor::new("hello world");
    let start = cursor.pos();
    let _ = cursor.eat_while_ascii(|byte| byte.is_ascii_alphabetic());
    let span = cursor.span_from(start);

    assert_eq!(span, Span::new(0, 5));
    assert_eq!(cursor.slice(start), "hello");
    assert_eq!(cursor.slice_span(span), "hello");
}
