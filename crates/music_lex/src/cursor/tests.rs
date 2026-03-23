use super::*;

#[test]
fn new_starts_at_zero() {
    let c = Cursor::new("abc");
    assert_eq!(c.pos(), 0);
    assert!(!c.is_eof());
}

#[test]
fn peek_returns_first_char() {
    let c = Cursor::new("hello");
    assert_eq!(c.peek(), Some('h'));
}

#[test]
fn peek_on_empty_returns_none() {
    let c = Cursor::new("");
    assert_eq!(c.peek(), None);
    assert!(c.is_eof());
}

#[test]
fn peek_next_returns_second_char() {
    let c = Cursor::new("ab");
    assert_eq!(c.peek_next(), Some('b'));
}

#[test]
fn peek_next_returns_none_for_single_char() {
    let c = Cursor::new("a");
    assert_eq!(c.peek_next(), None);
}

#[test]
fn advance_returns_char_and_moves_pos() {
    let mut c = Cursor::new("abc");
    assert_eq!(c.advance(), Some('a'));
    assert_eq!(c.pos(), 1);
    assert_eq!(c.advance(), Some('b'));
    assert_eq!(c.pos(), 2);
    assert_eq!(c.advance(), Some('c'));
    assert_eq!(c.pos(), 3);
    assert_eq!(c.advance(), None);
    assert!(c.is_eof());
}

#[test]
fn advance_handles_multibyte_utf8() {
    let mut c = Cursor::new("aé€");
    assert_eq!(c.advance(), Some('a'));
    assert_eq!(c.pos(), 1);
    assert_eq!(c.advance(), Some('é'));
    assert_eq!(c.pos(), 3); // é is 2 bytes
    assert_eq!(c.advance(), Some('€'));
    assert_eq!(c.pos(), 6); // € is 3 bytes
    assert!(c.is_eof());
}

#[test]
fn advance_by_moves_position() {
    let mut c = Cursor::new("abcdef");
    c.advance_by(3);
    assert_eq!(c.pos(), 3);
    assert_eq!(c.peek(), Some('d'));
}

#[test]
fn eat_consumes_matching_char() {
    let mut c = Cursor::new("ab");
    assert!(c.eat('a'));
    assert_eq!(c.pos(), 1);
    assert!(!c.eat('a'));
    assert_eq!(c.pos(), 1);
}

#[test]
fn eat_while_consumes_matching_chars() {
    let mut c = Cursor::new("aaabbb");
    let consumed = c.eat_while(|ch| ch == 'a');
    assert_eq!(consumed, "aaa");
    assert_eq!(c.pos(), 3);
}

#[test]
fn eat_while_returns_empty_on_no_match() {
    let mut c = Cursor::new("bbb");
    let consumed = c.eat_while(|ch| ch == 'a');
    assert_eq!(consumed, "");
    assert_eq!(c.pos(), 0);
}

#[test]
fn remaining_returns_bytes_from_pos() {
    let mut c = Cursor::new("hello");
    let _ = c.advance();
    let _ = c.advance();
    assert_eq!(c.remaining(), b"llo");
}

#[test]
fn remaining_returns_empty_at_eof() {
    let mut c = Cursor::new("a");
    let _ = c.advance();
    assert_eq!(c.remaining(), b"");
}

#[test]
fn slice_returns_substring() {
    let mut c = Cursor::new("hello world");
    let _ = c.advance(); // h
    let _ = c.advance(); // e
    let _ = c.advance(); // l
    let _ = c.advance(); // l
    let _ = c.advance(); // o
    assert_eq!(c.slice(0), "hello");
    assert_eq!(c.slice(2), "llo");
}

#[test]
fn span_from_produces_correct_span() {
    let mut c = Cursor::new("abcdef");
    let _ = c.advance();
    let _ = c.advance();
    let start = c.pos();
    let _ = c.advance();
    let _ = c.advance();
    let span = c.span_from(start);
    assert_eq!(span, Span::new(2, 4));
}
