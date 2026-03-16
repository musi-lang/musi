use super::Span;

#[test]
fn test_end_is_start_plus_length() {
    let s = Span::new(10, 5);
    assert_eq!(s.end(), 15);
}

#[test]
fn test_merge_adjacent_spans() {
    let a = Span::new(0, 3);
    let b = Span::new(3, 4);
    let m = a.merge(b);
    assert_eq!(m.start, 0);
    assert_eq!(m.length, 7);
}

#[test]
fn test_merge_overlapping_spans() {
    let a = Span::new(2, 6); // 2..8
    let b = Span::new(5, 4); // 5..9
    let m = a.merge(b);
    assert_eq!(m.start, 2);
    assert_eq!(m.end(), 9);
}

#[test]
fn test_merge_reversed_order_is_commutative() {
    let a = Span::new(10, 3);
    let b = Span::new(0, 5);
    assert_eq!(a.merge(b), b.merge(a));
}

#[test]
fn test_dummy_is_zero_length_at_zero() {
    assert_eq!(Span::DUMMY.start, 0);
    assert_eq!(Span::DUMMY.length, 0);
    assert_eq!(Span::DUMMY.end(), 0);
}

#[test]
fn test_contains_offset_within_span_returns_true() {
    let s = Span::new(5, 10);
    assert!(s.contains(5));
    assert!(s.contains(10));
    assert!(s.contains(14));
}

#[test]
fn test_contains_offset_outside_span_returns_false() {
    let s = Span::new(5, 10);
    assert!(!s.contains(4));
    assert!(!s.contains(15));
}

#[test]
fn test_contains_zero_length_span_contains_nothing() {
    let s = Span::new(5, 0);
    assert!(!s.contains(5));
}
