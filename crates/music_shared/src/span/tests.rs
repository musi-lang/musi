use super::*;

#[test]
fn dummy_is_zero() {
    assert_eq!(Span::DUMMY.start, 0);
    assert_eq!(Span::DUMMY.end, 0);
}

#[test]
fn new_sets_start_and_end() {
    let span = Span::new(5, 10);
    assert_eq!(span.start, 5);
    assert_eq!(span.end, 10);
}

#[test]
fn len_returns_difference() {
    assert_eq!(Span::new(3, 8).len(), 5);
}

#[test]
fn len_of_empty_is_zero() {
    assert_eq!(Span::DUMMY.len(), 0);
    assert_eq!(Span::new(4, 4).len(), 0);
}

#[test]
fn is_empty_true_for_equal_bounds() {
    assert!(Span::DUMMY.is_empty());
    assert!(Span::new(7, 7).is_empty());
}

#[test]
fn is_empty_false_for_nonempty() {
    assert!(!Span::new(0, 1).is_empty());
}

#[test]
fn contains_within_range() {
    let span = Span::new(5, 10);
    assert!(span.contains(5));
    assert!(span.contains(9));
    assert!(!span.contains(10));
    assert!(!span.contains(4));
}

#[test]
fn merge_covers_both() {
    let a = Span::new(2, 5);
    let b = Span::new(8, 12);
    let merged = a.merge(b);
    assert_eq!(merged.start, 2);
    assert_eq!(merged.end, 12);
}

#[test]
fn merge_overlapping() {
    let a = Span::new(2, 8);
    let b = Span::new(5, 12);
    let merged = a.merge(b);
    assert_eq!(merged.start, 2);
    assert_eq!(merged.end, 12);
}

#[test]
fn merge_is_commutative() {
    let a = Span::new(1, 5);
    let b = Span::new(10, 20);
    assert_eq!(a.merge(b), b.merge(a));
}

#[test]
fn to_creates_span_from_start_to_end() {
    let a = Span::new(3, 7);
    let b = Span::new(10, 15);
    let result = a.to(b);
    assert_eq!(result.start, 3);
    assert_eq!(result.end, 15);
}

#[test]
fn spanned_equality_ignores_span() {
    let a = Spanned::new(42, Span::new(0, 5));
    let b = Spanned::new(42, Span::new(10, 20));
    assert_eq!(a, b);
}

#[test]
fn spanned_inequality_on_different_kind() {
    let a = Spanned::new(1, Span::new(0, 5));
    let b = Spanned::new(2, Span::new(0, 5));
    assert_ne!(a, b);
}

#[test]
fn spanned_hash_ignores_span() {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let a = Spanned::new(42, Span::new(0, 5));
    let b = Spanned::new(42, Span::new(100, 200));

    let mut ha = DefaultHasher::new();
    a.hash(&mut ha);
    let mut hb = DefaultHasher::new();
    b.hash(&mut hb);
    assert_eq!(ha.finish(), hb.finish());
}

#[test]
fn spanned_map_transforms_kind() {
    let s = Spanned::new(10, Span::new(1, 2));
    let doubled = s.map(|x| x * 2);
    assert_eq!(doubled.kind, 20);
    assert_eq!(doubled.span, Span::new(1, 2));
}

#[test]
fn spanned_as_ref_borrows_kind() {
    let s = Spanned::new(String::from("hello"), Span::new(0, 5));
    let r = s.as_ref();
    assert_eq!(r.kind, &String::from("hello"));
    assert_eq!(r.span, Span::new(0, 5));
}

#[test]
fn spanned_dummy_uses_dummy_span() {
    let s = Spanned::dummy(99);
    assert_eq!(s.kind, 99);
    assert_eq!(s.span, Span::DUMMY);
}

#[test]
fn merge_with_dummy() {
    let span = Span::new(5, 10);
    let merged = span.merge(Span::DUMMY);
    // DUMMY is (0,0), so merge picks min start=0, max end=10
    assert_eq!(merged.start, 0);
    assert_eq!(merged.end, 10);

    let merged_rev = Span::DUMMY.merge(span);
    assert_eq!(merged_rev.start, 0);
    assert_eq!(merged_rev.end, 10);
}

#[test]
fn display_format() {
    assert_eq!(format!("{}", Span::new(5, 10)), "5..10");
}

#[test]
fn default_equals_dummy() {
    assert_eq!(Span::default(), Span::DUMMY);
}

#[cfg(not(debug_assertions))]
#[test]
fn inverted_span_len_is_zero() {
    // In release mode (no debug_assert), start > end is allowed but len() saturates to 0
    let span = Span::new(10, 5);
    assert_eq!(span.len(), 0);
}
