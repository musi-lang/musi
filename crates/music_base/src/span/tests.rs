use super::{Span, Spanned};

#[test]
fn span_len() {
    let span = Span::new(5, 10);
    assert_eq!(span.len(), 5);
}

#[test]
fn span_is_empty() {
    assert!(Span::new(5, 5).is_empty());
    assert!(!Span::new(5, 6).is_empty());
}

#[test]
fn span_contains() {
    let span = Span::new(5, 10);
    assert!(!span.contains(4));
    assert!(span.contains(5));
    assert!(span.contains(9));
    assert!(!span.contains(10));
}

#[test]
fn merge_spans() {
    let a = Span::new(5, 10);
    let b = Span::new(1, 3);
    let merged = a.merge(b);
    assert_eq!(merged.start, 1);
    assert_eq!(merged.end, 10);
}

#[test]
fn to_span() {
    let a = Span::new(5, 10);
    let b = Span::new(20, 30);
    let combined = a.to(b);
    assert_eq!(combined.start, 5);
    assert_eq!(combined.end, 30);
}

#[test]
fn spanned_eq_includes_span() {
    let a = Spanned::new(123, Span::new(0, 1));
    let b = Spanned::new(123, Span::new(100, 200));
    assert_ne!(a, b);

    let c = Spanned::new(123, Span::new(0, 1));
    assert_eq!(a, c);
}

#[test]
fn spanned_map_preserves_span() {
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
    let span = Span::new(10, 5);
    assert_eq!(span.len(), 0);
}
