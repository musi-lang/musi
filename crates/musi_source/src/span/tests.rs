use super::*;

#[test]
fn test_new() {
    let lo = SourcePosition::new(10);
    let hi = SourcePosition::new(20);
    let range = SourceSpan::new(lo, hi);

    assert_eq!(range.lo.position, 10);
    assert_eq!(range.hi.position, 20);
}

#[test]
fn test_dummy() {
    let dummy = SourceSpan::dummy();
    assert!(dummy.is_dummy());
    assert_eq!(dummy.lo.position, 0);
    assert_eq!(dummy.hi.position, 0);
}

#[test]
fn test_is_dummy_false() {
    let range = SourceSpan::new(SourcePosition::new(0), SourcePosition::new(1));
    assert!(!range.is_dummy());
}

#[test]
fn test_contains_inclusive() {
    let range = SourceSpan::new(SourcePosition::new(10), SourcePosition::new(20));

    assert!(range.contains(SourcePosition::new(10)));
    assert!(range.contains(SourcePosition::new(15)));
    assert!(range.contains(SourcePosition::new(20)));
}

#[test]
fn test_contains_exclusive() {
    let range = SourceSpan::new(SourcePosition::new(10), SourcePosition::new(20));

    assert!(!range.contains(SourcePosition::new(9)));
    assert!(!range.contains(SourcePosition::new(21)));
}

#[test]
fn test_overlaps_true() {
    let range1 = SourceSpan::new(SourcePosition::new(10), SourcePosition::new(20));
    let range2 = SourceSpan::new(SourcePosition::new(15), SourcePosition::new(25));

    assert!(range1.overlaps(range2));
    assert!(range2.overlaps(range1));
}

#[test]
fn test_overlaps_false() {
    let range1 = SourceSpan::new(SourcePosition::new(10), SourcePosition::new(20));
    let range2 = SourceSpan::new(SourcePosition::new(21), SourcePosition::new(30));

    assert!(!range1.overlaps(range2));
    assert!(!range2.overlaps(range1));
}

#[test]
fn test_overlaps_touching() {
    let range1 = SourceSpan::new(SourcePosition::new(10), SourcePosition::new(20));
    let range2 = SourceSpan::new(SourcePosition::new(20), SourcePosition::new(30));

    assert!(range1.overlaps(range2));
    assert!(range2.overlaps(range1));
}

#[test]
fn test_overlaps_identical() {
    let range = SourceSpan::new(SourcePosition::new(10), SourcePosition::new(20));

    assert!(range.overlaps(range));
}

#[test]
fn test_equality() {
    let range1 = SourceSpan::new(SourcePosition::new(10), SourcePosition::new(20));
    let range2 = SourceSpan::new(SourcePosition::new(10), SourcePosition::new(20));
    let range3 = SourceSpan::new(SourcePosition::new(10), SourcePosition::new(21));

    assert_eq!(range1, range2);
    assert_ne!(range1, range3);
}
