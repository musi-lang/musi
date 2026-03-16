use super::{SourceDb, compute_line_starts};

#[test]
fn test_add_roundtrips_source_and_name() {
    let mut db = SourceDb::new();
    let id = db.add("test.mu", "hello world");
    assert_eq!(db.source(id), "hello world");
    assert_eq!(db.name(id), "test.mu");
}

#[test]
fn test_lookup_single_line_offset_zero() {
    let mut db = SourceDb::new();
    let id = db.add("a.mu", "abc");
    assert_eq!(db.lookup(id, 0), (1, 1));
}

#[test]
fn test_lookup_multiline_positions() {
    // "abc\ndef\nghi"
    //  0123 4567 89..
    let mut db = SourceDb::new();
    let id = db.add("m.mu", "abc\ndef\nghi");
    assert_eq!(db.lookup(id, 0), (1, 1));
    assert_eq!(db.lookup(id, 2), (1, 3));
    assert_eq!(db.lookup(id, 4), (2, 1));
    assert_eq!(db.lookup(id, 6), (2, 3));
    assert_eq!(db.lookup(id, 8), (3, 1));
    assert_eq!(db.lookup(id, 10), (3, 3));
}

#[test]
fn test_lookup_at_exact_line_starts() {
    let mut db = SourceDb::new();
    let id = db.add("s.mu", "a\nb\nc\n");
    assert_eq!(db.lookup(id, 0), (1, 1));
    assert_eq!(db.lookup(id, 2), (2, 1));
    assert_eq!(db.lookup(id, 4), (3, 1));
    assert_eq!(db.lookup(id, 6), (4, 1));
}

#[test]
fn test_get_line_strips_trailing_newline() {
    let mut db = SourceDb::new();
    let id = db.add("g.mu", "first\nsecond\nthird\n");
    assert_eq!(db.get_line(id, 1), "first");
    assert_eq!(db.get_line(id, 2), "second");
    assert_eq!(db.get_line(id, 3), "third");
}

#[test]
fn test_get_line_last_without_trailing_newline() {
    let mut db = SourceDb::new();
    let id = db.add("l.mu", "alpha\nbeta");
    assert_eq!(db.get_line(id, 1), "alpha");
    assert_eq!(db.get_line(id, 2), "beta");
}

#[test]
fn test_compute_line_starts_empty_source() {
    let starts = compute_line_starts("");
    assert_eq!(starts, vec![0]);
}

#[test]
fn test_compute_line_starts_no_newline() {
    let starts = compute_line_starts("hello");
    assert_eq!(starts, vec![0]);
}

#[test]
fn test_compute_line_starts_multiple_lines() {
    let starts = compute_line_starts("ab\ncd\n");
    assert_eq!(starts, vec![0, 3, 6]);
}
