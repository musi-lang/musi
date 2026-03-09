use super::*;
use crate::{SourceDb, Span};

#[test]
fn test_new_bag_has_no_errors() {
    let bag = DiagnosticBag::new();
    assert!(!bag.has_errors());
}

#[test]
fn test_bag_with_error_reports_has_errors() {
    let mut bag = DiagnosticBag::new();
    let mut db = SourceDb::new();
    let fid = db.add("test.mu", "hello world\n");
    let _diag = bag.error("bad thing", Span::new(0, 5), fid);
    assert!(bag.has_errors());
}

#[test]
fn test_render_simple_format() {
    let mut bag = DiagnosticBag::new();
    let mut db = SourceDb::new();
    let fid = db.add("test.mu", "hello world\n");
    let _diag = bag.error("my message", Span::new(0, 5), fid);
    let rendered = bag
        .iter()
        .next()
        .expect("one diagnostic")
        .render_simple(&db);
    assert_eq!(rendered, "test.mu:1:1: error: my message");
}

#[test]
fn test_iter_yields_in_insertion_order() {
    let mut bag = DiagnosticBag::new();
    let mut db = SourceDb::new();
    let fid = db.add("test.mu", "hello world\n");
    let _d1 = bag.error("first", Span::new(0, 1), fid);
    let _d2 = bag.warning("second", Span::new(1, 1), fid);
    let _d3 = bag.note("third", Span::new(2, 1), fid);
    let messages: Vec<&str> = bag.iter().map(|d| &*d.message).collect();
    assert_eq!(messages, vec!["first", "second", "third"]);
}

#[test]
fn test_add_secondary_label_chaining() {
    let mut bag = DiagnosticBag::new();
    let mut db = SourceDb::new();
    let fid = db.add("test.mu", "hello world\n");
    let diag = bag.error("main", Span::new(0, 5), fid);
    let _chained = diag
        .add_secondary(Span::new(6, 5), fid, "hint 1")
        .add_secondary(Span::new(0, 11), fid, "hint 2");
    assert_eq!(
        bag.iter().next().expect("one diagnostic").secondary.len(),
        2
    );
}

#[test]
fn test_bag_with_only_warnings_has_no_errors() {
    let mut bag = DiagnosticBag::new();
    let mut db = SourceDb::new();
    let fid = db.add("test.mu", "hello world\n");
    let _diag = bag.warning("just a warning", Span::new(0, 5), fid);
    assert!(!bag.has_errors());
}

#[test]
fn test_cap_limits_diagnostic_count() {
    let mut bag = DiagnosticBag::new();
    let mut db = SourceDb::new();
    let fid = db.add("test.mu", "x\n");
    for _ in 0..MAX_ERRORS + 10 {
        let _d = bag.error("e", Span::new(0, 1), fid);
    }
    let count: usize = bag.iter().count();
    assert_eq!(count, MAX_ERRORS);
    let last = bag.iter().last().expect("non-empty");
    assert!(last.message.contains("too many errors"));
}
