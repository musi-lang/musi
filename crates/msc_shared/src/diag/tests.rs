use std::fmt;

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
    for _ in 0..210 {
        let _d = bag.error("e", Span::new(0, 1), fid);
    }
    let count: usize = bag.iter().count();
    assert_eq!(count, 200);
    let last = bag.iter().last().expect("non-empty");
    assert!(last.message.contains("too many errors"));
}

struct MockError {
    severity: Severity,
    message: &'static str,
}

impl fmt::Display for MockError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.message)
    }
}

impl IntoDiagnostic for MockError {
    fn severity(&self) -> Severity {
        self.severity
    }
}

#[test]
fn test_report_error_creates_error_diagnostic() {
    let mut bag = DiagnosticBag::new();
    let mut db = SourceDb::new();
    let fid = db.add("test.mu", "hello world\n");
    let err = MockError {
        severity: Severity::Error,
        message: "something went wrong",
    };
    let _diag = bag.report(&err, Span::new(0, 5), fid);
    assert!(bag.has_errors());
    let diag = bag.iter().next().expect("one diagnostic");
    assert_eq!(&*diag.message, "something went wrong");
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn test_report_warning_creates_warning_diagnostic() {
    let mut bag = DiagnosticBag::new();
    let mut db = SourceDb::new();
    let fid = db.add("test.mu", "hello world\n");
    let warn = MockError {
        severity: Severity::Warning,
        message: "be careful",
    };
    let _diag = bag.report(&warn, Span::new(0, 5), fid);
    assert!(!bag.has_errors());
    let diag = bag.iter().next().expect("one diagnostic");
    assert_eq!(&*diag.message, "be careful");
    assert_eq!(diag.severity, Severity::Warning);
}
