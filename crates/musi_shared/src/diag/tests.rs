// Tests for the parent module — extracted from the inline test block.
    use super::*;
    use crate::{SourceDb, Span};

    #[test]
    fn new_bag_has_no_errors() {
        let bag = DiagnosticBag::new();
        assert!(!bag.has_errors());
    }

    #[test]
    fn bag_with_error_has_errors() {
        let mut bag = DiagnosticBag::new();
        let mut db = SourceDb::new();
        let fid = db.add("test.ms", "hello world\n");
        let _diag = bag.error("bad thing", Span::new(0, 5), fid);
        assert!(bag.has_errors());
    }

    #[test]
    fn render_simple_format() {
        let mut bag = DiagnosticBag::new();
        let mut db = SourceDb::new();
        let fid = db.add("test.ms", "hello world\n");
        let _diag = bag.error("my message", Span::new(0, 5), fid);
        let rendered = bag.iter().next().expect("one diagnostic").render_simple(&db);
        assert_eq!(rendered, "test.ms:1:1: error: my message");
    }

    #[test]
    fn iter_yields_in_order() {
        let mut bag = DiagnosticBag::new();
        let mut db = SourceDb::new();
        let fid = db.add("test.ms", "hello world\n");
        let _d1 = bag.error("first", Span::new(0, 1), fid);
        let _d2 = bag.warning("second", Span::new(1, 1), fid);
        let _d3 = bag.note("third", Span::new(2, 1), fid);
        let messages: Vec<&str> = bag.iter().map(|d| &*d.message).collect();
        assert_eq!(messages, vec!["first", "second", "third"]);
    }

    #[test]
    fn add_secondary_chaining() {
        let mut bag = DiagnosticBag::new();
        let mut db = SourceDb::new();
        let fid = db.add("test.ms", "hello world\n");
        let diag = bag.error("main", Span::new(0, 5), fid);
        let _chained = diag
            .add_secondary(Span::new(6, 5), fid, "hint 1")
            .add_secondary(Span::new(0, 11), fid, "hint 2");
        assert_eq!(bag.iter().next().expect("one diagnostic").secondary.len(), 2);
    }

    #[test]
    fn bag_with_only_warnings_has_no_errors() {
        let mut bag = DiagnosticBag::new();
        let mut db = SourceDb::new();
        let fid = db.add("test.ms", "hello world\n");
        let _diag = bag.warning("just a warning", Span::new(0, 5), fid);
        assert!(!bag.has_errors());
    }

    #[test]
    fn cap_limits_diagnostic_count() {
        let mut bag = DiagnosticBag::new();
        let mut db = SourceDb::new();
        let fid = db.add("test.ms", "x\n");
        for _ in 0..MAX_ERRORS + 10 {
            let _d = bag.error("e", Span::new(0, 1), fid);
        }
        let count: usize = bag.iter().count();
        assert_eq!(count, MAX_ERRORS);
        // Last diagnostic is the sentinel.
        let last = bag.iter().last().expect("non-empty");
        assert!(last.message.contains("too many errors"));
    }
