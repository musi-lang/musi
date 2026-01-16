use super::*;
use musi_source::{SourcePosition, SourceSpan};

#[test]
fn test_diagnostic_creation() {
    let mut bag = DiagnosticBag::new();
    let span = SourceSpan::new(SourcePosition::new(0), SourcePosition::new(5));

    bag.error(2001, span, 0, "missing ';' after value binding".to_owned());
    assert_eq!(bag.len(), 1);
    assert!(!bag.is_empty());
}

#[test]
fn test_severity_strings() {
    assert_eq!(Severity::Error.as_str(), "error");
    assert_eq!(Severity::Warning.as_str(), "warning");
    assert_eq!(Severity::Note.as_str(), "note");
}

#[test]
fn test_into_result_success() {
    let bag = DiagnosticBag::new();
    let result = bag.into_result(42);
    assert!(result.is_ok());
}

#[test]
fn test_into_result_error() {
    let mut bag = DiagnosticBag::new();
    let span = SourceSpan::new(SourcePosition::new(0), SourcePosition::new(5));
    bag.error(2001, span, 0, "test error".to_owned());
    let result = bag.into_result(42);
    assert!(result.is_err());
}

#[test]
fn test_hint_addition() {
    let mut bag = DiagnosticBag::new();
    let span = SourceSpan::new(SourcePosition::new(0), SourcePosition::new(5));

    bag.error(2001, span, 0, "test error".to_owned());
    bag.add_hint(0, "add ';' here".to_owned());

    assert_eq!(bag.len(), 1);
    let mut found = false;
    for diag in bag.iter() {
        if diag.hints.len() == 1 {
            found = true;
        }
    }
    assert!(found);
}

#[test]
fn test_extend() {
    let mut bag1 = DiagnosticBag::new();
    let mut bag2 = DiagnosticBag::new();
    let span = SourceSpan::new(SourcePosition::new(0), SourcePosition::new(5));

    bag1.error(2001, span, 0, "error 1".to_owned());
    bag2.error(2002, span, 0, "error 2".to_owned());

    bag1.extend(bag2);
    assert_eq!(bag1.len(), 2);
}
