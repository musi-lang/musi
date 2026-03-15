//! Integration tests for the type checker.
#![allow(clippy::tests_outside_test_module)]
//!
//! These tests verify the type checker by running the full lex → parse → analyze
//! pipeline on source code strings.

use music_lex::lex as lex_source;
use music_parse::parse;
use music_sema::{SemaResult, analyze};
use music_shared::{DiagnosticBag, Interner, Severity, SourceDb};

/// Helper to lex, parse, and analyze a source string.
fn analyze_src(src: &str) -> (SemaResult, DiagnosticBag) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let file_id = source_db.add("test.mu", src);

    // Lex
    let mut lex_diags = DiagnosticBag::new();
    let lexed = lex_source(src, file_id, &mut interner, &mut lex_diags);
    assert!(!lex_diags.has_errors(), "lex error");

    // Parse
    let mut parse_diags = DiagnosticBag::new();
    let module = parse(&lexed.tokens, file_id, &mut parse_diags, &mut interner);
    assert!(!parse_diags.has_errors(), "parse error");

    // Analyze
    let mut diags = DiagnosticBag::new();
    let result = analyze(&module, &mut interner, file_id, &mut diags);

    (result, diags)
}

#[test]
fn test_check_empty_program_produces_no_errors() {
    let (_result, diags) = analyze_src("");

    assert!(!diags.has_errors());
}

#[test]
fn test_check_integer_literal_infers_type() {
    let (result, diags) = analyze_src("42;");

    assert!(!diags.has_errors());
    assert_eq!(result.expr_types.len(), 1);
}

#[test]
fn test_check_let_binding_with_matching_annotation_no_error() {
    let (_result, diags) = analyze_src("let x : Int := 42;");

    assert!(!diags.has_errors());
}

#[test]
fn test_check_let_binding_type_mismatch_emits_error() {
    let (_result, diags) = analyze_src("let x : Bool := 42;");

    assert!(diags.has_errors());
}

#[test]
fn test_check_fn_call_correct_arity_no_error() {
    let (_result, diags) = analyze_src("let f := (x) => x;\nf(1);");

    assert!(!diags.has_errors());
}

#[test]
fn test_check_undefined_name_emits_error() {
    let (_result, diags) = analyze_src("z;");

    assert!(diags.has_errors());
}

#[test]
fn test_check_unused_variable_emits_warning_not_error() {
    let (_result, diags) = analyze_src("let x := 42;");

    assert!(!diags.has_errors());
    assert!(diags.iter().any(|d| d.severity == Severity::Warning));
}

#[test]
fn test_check_underscore_suppresses_unused_warning() {
    let (_result, diags) = analyze_src("let _x := 42;");

    assert!(!diags.has_errors());
    assert!(diags.iter().all(|d| d.severity != Severity::Warning));
}
