//! Integration tests for name resolution.
#![allow(clippy::tests_outside_test_module)]
//!
//! These tests verify the resolver by running the full lex → parse → resolve
//! pipeline on source code strings.

use music_lex::lex as lex_source;
use music_parse::parse;
use music_sema::def::DefTable;
use music_sema::resolve;
use music_sema::scope::ScopeTree;
use music_sema::well_known;
use music_shared::{DiagnosticBag, Interner, SourceDb};

/// Helper to lex, parse, and resolve a source string.
fn resolve_src(src: &str) -> (resolve::ResolveOutput, DiagnosticBag) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let file_id = source_db.add("test.mu", src);

    // Lex
    let mut lex_diags = DiagnosticBag::new();
    let lexed = lex_source(src, file_id, &mut interner, &mut lex_diags);
    assert!(!lex_diags.has_errors(), "lex error");

    // Parse
    let mut parse_diags = DiagnosticBag::new();
    let module = parse(&lexed.tokens, file_id, &mut parse_diags, &interner);
    assert!(!parse_diags.has_errors(), "parse error");

    // Setup: init scope tree and well-known types
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let module_scope = scopes.push_root();
    let _well_known =
        well_known::init_well_known(&mut interner, &mut defs, module_scope, &mut scopes);

    // Resolve
    let mut diags = DiagnosticBag::new();
    let resolved = resolve::resolve(
        &module,
        &mut interner,
        file_id,
        &mut diags,
        &mut defs,
        &mut scopes,
        module_scope,
    );

    (resolved, diags)
}

#[test]
fn test_resolve_empty_program_produces_no_defs_and_no_errors() {
    let (output, diags) = resolve_src("");

    assert!(output.pat_defs.is_empty());
    assert!(output.expr_defs.is_empty());
    assert!(!diags.has_errors());
}

#[test]
fn test_resolve_let_binding_registers_pat_def() {
    let (output, diags) = resolve_src("let x := 42;");

    assert_eq!(output.pat_defs.len(), 1);
    assert!(!diags.has_errors());
}

#[test]
fn test_resolve_name_reference_creates_expr_def() {
    let (output, diags) = resolve_src("let x := 42;\nx;");

    assert_eq!(output.expr_defs.len(), 1);
    assert!(!diags.has_errors());
}

#[test]
fn test_resolve_undefined_name_emits_error() {
    let (_output, diags) = resolve_src("z;");

    assert!(diags.has_errors());
}

#[test]
fn test_resolve_duplicate_top_level_binding_emits_error() {
    let (_output, diags) = resolve_src("let x := 1;\nlet x := 2;");

    assert!(diags.has_errors());
}

#[test]
fn test_resolve_fn_param_creates_def_in_body() {
    let (output, diags) = resolve_src("let f := (p) -> p;");

    // At least one pat_def: f
    assert!(!output.pat_defs.is_empty());
    // One expr_def: p reference in body
    assert_eq!(output.expr_defs.len(), 1);
    assert!(!diags.has_errors());
}
