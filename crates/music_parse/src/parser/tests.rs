//! Combinator unit tests.

use music_lex::lex as lex_source;
use music_lex::token::Token;
use music_shared::{DiagnosticBag, FileId, Interner, SourceDb};

use crate::parse;

fn lex(src: &str) -> (Vec<Token>, Interner, FileId) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let file_id = source_db.add("test.mu", src);
    let mut lex_diags = DiagnosticBag::new();
    let lexed = lex_source(src, file_id, &mut interner, &mut lex_diags);
    (lexed.tokens, interner, file_id)
}

#[test]
fn test_parse_empty_program_returns_empty_module() {
    let (tokens, mut interner, file_id) = lex("");
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &mut interner);
    assert!(module.stmts.is_empty());
    assert!(!diags.has_errors());
}

#[test]
fn test_parse_single_integer_stmt() {
    let (tokens, mut interner, file_id) = lex("42;");
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &mut interner);
    assert_eq!(module.stmts.len(), 1);
    assert!(!diags.has_errors());
}

#[test]
fn test_parse_multiple_stmts() {
    let (tokens, mut interner, file_id) = lex("1; 2; 3;");
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &mut interner);
    assert_eq!(module.stmts.len(), 3);
    assert!(!diags.has_errors());
}

#[test]
fn test_parse_missing_semicolon_emits_diagnostic() {
    let (tokens, mut interner, file_id) = lex("42");
    let mut diags = DiagnosticBag::new();
    let _module = parse(&tokens, file_id, &mut diags, &mut interner);
    assert!(diags.has_errors());
}

#[test]
fn test_parse_progress_guarantee_on_stray_token() {
    // A stray `)` at top level should not cause infinite loop
    let (tokens, mut interner, file_id) = lex(") 42;");
    let mut diags = DiagnosticBag::new();
    let _module = parse(&tokens, file_id, &mut diags, &mut interner);
    assert!(diags.has_errors());
}
