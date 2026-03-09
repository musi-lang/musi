//! Pattern parsing tests.

use music_ast::expr::Expr;
use music_ast::pat::Pat;
use music_lex::lex as lex_source;
use music_lex::token::Token;
use music_shared::{DiagnosticBag, FileId, Interner, SourceDb, Span};

use crate::parse;

fn lex(src: &str) -> (Vec<Token>, Interner, FileId) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let file_id = source_db.add("test.mu", src);
    let mut lex_diags = DiagnosticBag::new();
    let lexed = lex_source(src, file_id, &mut interner, &mut lex_diags);
    (lexed.tokens, interner, file_id)
}

/// Parse `let <pat> := 0;` and extract the pattern.
fn parse_pat_from_let(pat_src: &str) -> (Pat, DiagnosticBag) {
    let src = format!("let {pat_src} := 0;");
    let (tokens, interner, file_id) = lex(&src);
    let mut diags = DiagnosticBag::new();
    let module = parse(&tokens, file_id, &mut diags, &interner);
    assert_eq!(module.stmts.len(), 1);
    let expr = &module.arenas.exprs[module.stmts[0].expr];
    assert!(matches!(expr, Expr::Let { .. }), "expected Let");
    let Expr::Let { fields, .. } = expr else {
        return (
            Pat::Wild {
                span: Span::default(),
            },
            diags,
        );
    };
    let pat = module.arenas.pats[fields.pat].clone();
    (pat, diags)
}

#[test]
fn test_parse_wildcard_pattern() {
    let (pat, diags) = parse_pat_from_let("_");
    assert!(!diags.has_errors());
    assert!(matches!(pat, Pat::Wild { .. }));
}

#[test]
fn test_parse_bind_pattern() {
    let (pat, diags) = parse_pat_from_let("x");
    assert!(!diags.has_errors());
    assert!(matches!(pat, Pat::Bind { .. }));
}

#[test]
fn test_parse_literal_pattern() {
    let (pat, diags) = parse_pat_from_let("42");
    assert!(!diags.has_errors());
    assert!(matches!(pat, Pat::Lit { .. }));
}

#[test]
fn test_parse_tuple_pattern() {
    let (pat, diags) = parse_pat_from_let("(a, b)");
    assert!(!diags.has_errors());
    assert!(matches!(pat, Pat::Tuple { .. }), "expected Tuple");
    let Pat::Tuple { elems, .. } = pat else {
        return;
    };
    assert_eq!(elems.len(), 2);
}

#[test]
fn test_parse_array_pattern() {
    let (pat, diags) = parse_pat_from_let("[a, b, c]");
    assert!(!diags.has_errors());
    assert!(matches!(pat, Pat::Array { .. }), "expected Array");
    let Pat::Array { elems, .. } = pat else {
        return;
    };
    assert_eq!(elems.len(), 3);
}

#[test]
fn test_parse_or_pattern() {
    let (pat, diags) = parse_pat_from_let("a or b");
    assert!(!diags.has_errors());
    assert!(matches!(pat, Pat::Or { .. }));
}

#[test]
fn test_parse_variant_pattern() {
    let (pat, diags) = parse_pat_from_let(".Some(x)");
    assert!(!diags.has_errors());
    assert!(
        matches!(pat, Pat::Variant { .. }),
        "expected Variant, got {pat:?}"
    );
    let Pat::Variant { args, .. } = pat else {
        return;
    };
    assert_eq!(args.len(), 1);
}

#[test]
fn test_parse_record_pattern() {
    let (pat, diags) = parse_pat_from_let("{ x, y }");
    assert!(!diags.has_errors());
    assert!(matches!(pat, Pat::Record { .. }), "expected Record");
    let Pat::Record { fields, .. } = pat else {
        return;
    };
    assert_eq!(fields.len(), 2);
}
