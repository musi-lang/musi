use music_basic::SourceMap;
use music_lex::Lexer;
use music_names::Interner;
use music_parse::parse;
use music_resolve::{ResolveErrorKind, ResolveOptions, resolve_module};

fn resolve_text(text: &str) -> Vec<ResolveErrorKind> {
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", text);

    let lexed = Lexer::new(text).lex();
    let parsed = parse(source_id, &lexed);
    assert!(parsed.errors().is_empty(), "test inputs must parse");

    let mut interner = Interner::default();
    let resolved = resolve_module(
        parsed.tree(),
        &sources,
        &mut interner,
        ResolveOptions::default(),
    );
    resolved.errors.into_iter().map(|e| e.kind).collect()
}

#[test]
fn reports_undefined_binding() {
    let kinds = resolve_text("x;");
    assert!(matches!(
        &kinds[..],
        [ResolveErrorKind::UndefinedBinding { .. }]
    ));
}

#[test]
fn reports_duplicate_binding() {
    let kinds = resolve_text("let x := 1; let x := 2;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, ResolveErrorKind::DuplicateBinding { .. }))
    );
}

#[test]
fn let_binds_name_for_following_stmts() {
    let kinds = resolve_text("let x := 1; x;");
    assert!(kinds.is_empty(), "no errors expected, got {kinds:?}");
}

#[test]
fn fstring_interpolation_resolves_inner_expr() {
    let kinds = resolve_text("let x := 1; f\"x is {x}\";");
    assert!(kinds.is_empty(), "no errors expected, got {kinds:?}");
}
