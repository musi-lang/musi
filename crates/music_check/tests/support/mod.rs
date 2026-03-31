use music_basic::SourceMap;
use music_check::{SemaErrorKinds, SemaErrors};
use music_lex::Lexer;
use music_names::Interner;
use music_resolve::ResolveOptions;

pub(crate) fn analyze_text(text: &str) -> SemaErrorKinds {
    analyze_text_full(text)
        .into_iter()
        .map(|e| e.kind)
        .collect()
}

pub(crate) fn analyze_text_full(text: &str) -> SemaErrors {
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", text);

    let lexed = Lexer::new(text).lex();
    let parsed = music_parse::parse(source_id, &lexed);
    assert!(parsed.errors().is_empty(), "test inputs must parse");

    let mut interner = Interner::default();
    let analyzed = music_check::analyze_module(
        parsed.tree(),
        &sources,
        &mut interner,
        ResolveOptions::default(),
        None,
    );
    assert!(
        analyzed.resolve_errors.is_empty(),
        "test inputs must resolve, got {:#?}",
        analyzed.resolve_errors
    );
    analyzed.check_errors
}
