use music_basic::SourceMap;
use music_check::{SemaErrorKind, analyze_module};
use music_known::KnownSymbols;
use music_lex::Lexer;
use music_names::Interner;
use music_resolve::ResolveOptions;

#[test]
fn test_array_dim_out_of_range_reports_error() {
    let src = "let x : [18446744073709551616]Int := 0;";
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", src);

    let lexed = Lexer::new(src).lex();
    let parsed = music_parse::parse(source_id, &lexed);

    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    let analyzed = analyze_module(
        parsed.tree(),
        &sources,
        &mut interner,
        ResolveOptions {
            prelude: known.compiler_prelude().to_vec(),
            import_env: None,
        },
    );

    assert!(
        analyzed
            .check_errors
            .iter()
            .any(|e| matches!(&e.kind, SemaErrorKind::ArrayDimOutOfRange)),
        "expected ArrayDimOutOfRange, got {:?}",
        analyzed
            .check_errors
            .iter()
            .map(|e| &e.kind)
            .collect::<Vec<_>>()
    );
}
