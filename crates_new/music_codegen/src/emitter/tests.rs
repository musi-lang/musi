use music_basic::SourceMap;
use music_known::KnownSymbols;
use music_lex::Lexer;
use music_names::Interner;
use music_resolve::ResolveOptions;

use crate::emit_single_module;

#[test]
fn test_emit_single_module_decodes_numeric_separators() {
    let mut sources = SourceMap::new();
    let path = "entry.ms";
    let source_id = sources.add(path, "let x := 123_456; let y := 12_3.4_5e1;");
    let source = sources.get(source_id).expect("source exists");

    let lexed = Lexer::new(source.text()).lex();
    let parsed = music_parse::parse(source_id, &lexed);
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let known = KnownSymbols::new(&mut interner);
    let options = ResolveOptions {
        prelude: known.compiler_prelude().to_vec(),
        import_env: None,
    };
    let analyzed = music_check::analyze_module(parsed.tree(), &sources, &mut interner, options);
    assert!(analyzed.resolve_errors.is_empty());
    assert!(analyzed.check_errors.is_empty());

    let artifact = emit_single_module(path, &interner, &sources, &analyzed)
        .expect("emit")
        .artifact;

    let entries = artifact.constants.entries();
    assert!(entries.contains(&music_il::ConstantEntry::Int(123_456)));
    assert!(entries.contains(&music_il::ConstantEntry::Float(1234.5_f64.to_bits())));
}
