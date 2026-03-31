use music_basic::{SourceMap, Span};
use music_lex::Lexer;
use music_names::{Interner, NameSite};
use music_parse::parse;
use music_resolve::{ResolveOptions, resolve_module};

#[test]
fn test_or_pattern_binder_occurrences_record_refs() {
    let text = "let x or x := 0; x;";

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
    assert!(
        resolved.errors.is_empty(),
        "test inputs must resolve, got {:#?}",
        resolved.errors
    );

    let mut xs = text.match_indices('x').map(|(i, _)| i);
    let first = xs.next().expect("first x");
    let second = xs.next().expect("second x");

    let first_span = Span::new(first as u32, (first + 1) as u32);
    let second_span = Span::new(second as u32, (second + 1) as u32);

    let (first_binding, binding_site_span) = resolved
        .names
        .bindings
        .iter()
        .find(|(_id, b)| b.site.span == first_span)
        .map(|(id, b)| (id, b.site.span))
        .expect("binding for first x");
    assert_eq!(binding_site_span, first_span);

    let site = NameSite::new(source_id, second_span);
    let ref_binding = resolved
        .names
        .refs
        .get(&site)
        .copied()
        .expect("ref for second x");
    assert_eq!(ref_binding, first_binding);
}
