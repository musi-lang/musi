use music_base::SourceId;
use music_syntax::Lexer;

use super::{ImportSiteKind, collect_export_summary, collect_import_sites};

#[test]
fn collects_static_and_dynamic_import_sites() {
    let src = r#"
        let IO := import "std/io";
        let dyn := import module_path;
    "#;
    let lexed = Lexer::new(src).lex();
    let parsed = music_syntax::parse(lexed);
    let sites = collect_import_sites(SourceId::from_raw(0), parsed.tree());
    assert_eq!(sites.len(), 2);
    assert!(matches!(sites[0].kind, ImportSiteKind::Static { .. }));
    assert!(matches!(sites[1].kind, ImportSiteKind::Dynamic));
}

#[test]
fn collects_exports_and_marks_opaque() {
    let src = r"
        export let x := 1;
        export opaque let y := 2;
    ";
    let lexed = Lexer::new(src).lex();
    let parsed = music_syntax::parse(lexed);
    assert!(parsed.errors().is_empty());
    let summary = collect_export_summary(SourceId::from_raw(0), parsed.tree());

    let exports: Vec<&str> = summary.exports().collect();
    assert!(exports.contains(&"x"));
    assert!(exports.contains(&"y"));
    assert!(!summary.is_export_opaque("x"));
    assert!(summary.is_export_opaque("y"));
}

#[test]
fn record_pattern_without_colon_binds_field_name() {
    let src = r"
        export let {x, y} := r;
    ";
    let lexed = Lexer::new(src).lex();
    let parsed = music_syntax::parse(lexed);
    let summary = collect_export_summary(SourceId::from_raw(0), parsed.tree());
    let exports: Vec<&str> = summary.exports().collect();
    assert!(exports.contains(&"x"));
    assert!(exports.contains(&"y"));
}

#[test]
fn record_pattern_with_colon_binds_inner_pattern() {
    let src = r"
        export let {x: y} := r;
    ";
    let lexed = Lexer::new(src).lex();
    let parsed = music_syntax::parse(lexed);
    let summary = collect_export_summary(SourceId::from_raw(0), parsed.tree());
    let exports: Vec<&str> = summary.exports().collect();
    assert!(!exports.contains(&"x"));
    assert!(exports.contains(&"y"));
}
