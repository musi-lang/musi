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
fn collects_static_template_import_site() {
    let src = r"
        let IO := import `std/io`;
    ";
    let lexed = Lexer::new(src).lex();
    let parsed = music_syntax::parse(lexed);
    let sites = collect_import_sites(SourceId::from_raw(0), parsed.tree());
    assert_eq!(sites.len(), 1);
    assert!(matches!(sites[0].kind, ImportSiteKind::Static { .. }));
}

#[test]
fn import_sites_ignore_quote_expr() {
    let src = r#"
        let A := import "a";
        quote { let B := import "b"; };
    "#;
    let lexed = Lexer::new(src).lex();
    let parsed = music_syntax::parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
    let sites = collect_import_sites(SourceId::from_raw(0), parsed.tree());
    assert_eq!(sites.len(), 1);
    assert!(matches!(sites[0].kind, ImportSiteKind::Static { .. }));
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

#[test]
fn export_foreign_group_collects_binding_names() {
    let src = r#"
        export foreign "c" (
          let puts (msg : CString) : Int;
          let gets (buf : CString) : Int;
        );
    "#;
    let lexed = Lexer::new(src).lex();
    let parsed = music_syntax::parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
    let summary = collect_export_summary(SourceId::from_raw(0), parsed.tree());
    let exports: Vec<&str> = summary.exports().collect();
    assert!(exports.contains(&"puts"));
    assert!(exports.contains(&"gets"));
}

#[test]
fn export_instance_is_tracked_separately() {
    let src = r"
        export instance Eq[Int] { };
    ";
    let lexed = Lexer::new(src).lex();
    let parsed = music_syntax::parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
    let summary = collect_export_summary(SourceId::from_raw(0), parsed.tree());
    assert_eq!(summary.exported_instance_count(), 1);
    assert_eq!(summary.exported_instances().count(), 1);
}

#[test]
fn opaque_export_marking_is_order_independent() {
    let src = r#"
        export let x := 1;
        export foreign "c" (
          let x (msg : CString) : Int;
          let y (msg : CString) : Int;
        );
        export opaque let x := 2;
    "#;
    let lexed = Lexer::new(src).lex();
    let parsed = music_syntax::parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
    let summary = collect_export_summary(SourceId::from_raw(0), parsed.tree());
    let exports: Vec<&str> = summary.exports().collect();
    assert_eq!(exports.iter().filter(|name| **name == "x").count(), 1);
    assert_eq!(exports.iter().filter(|name| **name == "y").count(), 1);
    assert!(summary.is_export_opaque("x"));
    assert!(!summary.is_export_opaque("y"));
}
