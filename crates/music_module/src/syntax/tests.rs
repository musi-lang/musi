#![allow(unused_imports)]

use music_base::SourceId;
use music_syntax::{Lexer, parse};

use super::{ImportSiteKind, collect_export_summary, collect_import_sites};

mod success {
    use super::*;

    #[test]
    fn collects_static_and_module_load_sites() {
        let src = r#"
        let IO := import "std/io";
        let dyn := import module_path;
    "#;
        let lexed = Lexer::new(src).lex();
        let parsed = parse(lexed);
        let sites = collect_import_sites(SourceId::from_raw(0), parsed.tree());
        assert_eq!(sites.len(), 2);
        assert!(matches!(sites[0].kind, ImportSiteKind::Static { .. }));
        assert!(matches!(sites[1].kind, ImportSiteKind::NonLiteral));
    }

    #[test]
    fn template_import_site_is_nonliteral() {
        let src = r"
        let IO := import `std/io`;
    ";
        let lexed = Lexer::new(src).lex();
        let parsed = parse(lexed);
        let sites = collect_import_sites(SourceId::from_raw(0), parsed.tree());
        assert_eq!(sites.len(), 1);
        assert!(matches!(sites[0].kind, ImportSiteKind::NonLiteral));
    }

    #[test]
    fn collects_tuple_static_import_sites() {
        let src = r#"
        let (StdCmp, StdWord) := import ("@std/cmp", "@std/word");
    "#;
        let lexed = Lexer::new(src).lex();
        let parsed = parse(lexed);
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
        let sites = collect_import_sites(SourceId::from_raw(0), parsed.tree());
        assert_eq!(sites.len(), 2);
        assert!(matches!(
            &sites[0].kind,
            ImportSiteKind::Static { spec } if spec.as_str() == "@std/cmp"
        ));
        assert!(matches!(
            &sites[1].kind,
            ImportSiteKind::Static { spec } if spec.as_str() == "@std/word"
        ));
    }

    #[test]
    fn import_sites_ignore_string_contents() {
        let src = r#"
        let A := import "a";
        let text := "let B := import \"b\";";
    "#;
        let lexed = Lexer::new(src).lex();
        let parsed = parse(lexed);
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
        let sites = collect_import_sites(SourceId::from_raw(0), parsed.tree());
        assert_eq!(sites.len(), 1);
        assert!(matches!(sites[0].kind, ImportSiteKind::Static { .. }));
    }

    #[test]
    fn collects_exports_and_marks_hidden() {
        let src = r"
        export let x := 1;
        export hidden let y := 2;
    ";
        let lexed = Lexer::new(src).lex();
        let parsed = parse(lexed);
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
        let parsed = parse(lexed);
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
        let parsed = parse(lexed);
        let summary = collect_export_summary(SourceId::from_raw(0), parsed.tree());
        let exports: Vec<&str> = summary.exports().collect();
        assert!(!exports.contains(&"x"));
        assert!(exports.contains(&"y"));
    }

    #[test]
    fn removed_given_exports_are_absent() {
        let src = r"
        export let Eq := shape { };
    ";
        let lexed = Lexer::new(src).lex();
        let parsed = parse(lexed);
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
        let summary = collect_export_summary(SourceId::from_raw(0), parsed.tree());
        assert_eq!(summary.exported_given_count(), 0);
        assert_eq!(summary.exported_givens().count(), 0);
    }

    #[test]
    fn hidden_export_marking_is_order_independent() {
        let src = r"
        export let x := 1;
        @foreign(abi := .c)
        export let y (msg : CString) : Int;
        export hidden let x := 2;
    ";
        let lexed = Lexer::new(src).lex();
        let parsed = parse(lexed);
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
        let summary = collect_export_summary(SourceId::from_raw(0), parsed.tree());
        let exports: Vec<&str> = summary.exports().collect();
        assert_eq!(exports.iter().filter(|name| **name == "x").count(), 1);
        assert_eq!(exports.iter().filter(|name| **name == "y").count(), 1);
        assert!(summary.is_export_opaque("x"));
        assert!(!summary.is_export_opaque("y"));
    }
}

mod failure {}
