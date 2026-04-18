use std::path::Path;

use crate::parser::parse_catalog;

use super::*;

mod success {
    use super::*;

    #[test]
    fn accepts_known_map_kind() {
        let catalog = parse_catalog(
            Path::new("test.def"),
            r#"
catalog demo crate DemoKind crates/demo/src/diag_catalog_gen.rs
diag Known 1000
  message "known"
  primary "known"
map demo_error_kind crate::DemoError required
  case "Known { .. }" Known
end
"#,
        )
        .unwrap();

        validate_catalogs(&[catalog]).unwrap();
    }
}

mod failure {
    use super::*;

    #[test]
    fn rejects_unknown_map_kind() {
        let catalog = parse_catalog(
            Path::new("test.def"),
            r#"
catalog demo crate DemoKind crates/demo/src/diag_catalog_gen.rs
diag Known 1000
  message "known"
  primary "known"
map demo_error_kind crate::DemoError required
  case "Known { .. }" Missing
end
"#,
        )
        .unwrap();

        let error = validate_catalogs(&[catalog]).unwrap_err();

        assert!(error.0.contains("unknown diagnostic kind `Missing`"));
    }

    #[test]
    fn rejects_duplicate_map_case() {
        let catalog = parse_catalog(
            Path::new("test.def"),
            r#"
catalog demo crate DemoKind crates/demo/src/diag_catalog_gen.rs
diag Known 1000
  message "known"
  primary "known"
map demo_error_kind crate::DemoError required
  case "Known { .. }" Known
  case "Known { .. }" Known
end
"#,
        )
        .unwrap();

        let error = validate_catalogs(&[catalog]).unwrap_err();

        assert!(error.0.contains("duplicate case `Known { .. }`"));
    }

    #[test]
    fn rejects_suffix_category_wording() {
        let catalog = parse_catalog(
            Path::new("test.def"),
            r#"
catalog demo crate DemoKind crates/demo/src/diag_catalog_gen.rs
diag Bad 1000
  message "opcode `{opcode}` unknown"
  primary "opcode `{opcode}` unknown"
"#,
        )
        .unwrap();

        let error = validate_catalogs(&[catalog]).unwrap_err();

        assert!(error.0.contains("category after subject"));
    }

    #[test]
    fn rejects_bare_vague_wording() {
        let catalog = parse_catalog(
            Path::new("test.def"),
            r#"
catalog demo crate DemoKind crates/demo/src/diag_catalog_gen.rs
diag Bad 1000
  message "unknown field"
  primary "unknown field"
"#,
        )
        .unwrap();

        let error = validate_catalogs(&[catalog]).unwrap_err();

        assert!(error.0.contains("lacks concrete subject"));
    }

    #[test]
    fn rejects_duplicate_message_across_catalogs() {
        let first = parse_catalog(
            Path::new("first.def"),
            r#"
catalog first crate FirstKind crates/first/src/diag_catalog_gen.rs
diag Known 1000
  message "specific failure `one`"
  primary "specific failure `one`"
"#,
        )
        .unwrap();
        let second = parse_catalog(
            Path::new("second.def"),
            r#"
catalog second crate SecondKind crates/second/src/diag_catalog_gen.rs
diag Known 1001
  message "specific failure `one`"
  primary "specific failure `two`"
"#,
        )
        .unwrap();

        let error = validate_catalogs(&[first, second]).unwrap_err();

        assert!(
            error
                .0
                .contains("diagnostic message `specific failure `one``")
        );
    }
}
