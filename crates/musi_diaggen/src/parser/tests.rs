use std::path::Path;

use super::*;

mod success {
    use super::*;

    #[test]
    fn parses_catalog_with_kind_map() {
        let catalog = parse_catalog(
            Path::new("test.def"),
            r#"
catalog demo crate DemoKind crates/demo/src/diag_catalog_gen.rs
diag Known 1000
  message "known"
map demo_error_kind crate::DemoError option
  case "Known { .. }" Known
  case "Foreign(_)" none
end
"#,
        )
        .unwrap();

        assert_eq!(catalog.owner, "demo");
        assert_eq!(catalog.entries.len(), 1);
        assert_eq!(catalog.entries[0].primary, "known");
        assert_eq!(catalog.maps.len(), 1);
        assert_eq!(catalog.maps[0].cases.len(), 2);
    }
}

mod failure {}
