use std::path::Path;

use crate::model::Catalog;
use crate::parser::parse_catalog;
use crate::validate::validate_catalogs;

use super::*;

mod success {
    use super::*;

    #[test]
    fn renders_optional_kind_map() {
        let catalog = parse_demo_catalog();

        validate_catalogs(&[catalog]).unwrap();
        let rendered = render_catalog(&parse_demo_catalog()).unwrap();

        assert!(rendered.contains(
            "pub const fn demo_error_kind(source: &crate::DemoError) -> Option<DemoKind>"
        ));
        assert!(rendered.contains("crate::DemoError::Known { .. } => Some(DemoKind::Known)"));
        assert!(rendered.contains("crate::DemoError::Foreign(_) => None"));
    }

    #[test]
    fn renders_required_kind_map() {
        let catalog = parse_required_demo_catalog();

        validate_catalogs(&[catalog]).unwrap();
        let rendered = render_catalog(&parse_required_demo_catalog()).unwrap();

        assert!(
            rendered
                .contains("pub const fn demo_error_kind(source: &crate::DemoError) -> DemoKind")
        );
        assert!(rendered.contains("crate::DemoError::Known { .. } => DemoKind::Known"));
    }
}

mod failure {}

fn parse_demo_catalog() -> Catalog {
    parse_catalog(
        Path::new("test.def"),
        r#"
catalog demo crate DemoKind crates/demo/src/diag_catalog_gen.rs
diag Known 1000
  message "known"
  primary "known"
map demo_error_kind crate::DemoError option
  case "Known { .. }" Known
  case "Foreign(_)" none
end
"#,
    )
    .unwrap()
}

fn parse_required_demo_catalog() -> Catalog {
    parse_catalog(
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
    .unwrap()
}
