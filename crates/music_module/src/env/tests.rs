use super::{ImportError, ImportErrorKind};

mod success {}

mod failure {
    use super::*;

    #[test]
    fn import_error_kind_display_is_stable() {
        assert_eq!(
            ImportErrorKind::ModuleNotFound.to_string(),
            "module not found"
        );
        assert_eq!(
            ImportErrorKind::SpecifierInvalid.to_string(),
            "module specifier invalid"
        );
    }

    #[test]
    fn import_error_display_includes_kind_and_message() {
        let err = ImportError::new(ImportErrorKind::ModuleNotFound, "dep/math");
        assert_eq!(err.kind(), ImportErrorKind::ModuleNotFound);
        assert_eq!(err.message(), "dep/math");
        assert_eq!(err.to_string(), "module not found: dep/math");
    }
}
