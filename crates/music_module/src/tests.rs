#![allow(unused_imports)]

use crate::{ImportError, ImportErrorKind};

mod success {
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
}

mod failure {
    use super::*;

    #[test]
    fn import_error_display_includes_typed_kind() {
        let err = ImportError::new(ImportErrorKind::ModuleNotFound, "unknown module `dep`");

        assert_eq!(err.kind(), ImportErrorKind::ModuleNotFound);
        assert!(err.message().contains("unknown module"));
    }
}
