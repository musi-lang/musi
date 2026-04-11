use crate::{ImportError, ImportErrorKind};

#[test]
fn import_error_display_includes_typed_kind() {
    let err = ImportError::new(ImportErrorKind::ModuleNotFound, "unknown module `dep`");

    assert_eq!(err.kind(), ImportErrorKind::ModuleNotFound);
    assert!(err.message().contains("unknown module"));
}
