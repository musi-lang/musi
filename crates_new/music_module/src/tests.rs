use crate::{ImportError, ImportErrorKind};

#[test]
fn import_error_display_includes_typed_kind() {
    let err = ImportError::new(ImportErrorKind::NotFound, "unknown module `dep`");

    assert_eq!(err.kind(), ImportErrorKind::NotFound);
    assert_eq!(err.message(), "unknown module `dep`");
    assert_eq!(err.to_string(), "module not found: unknown module `dep`");
}
