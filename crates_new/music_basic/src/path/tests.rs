use std::path::{Path, PathBuf};

use super::{normalize_path, resolve_import_path};

#[test]
fn test_normalize_path_elides_dot_and_parent() {
    let path = Path::new("a/./b/../c");
    assert_eq!(normalize_path(path), PathBuf::from("a/c"));
}

#[test]
fn test_resolve_import_path_relativizes_and_adds_extension() {
    let from = Path::new("src/main.ms");
    assert_eq!(
        resolve_import_path(from, "./util"),
        PathBuf::from("src/util.ms")
    );
}
