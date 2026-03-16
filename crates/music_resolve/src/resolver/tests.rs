use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;

use crate::error::ResolveError;
use crate::resolver::{ResolverConfig, resolve_import};
use crate::specifier::parse_specifier;

fn temp_dir() -> tempfile::TempDir {
    tempfile::tempdir().expect("create temp dir")
}

fn make_config(std_root: PathBuf, project_root: PathBuf) -> ResolverConfig {
    ResolverConfig {
        std_root,
        cache_dir: PathBuf::from("/tmp/musi-test-cache"),
        project_root,
        manifest_imports: HashMap::new(),
        manifest_deps: HashMap::new(),
    }
}

#[test]
fn test_resolve_musi_core_as_directory_index() {
    let dir = temp_dir();
    let std_root = dir.path().join("stdlib");
    let core_dir = std_root.join("core");
    fs::create_dir_all(&core_dir).unwrap();
    fs::write(core_dir.join("index.ms"), "-- core module").unwrap();

    let config = make_config(std_root, dir.path().to_path_buf());
    let spec = parse_specifier("musi:core").unwrap();
    let result = resolve_import(&spec, dir.path().join("main.ms").as_ref(), &config).unwrap();
    assert!(result.ends_with("core/index.ms"));
}

#[test]
fn test_resolve_musi_nested_path_as_file() {
    let dir = temp_dir();
    let std_root = dir.path().join("stdlib");
    let io_dir = std_root.join("io");
    fs::create_dir_all(&io_dir).unwrap();
    fs::write(io_dir.join("print.ms"), "-- print module").unwrap();

    let config = make_config(std_root, dir.path().to_path_buf());
    let spec = parse_specifier("musi:io/print").unwrap();
    let result = resolve_import(&spec, dir.path().join("main.ms").as_ref(), &config).unwrap();
    assert!(result.ends_with("io/print.ms"));
}

#[test]
fn test_resolve_relative_with_extension() {
    let dir = temp_dir();
    fs::write(dir.path().join("lib.ms"), "-- lib").unwrap();

    let config = make_config(dir.path().join("stdlib"), dir.path().to_path_buf());
    let spec = parse_specifier("./lib").unwrap();
    let importing = dir.path().join("main.ms");
    let result = resolve_import(&spec, &importing, &config).unwrap();
    assert!(result.ends_with("lib.ms"));
}

#[test]
fn test_resolve_relative_already_has_extension() {
    let dir = temp_dir();
    fs::write(dir.path().join("lib.ms"), "-- lib").unwrap();

    let config = make_config(dir.path().join("stdlib"), dir.path().to_path_buf());
    let spec = parse_specifier("./lib.ms").unwrap();
    let importing = dir.path().join("main.ms");
    let result = resolve_import(&spec, &importing, &config).unwrap();
    assert!(result.ends_with("lib.ms"));
}

#[test]
fn test_resolve_relative_missing_returns_error() {
    let dir = temp_dir();
    let config = make_config(dir.path().join("stdlib"), dir.path().to_path_buf());
    let spec = parse_specifier("./nonexistent").unwrap();
    let importing = dir.path().join("main.ms");
    let err = resolve_import(&spec, &importing, &config).unwrap_err();
    assert!(matches!(err, ResolveError::ModuleNotFound { .. }));
}

#[test]
fn test_resolve_bare_from_manifest_imports() {
    let dir = temp_dir();
    let std_root = dir.path().join("stdlib");
    let core_dir = std_root.join("core");
    fs::create_dir_all(&core_dir).unwrap();
    fs::write(core_dir.join("index.ms"), "-- core").unwrap();

    let mut config = make_config(std_root, dir.path().to_path_buf());
    let _prev = config
        .manifest_imports
        .insert("core".to_owned(), "musi:core".to_owned());

    let spec = parse_specifier("core").unwrap();
    let importing = dir.path().join("main.ms");
    let result = resolve_import(&spec, &importing, &config).unwrap();
    assert!(result.ends_with("core/index.ms"));
}

#[test]
fn test_resolve_bare_not_in_manifest_returns_error() {
    let dir = temp_dir();
    let config = make_config(dir.path().join("stdlib"), dir.path().to_path_buf());
    let spec = parse_specifier("unknown-lib").unwrap();
    let importing = dir.path().join("main.ms");
    let err = resolve_import(&spec, &importing, &config).unwrap_err();
    assert!(matches!(err, ResolveError::UnresolvedBareImport { .. }));
}

#[test]
fn test_resolve_msr_returns_error() {
    let err = parse_specifier("msr:my-package").unwrap_err();
    assert!(matches!(err, ResolveError::RegistryNotSupported { .. }));
}

#[test]
fn test_resolve_musi_not_found() {
    let dir = temp_dir();
    let std_root = dir.path().join("stdlib");
    fs::create_dir_all(&std_root).unwrap();

    let config = make_config(std_root, dir.path().to_path_buf());
    let spec = parse_specifier("musi:nonexistent").unwrap();
    let importing = dir.path().join("main.ms");
    let err = resolve_import(&spec, &importing, &config).unwrap_err();
    assert!(matches!(err, ResolveError::ModuleNotFound { .. }));
}
