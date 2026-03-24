use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use music_found::Interner;

use crate::def::DefId;
use crate::loader::{ModuleExports, ModuleLoader};

fn setup_test_dir() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    fs::create_dir_all(dir.path().join("std/math")).unwrap();
    fs::write(dir.path().join("std/math/mod.ms"), "// math").unwrap();
    fs::write(dir.path().join("foo.ms"), "// foo").unwrap();
    fs::create_dir_all(dir.path().join("sub")).unwrap();
    fs::write(dir.path().join("sub/bar.ms"), "// bar").unwrap();
    dir
}

/// Canonicalize a path for comparison (temp dirs may have symlinks).
fn canon(p: &Path) -> PathBuf {
    p.canonicalize().unwrap()
}

#[test]
fn resolve_path_relative() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("sub/bar.ms");

    let resolved = loader.resolve_path("./bar", &current_file);
    assert!(resolved.is_some(), "expected ./bar to resolve");
    assert_eq!(resolved.unwrap(), canon(&dir.path().join("sub/bar.ms")));
}

#[test]
fn resolve_path_relative_parent() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("sub/bar.ms");

    let resolved = loader.resolve_path("../foo", &current_file);
    assert!(resolved.is_some(), "expected ../foo to resolve");
    assert_eq!(resolved.unwrap(), canon(&dir.path().join("foo.ms")));
}

#[test]
fn resolve_path_std() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve_path("@std/math", &current_file);
    assert!(resolved.is_some(), "expected @std/math to resolve");
    assert_eq!(
        resolved.unwrap(),
        canon(&dir.path().join("std/math/mod.ms"))
    );
}

#[test]
fn resolve_path_config_mapped() {
    let dir = setup_test_dir();
    let mut mappings = HashMap::new();
    let _prev = mappings.insert("mylib".into(), "./foo".into());

    let loader = ModuleLoader::new(dir.path().to_path_buf()).with_config_imports(mappings);
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve_path("mylib", &current_file);
    assert!(
        resolved.is_some(),
        "expected config-mapped 'mylib' to resolve"
    );
    assert_eq!(resolved.unwrap(), canon(&dir.path().join("foo.ms")));
}

#[test]
fn resolve_path_not_found() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve_path("./nonexistent", &current_file);
    assert!(resolved.is_none(), "expected missing file to return None");
}

#[test]
fn cache_loading_state() {
    let dir = setup_test_dir();
    let mut loader = ModuleLoader::new(dir.path().to_path_buf());
    let path = dir.path().join("foo.ms");

    assert!(!loader.is_loading(&path));
    loader.mark_loading(path.clone());
    assert!(loader.is_loading(&path));
    assert!(loader.get_cached(&path).is_none());
}

#[test]
fn cache_loaded_state() {
    let dir = setup_test_dir();
    let mut loader = ModuleLoader::new(dir.path().to_path_buf());
    let path = dir.path().join("foo.ms");

    let mut interner = Interner::new();
    let sym = interner.intern("x");

    let mut exports_map = HashMap::new();
    let def_id = DefId::from_raw(0);
    let _prev = exports_map.insert(sym, def_id);

    loader.mark_loaded(
        path.clone(),
        ModuleExports {
            exports: exports_map,
        },
    );
    assert!(!loader.is_loading(&path));

    let cached = loader.get_cached(&path);
    assert!(cached.is_some());
    assert!(cached.unwrap().exports.contains_key(&sym));
}

#[test]
fn resolve_path_from_root() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve_path("foo", &current_file);
    assert!(resolved.is_some(), "expected 'foo' to resolve from root");
    assert_eq!(resolved.unwrap(), canon(&dir.path().join("foo.ms")));
}

#[test]
fn with_std_path_override() {
    let dir = setup_test_dir();
    let alt_std = dir.path().join("alt_std");
    fs::create_dir_all(alt_std.join("io")).unwrap();
    fs::write(alt_std.join("io/mod.ms"), "// io").unwrap();

    let loader = ModuleLoader::new(dir.path().to_path_buf()).with_std_path(alt_std.clone());
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve_path("@std/io", &current_file);
    assert!(resolved.is_some(), "expected @std/io from alt path");
    assert_eq!(resolved.unwrap(), canon(&alt_std.join("io/mod.ms")));
}
