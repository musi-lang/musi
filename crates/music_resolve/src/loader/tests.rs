use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

use crate::loader::{ModuleLoader, ResolvedImport};

fn setup_test_dir() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
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
fn resolve_relative() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("sub/bar.ms");

    let resolved = loader.resolve("./bar", &current_file);
    assert_eq!(
        resolved,
        Some(ResolvedImport::File(canon(&dir.path().join("sub/bar.ms"))))
    );
}

#[test]
fn resolve_relative_parent() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("sub/bar.ms");

    let resolved = loader.resolve("../foo", &current_file);
    assert_eq!(
        resolved,
        Some(ResolvedImport::File(canon(&dir.path().join("foo.ms"))))
    );
}

#[test]
fn resolve_config_mapped() {
    let dir = setup_test_dir();
    let mut mappings = HashMap::new();
    let _prev = mappings.insert("mylib".into(), "./foo".into());

    let loader = ModuleLoader::new(dir.path().to_path_buf()).with_config_imports(mappings);
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve("mylib", &current_file);
    assert_eq!(
        resolved,
        Some(ResolvedImport::File(canon(&dir.path().join("foo.ms"))))
    );
}

#[test]
fn resolve_not_found() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve("./nonexistent", &current_file);
    assert!(resolved.is_none());
}

#[test]
fn resolve_from_root() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve("foo", &current_file);
    assert_eq!(
        resolved,
        Some(ResolvedImport::File(canon(&dir.path().join("foo.ms"))))
    );
}

#[test]
fn resolve_musi_intrinsic_module() {
    let loader = ModuleLoader::new(PathBuf::from("/project"));
    let current_file = PathBuf::from("/project/main.ms");

    let resolved = loader.resolve("musi:test", &current_file);
    assert!(
        matches!(resolved, Some(ResolvedImport::File(path)) if path.ends_with("crates/music_owned/modules/test.ms"))
    );
    assert!(loader.resolve("musi:core", &current_file).is_none());
}

#[test]
fn resolve_msr_reserved() {
    let loader = ModuleLoader::new(PathBuf::from("/project"));
    let current_file = PathBuf::from("/project/main.ms");

    assert_eq!(
        loader.resolve("msr:some/pkg", &current_file),
        Some(ResolvedImport::ReservedRegistry)
    );
}

#[test]
fn resolve_config_mapped_to_git() {
    let dir = setup_test_dir();
    // Create a fake cached git checkout
    let cache_dir = dir.path().join("cache/git");
    let cached_pkg = cache_dir.join("github.com/musi-lang/std/latest");
    fs::create_dir_all(&cached_pkg).unwrap();
    fs::write(cached_pkg.join("index.ms"), "// std").unwrap();

    let mut mappings = HashMap::new();
    let _prev = mappings.insert("@std".into(), "git:github.com/musi-lang/std".into());

    let loader = ModuleLoader::new(dir.path().to_path_buf())
        .with_config_imports(mappings)
        .with_git_cache_dir(cache_dir);
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve("@std", &current_file);
    assert!(
        matches!(resolved, Some(ResolvedImport::Git(_))),
        "expected Git variant, got {resolved:?}"
    );
}

#[test]
fn std_prefix_not_special() {
    let dir = setup_test_dir();
    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve("@std/math", &current_file);
    assert!(resolved.is_none());
}

#[test]
fn resolve_config_prefix_from_root() {
    let dir = tempfile::tempdir().unwrap();
    fs::create_dir_all(dir.path().join("vendor/std/math")).unwrap();
    fs::write(dir.path().join("vendor/std/math/index.ms"), "// math").unwrap();
    fs::create_dir_all(dir.path().join("examples/numbers")).unwrap();
    fs::write(dir.path().join("examples/numbers/main.ms"), "// main").unwrap();

    let mut mappings = HashMap::new();
    let _prev = mappings.insert("@std/".into(), "./vendor/std/".into());

    let loader = ModuleLoader::new(dir.path().to_path_buf()).with_config_imports(mappings);
    let current_file = dir.path().join("examples/numbers/main.ms");

    let resolved = loader.resolve("@std/math", &current_file);
    assert_eq!(
        resolved,
        Some(ResolvedImport::File(canon(
            &dir.path().join("vendor/std/math/index.ms")
        )))
    );
}

#[test]
fn resolve_directory_root_uses_manifest_main() {
    let dir = tempfile::tempdir().unwrap();
    fs::create_dir_all(dir.path().join("pkg/src")).unwrap();
    fs::write(
        dir.path().join("pkg/musi.json"),
        r#"{ "main": "./src/app.ms" }"#,
    )
    .unwrap();
    fs::write(dir.path().join("pkg/src/app.ms"), "// app").unwrap();

    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve("./pkg", &current_file);
    assert_eq!(
        resolved,
        Some(ResolvedImport::File(canon(
            &dir.path().join("pkg/src/app.ms")
        )))
    );
}

#[test]
fn resolve_directory_subpath_uses_manifest_exports() {
    let dir = tempfile::tempdir().unwrap();
    fs::create_dir_all(dir.path().join("pkg/src")).unwrap();
    fs::write(
        dir.path().join("pkg/musi.json"),
        r#"{ "main": "./index.ms", "exports": { "./math": "./src/math.ms" } }"#,
    )
    .unwrap();
    fs::write(dir.path().join("pkg/index.ms"), "// root").unwrap();
    fs::write(dir.path().join("pkg/src/math.ms"), "// math").unwrap();

    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("main.ms");

    let resolved = loader.resolve("./pkg/math", &current_file);
    assert_eq!(
        resolved,
        Some(ResolvedImport::File(canon(
            &dir.path().join("pkg/src/math.ms")
        )))
    );
}

#[test]
fn reject_explicit_package_entry_import() {
    let dir = tempfile::tempdir().unwrap();
    fs::create_dir_all(dir.path().join("pkg")).unwrap();
    fs::write(
        dir.path().join("pkg/musi.json"),
        r#"{ "main": "./index.ms" }"#,
    )
    .unwrap();
    fs::write(dir.path().join("pkg/index.ms"), "// root").unwrap();
    fs::write(dir.path().join("main.ms"), "// main").unwrap();

    let loader = ModuleLoader::new(dir.path().to_path_buf());
    let current_file = dir.path().join("main.ms");

    assert_eq!(
        loader.resolve("./pkg", &current_file),
        Some(ResolvedImport::File(canon(
            &dir.path().join("pkg/index.ms")
        )))
    );
    assert!(loader.resolve("./pkg/index", &current_file).is_none());
    assert!(loader.resolve("./pkg/index.ms", &current_file).is_none());
}
