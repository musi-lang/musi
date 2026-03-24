use std::fs;
use std::path::PathBuf;

use crate::ResolveErrorKind;
use crate::driver::resolve_project;
use crate::loader::ModuleLoader;

fn setup_project(files: &[(&str, &str)]) -> (tempfile::TempDir, PathBuf) {
    let dir = tempfile::tempdir().unwrap();
    for (name, content) in files {
        let path = dir.path().join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(&path, content).unwrap();
    }
    let entry = dir.path().join(files[0].0);
    (dir, entry)
}

#[test]
fn single_module_resolves() {
    let (dir, entry) = setup_project(&[("main.ms", "let x := 42")]);
    let loader = ModuleLoader::new(dir.path().to_path_buf());

    let result = resolve_project(&entry, &loader).unwrap();

    assert_eq!(result.order.len(), 1);
    assert_eq!(result.modules.len(), 1);

    let mod_id = result.order[0];
    let module = &result.modules[&mod_id];
    assert!(module.errors.is_empty(), "errors: {:?}", module.errors);
}

#[test]
fn two_module_chain() {
    let (dir, entry) = setup_project(&[
        ("main.ms", "import \"./utils\" as Utils; let x := 1"),
        ("utils.ms", "export let helper := 42"),
    ]);
    let loader = ModuleLoader::new(dir.path().to_path_buf());

    let result = resolve_project(&entry, &loader).unwrap();

    assert_eq!(result.graph.len(), 2);
    assert_eq!(result.order.len(), 2);

    // utils should come before main in topo order
    let main_pos = result
        .order
        .iter()
        .position(|id| result.graph.path(*id).ends_with("main.ms"))
        .unwrap();
    let utils_pos = result
        .order
        .iter()
        .position(|id| result.graph.path(*id).ends_with("utils.ms"))
        .unwrap();
    assert!(utils_pos < main_pos);
}

#[test]
fn exports_propagate_to_importer() {
    let (dir, entry) = setup_project(&[
        ("main.ms", "import \"./lib\" as _"),
        ("lib.ms", "export let foo := 1"),
    ]);
    let loader = ModuleLoader::new(dir.path().to_path_buf());

    let result = resolve_project(&entry, &loader).unwrap();

    // lib.ms should have exported `foo`
    let lib_id = result
        .order
        .iter()
        .find(|id| result.graph.path(**id).ends_with("lib.ms"))
        .unwrap();
    let exports = result.graph.get_exports(*lib_id).unwrap();
    assert!(
        !exports.exports.is_empty(),
        "lib.ms should export at least `foo`"
    );
}

#[test]
fn diamond_dependency() {
    let (dir, entry) = setup_project(&[
        (
            "main.ms",
            "import \"./a\" as A; import \"./b\" as B; let x := 1",
        ),
        ("a.ms", "import \"./shared\" as S; export let a := 1"),
        ("b.ms", "import \"./shared\" as S; export let b := 2"),
        ("shared.ms", "export let s := 0"),
    ]);
    let loader = ModuleLoader::new(dir.path().to_path_buf());

    let result = resolve_project(&entry, &loader).unwrap();

    assert_eq!(result.graph.len(), 4);
    assert_eq!(result.order.len(), 4);

    // shared must come before both a and b
    let shared_pos = result
        .order
        .iter()
        .position(|id| result.graph.path(*id).ends_with("shared.ms"))
        .unwrap();
    let a_pos = result
        .order
        .iter()
        .position(|id| result.graph.path(*id).ends_with("a.ms"))
        .unwrap();
    let b_pos = result
        .order
        .iter()
        .position(|id| result.graph.path(*id).ends_with("b.ms"))
        .unwrap();
    assert!(shared_pos < a_pos);
    assert!(shared_pos < b_pos);
}

#[test]
fn missing_file_returns_error() {
    let dir = tempfile::tempdir().unwrap();
    let entry = dir.path().join("nonexistent.ms");
    let loader = ModuleLoader::new(dir.path().to_path_buf());

    let result = resolve_project(&entry, &loader);
    assert!(result.is_err());
}

#[test]
fn musi_builtin_import_resolves() {
    let (dir, entry) = setup_project(&[("main.ms", "import \"musi:core\" as Core; let x := 1")]);
    let loader = ModuleLoader::new(dir.path().to_path_buf());

    let result = resolve_project(&entry, &loader).unwrap();

    // Only 1 module in graph (musi: doesn't create a file module)
    assert_eq!(result.graph.len(), 1);

    let mod_id = result.order[0];
    let module = &result.modules[&mod_id];
    let import_errors: Vec<_> = module
        .errors
        .iter()
        .filter(|e| matches!(e.kind, ResolveErrorKind::ImportNotFound(_)))
        .collect();
    assert!(
        import_errors.is_empty(),
        "musi:core should not produce ImportNotFound: {import_errors:?}"
    );
}
