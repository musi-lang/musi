use std::env::temp_dir;
use std::fs;
use std::mem::drop;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use music_base::diag::DiagCode;
use music_module::ModuleKey;
use music_seam::Artifact;

use crate::{
    PackageSource, Project, ProjectError, ProjectOptions, ProjectTestTargetKind,
    ProjectTestTargetSource,
};

static NEXT_TEMP_ID: AtomicU64 = AtomicU64::new(0);

struct TempDir {
    path: PathBuf,
}

impl TempDir {
    fn new() -> Self {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("clock should be after epoch")
            .as_nanos();
        let sequence = NEXT_TEMP_ID.fetch_add(1, Ordering::Relaxed);
        let path = temp_dir().join(format!("musi-project-test-{unique}-{sequence}"));
        fs::create_dir_all(&path).expect("temp dir should be created");
        Self { path }
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TempDir {
    fn drop(&mut self) {
        drop(fs::remove_dir_all(&self.path));
    }
}

fn write_file(root: &Path, relative: &str, text: &str) {
    let path = root.join(relative);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("parent dirs should exist");
    }
    fs::write(path, text).expect("file should be written");
}

#[test]
fn compiles_root_package_with_workspace_member_dependency() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0",
  "dependencies": { "util": "*" },
  "workspace": ["packages/util"]
}"#,
    );
    write_file(
        temp.path(),
        "index.ms",
        r#"import "util"; export let answer : Int := 42;"#,
    );
    write_file(
        temp.path(),
        "packages/util/musi.json",
        r#"{
  "name": "util",
  "version": "0.1.0",
  "exports": "./index.ms"
}"#,
    );
    write_file(
        temp.path(),
        "packages/util/index.ms",
        r"export let base : Int := 41;",
    );

    let project = Project::load(temp.path(), ProjectOptions::default()).expect("project loads");
    let output = project.compile_root_entry().expect("root entry compiles");

    assert!(output.artifact.validate().is_ok());
    assert!(output.text.contains("@util@0.1.0/index.ms::base"));
    assert!(output.text.contains("@app@1.0.0/index.ms::answer"));
    assert!(project.package("util").is_some());
    assert_eq!(project.workspace().members.len(), 1);
}

#[test]
fn loads_project_from_nearest_manifest_ancestor() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0"
}"#,
    );
    write_file(temp.path(), "index.ms", "export let answer : Int := 42;");
    write_file(
        temp.path(),
        "src/main.ms",
        "export let main () : Int := 42;",
    );

    let project =
        crate::load_project_ancestor(temp.path().join("src/main.ms"), ProjectOptions::default())
            .expect("ancestor project should load");

    assert_eq!(project.root_dir(), temp.path());
}

#[test]
fn resolves_registry_dependency_and_caches_it_locally() {
    let temp = TempDir::new();
    let registry_root = temp.path().join("registry");
    let cache_root = temp.path().join("cache");

    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0",
  "dependencies": { "ext": "^1.0.0" }
}"#,
    );
    write_file(
        temp.path(),
        "index.ms",
        r#"import "ext"; export let answer : Int := 42;"#,
    );
    write_file(
        &registry_root,
        "ext/1.2.0/musi.json",
        r#"{
  "name": "ext",
  "version": "1.2.0",
  "exports": "./index.ms"
}"#,
    );
    write_file(
        &registry_root,
        "ext/1.2.0/index.ms",
        r"export let ext_answer : Int := 7;",
    );

    let project = Project::load(
        temp.path(),
        ProjectOptions {
            registry_root: Some(registry_root),
            cache_root: Some(cache_root.clone()),
            ..ProjectOptions::default()
        },
    )
    .expect("project loads");
    let output = project.compile_root_entry().expect("project compiles");

    assert!(output.artifact.validate().is_ok());
    assert!(cache_root.join("ext/1.2.0/musi.json").is_file());
    assert!(project.lockfile_needs_write());
    let ext = project
        .package("ext")
        .expect("registry package should resolve");
    assert!(matches!(ext.source, PackageSource::Registry { .. }));
}

#[test]
fn frozen_lock_requires_existing_lockfile() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0",
  "lock": { "frozen": true }
}"#,
    );
    write_file(temp.path(), "index.ms", r"export let answer : Int := 42;");

    let error =
        Project::load(temp.path(), ProjectOptions::default()).expect_err("load should fail");
    assert!(matches!(error, ProjectError::MissingFrozenLockfile { .. }));
    assert_eq!(error.diag_code(), Some(DiagCode::new(3620)));
}

#[test]
fn validation_error_carries_typed_diag_identity() {
    let error = ProjectError::ManifestValidationFailed {
        message: "name is required".into(),
    };

    assert_eq!(error.diag_code(), Some(DiagCode::new(3606)));
    assert_eq!(
        error.diag_message().as_deref(),
        Some("manifest validation failed (`name is required`)")
    );
}

#[test]
fn task_plan_is_returned_in_dependency_order() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0",
  "tasks": {
    "build": "music build",
    "lint": { "command": "music lint", "dependencies": ["build"] },
    "test": { "command": "music test", "dependencies": ["lint"] }
  }
}"#,
    );
    write_file(temp.path(), "index.ms", r"export let answer : Int := 42;");

    let project = Project::load(temp.path(), ProjectOptions::default()).expect("project loads");
    let plan = project.task_plan("test").expect("task plan should resolve");

    assert_eq!(plan.len(), 3);
    assert_eq!(plan[0].command, "music build");
    assert_eq!(plan[1].command, "music lint");
    assert_eq!(plan[2].command, "music test");
}

#[test]
fn manifest_imports_remap_specifiers_before_project_resolution() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0",
  "dependencies": { "util": "*" },
  "imports": { "alias": "util" },
  "workspace": ["packages/util"]
}"#,
    );
    write_file(
        temp.path(),
        "index.ms",
        r#"import "alias"; export let answer : Int := 42;"#,
    );
    write_file(
        temp.path(),
        "packages/util/musi.json",
        r#"{
  "name": "util",
  "version": "0.1.0",
  "exports": "./index.ms"
}"#,
    );
    write_file(
        temp.path(),
        "packages/util/index.ms",
        r"export let base : Int := 41;",
    );

    let artifact: Artifact = Project::load(temp.path(), ProjectOptions::default())
        .expect("project loads")
        .compile_root_entry_artifact()
        .expect("artifact compiles");

    assert!(artifact.validate().is_ok());
}

#[test]
fn discovers_co_located_project_test_targets() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0",
  "dependencies": { "@std": "*" },
  "workspace": ["packages/std"]
}"#,
    );
    write_file(temp.path(), "index.ms", r"export let answer : Int := 42;");
    write_file(
        temp.path(),
        "packages/std/musi.json",
        r#"{
  "name": "@std",
  "version": "0.1.0",
  "exports": "./index.ms"
}"#,
    );
    write_file(
        temp.path(),
        "packages/std/index.ms",
        r#"
export let version := "0.1.0";
"#,
    );
    write_file(
        temp.path(),
        "packages/std/testing/index.ms",
        r#"
export let pass := { passed := .True, message := "" };
export let describe (_name, _body) : Unit ~> Unit := _body();
export let it (_name, _body) : Unit ~> Unit := _body();
"#,
    );
    write_file(
        temp.path(),
        "packages/std/math/abs.test.ms",
        r"
export let test () : Unit := 0;
",
    );

    let project = Project::load(temp.path(), ProjectOptions::default()).expect("project loads");
    let tests = project.test_targets().expect("test targets should resolve");

    assert!(tests.iter().any(|test| test.package.name == "@std"));
    assert!(tests.iter().any(|test| {
        test.module_key
            .as_str()
            .contains("@@std@0.1.0/math/abs.test.ms")
    }));
}

#[test]
fn merges_synthetic_law_suites_into_project_test_targets() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0"
}"#,
    );
    write_file(
        temp.path(),
        "index.ms",
        r"
foreign let musi_true () : Bool;

export let Console := effect {
  let readln () : String;
  law total () := musi_true();
};
",
    );
    write_file(
        temp.path(),
        "laws.test.ms",
        r"
export let test () := 0;
",
    );

    let project = Project::load(temp.path(), ProjectOptions::default()).expect("project loads");
    let targets = project
        .test_targets()
        .expect("test targets should synthesize");

    assert_eq!(targets.len(), 2);
    assert_eq!(targets[0].kind, ProjectTestTargetKind::Module);
    assert_eq!(targets[1].kind, ProjectTestTargetKind::SyntheticLawSuite);
    assert_eq!(
        targets[1].module_key,
        ModuleKey::new("@app@1.0.0/index.ms::__laws")
    );
    assert_eq!(
        targets[1].source_module_key,
        ModuleKey::new("@app@1.0.0/index.ms")
    );
    assert_eq!(targets[1].export_name.as_ref(), "__laws_test");
    let ProjectTestTargetSource::SyntheticModule = &targets[1].source else {
        panic!("synthetic suite source expected");
    };
}

#[test]
fn compiles_workspace_std_package_and_test_modules() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root should resolve");
    let project = Project::load(&repo_root, ProjectOptions::default()).expect("project loads");

    let output = project
        .compile_package_entry("@std")
        .expect("@std entry compiles");
    assert!(output.artifact.validate().is_ok());

    for test in project.test_targets().expect("test targets should resolve") {
        if test.kind != ProjectTestTargetKind::Module {
            continue;
        }
        let output = project
            .compile_module(&test.module_key)
            .expect("package test module compiles");
        assert!(output.artifact.validate().is_ok());
    }
}

#[test]
fn compiles_static_reexport_chain_across_packages() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0",
  "dependencies": { "hub": "*" },
  "workspace": ["packages/hub", "packages/dep"]
}"#,
    );
    write_file(
        temp.path(),
        "index.ms",
        r#"
let Hub := import "hub";
export let answer () : Bool := Hub.Dep.equals([1, 2], [1, 2]);
"#,
    );
    write_file(
        temp.path(),
        "packages/hub/musi.json",
        r#"{
  "name": "hub",
  "version": "0.1.0",
  "main": "./index.ms",
  "exports": {
    ".": "./index.ms"
  }
}"#,
    );
    write_file(
        temp.path(),
        "packages/hub/index.ms",
        r#"
export let Dep := import "dep";
"#,
    );
    write_file(
        temp.path(),
        "packages/dep/musi.json",
        r#"{
  "name": "dep",
  "version": "0.1.0",
  "main": "./index.ms",
  "exports": {
    ".": "./index.ms"
  }
}"#,
    );
    write_file(
        temp.path(),
        "packages/dep/index.ms",
        r"
export let equals (left : Array[Int], right : Array[Int]) : Bool := left = right;
",
    );

    let project = Project::load(temp.path(), ProjectOptions::default()).expect("project loads");
    let artifact = project
        .compile_root_entry_artifact()
        .expect("root entry compiles through static reexport chain");

    assert!(artifact.validate().is_ok());
}

#[test]
fn std_root_exports_keep_static_module_targets() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root should resolve");
    let project = Project::load(&repo_root, ProjectOptions::default()).expect("project loads");
    let entry = project
        .package_entry("@std")
        .expect("@std package entry resolves");
    let mut session = project.build_session().expect("project session builds");
    let sema = session
        .check_module(&entry.module_key)
        .expect("@std sema should succeed");
    let surface = sema.surface();

    let bytes = surface
        .exported_value("Bytes")
        .expect("Bytes export should exist");
    let math = surface
        .exported_value("Math")
        .expect("Math export should exist");
    let option = surface
        .exported_value("Option")
        .expect("Option export should exist");

    assert_eq!(
        bytes.module_target.as_ref(),
        Some(&ModuleKey::new("@@std@0.1.0/bytes/index.ms"))
    );
    assert_eq!(
        math.module_target.as_ref(),
        Some(&ModuleKey::new("@@std@0.1.0/math/index.ms"))
    );
    assert_eq!(
        option.module_target.as_ref(),
        Some(&ModuleKey::new("@@std@0.1.0/option/index.ms"))
    );
}

#[test]
fn std_root_test_import_resolves_to_std_entry() {
    let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("repo root should resolve");
    let project = Project::load(&repo_root, ProjectOptions::default()).expect("project loads");
    let test_key = ModuleKey::new("@@std@0.1.0/index.test.ms");
    let mut session = project.build_session().expect("project session builds");
    let resolved = session
        .resolve_module(&test_key)
        .expect("@std index.test should resolve")
        .clone();

    assert!(
        resolved
            .imports
            .iter()
            .any(|import| import.to == ModuleKey::new("@@std@0.1.0/index.ms"))
    );
}

#[test]
fn std_root_member_alias_keeps_module_target() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        r#"{
  "name": "app",
  "version": "1.0.0",
  "dependencies": { "@std": "*" },
  "workspace": ["packages/std"]
}"#,
    );
    write_file(
        temp.path(),
        "index.ms",
        r#"
let Std := import "@std";
export let Bytes := Std.Bytes;
"#,
    );
    write_file(
        temp.path(),
        "packages/std/musi.json",
        r#"{
  "name": "@std",
  "version": "0.1.0",
  "main": "./index.ms",
  "exports": {
    ".": "./index.ms",
    "./bytes": "./bytes/index.ms"
  }
}"#,
    );
    write_file(
        temp.path(),
        "packages/std/index.ms",
        r#"
export let Bytes := import "@std/bytes";
"#,
    );
    write_file(
        temp.path(),
        "packages/std/bytes/index.ms",
        r"
export let equals (left : Array[Int], right : Array[Int]) : Bool := left = right;
",
    );

    let project = Project::load(temp.path(), ProjectOptions::default()).expect("project loads");
    let mut session = project.build_session().expect("project session builds");
    let entry = project.root_entry().expect("root entry resolves");
    let sema = session
        .check_module(&entry.module_key)
        .expect("root sema should succeed");
    let bytes = sema
        .surface()
        .exported_value("Bytes")
        .expect("Bytes export should exist");

    assert_eq!(
        bytes.module_target.as_ref(),
        Some(&ModuleKey::new("@@std@0.1.0/bytes/index.ms"))
    );
}
