use std::env::temp_dir;
use std::fs;
use std::mem::drop;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use music_bc::Artifact;

use crate::{PackageSource, Project, ProjectError, ProjectOptions};

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
        r#"import "util"; export let answer : Int = 42;"#,
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
        r"export let base : Int = 41;",
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
        r#"import "ext"; export let answer : Int = 42;"#,
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
        r"export let ext_answer : Int = 7;",
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
    write_file(temp.path(), "index.ms", r"export let answer : Int = 42;");

    let error =
        Project::load(temp.path(), ProjectOptions::default()).expect_err("load should fail");
    assert!(matches!(error, ProjectError::MissingFrozenLockfile { .. }));
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
    write_file(temp.path(), "index.ms", r"export let answer : Int = 42;");

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
        r#"import "alias"; export let answer : Int = 42;"#,
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
        r"export let base : Int = 41;",
    );

    let artifact: Artifact = Project::load(temp.path(), ProjectOptions::default())
        .expect("project loads")
        .compile_root_entry_artifact()
        .expect("artifact compiles");

    assert!(artifact.validate().is_ok());
}
