#![allow(unused_imports)]

use std::env::temp_dir;
use std::fs::{self, DirEntry};
use std::mem::drop;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use music_base::diag::{DiagCode, DiagContext};
use music_module::ModuleKey;
use music_seam::Artifact;

use crate::builtin_std::STD_FILES;
use crate::diag::ProjectDiagKind;
use crate::manifest::{
    FmtGroupLayout, FmtMatchArmArrowAlignment, FmtOperatorBreak, FmtProfile, License, LicenseFile,
    PackageManifest, PublishConfig,
};
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

fn run_git(root: &Path, args: &[&str]) {
    let output = Command::new("git")
        .args(args)
        .current_dir(root)
        .output()
        .expect("git should run");
    assert!(
        output.status.success(),
        "git command should succeed: {args:?}"
    );
}

fn assert_manifest_validation_error(
    manifest: &str,
    load_note: &str,
    kind: ProjectDiagKind,
    context: &DiagContext,
) {
    let temp = TempDir::new();
    write_file(temp.path(), "musi.json", manifest);
    write_file(temp.path(), "index.ms", r"export let result : Int := 42;");

    let error = Project::load(temp.path(), ProjectOptions::default()).expect_err(load_note);

    let expected_message = kind.message_with(context);
    assert_eq!(error.diag_code(), Some(kind.code()));
    assert_eq!(
        error.diag_message().as_deref(),
        Some(expected_message.as_str())
    );
}

fn write_option_prelude_entry(root: &Path) {
    write_file(
        root,
        "index.ms",
        r"
export let result () : Option[Int] := someOf[Int](1);
",
    );
}

fn check_root_entry(temp: &TempDir) -> Result<(), String> {
    let project = Project::load(temp.path(), ProjectOptions::default())
        .map_err(|error| format!("{error:?}"))?;
    let entry = project.root_entry().map_err(|error| format!("{error:?}"))?;
    let mut session = project
        .build_session()
        .map_err(|error| format!("{error:?}"))?;
    session
        .check_module(&entry.module_key)
        .map(|_| ())
        .map_err(|error| format!("{error:?}"))
}

fn assert_builtin_std_root_compiles(manifest: &str, suite_name: &str) {
    let temp = TempDir::new();
    write_file(temp.path(), "musi.json", manifest);
    write_file(
        temp.path(),
        "index.ms",
        &format!(
            r#"
let Testing := import "@std/testing";
export let test () :=
  (
    Testing.describe("{suite_name}");
    Testing.it("adds values", Testing.toBe(1 + 2, 3));
    Testing.endDescribe()
  );
"#
        ),
    );

    let project = Project::load(temp.path(), ProjectOptions::default()).expect("project loads");
    assert!(project.package("@std").is_some());
    let output = project.compile_root_entry().expect("root entry compiles");

    assert!(output.artifact.validate().is_ok());
}

fn collect_missing_std_export_docs(root: &Path, dir: &Path, missing: &mut Vec<String>) {
    let mut entries = fs::read_dir(dir)
        .expect("std dir should be readable")
        .collect::<Result<Vec<_>, _>>()
        .expect("std dir entries should be readable");
    entries.sort_by_key(DirEntry::path);
    for entry in entries {
        let path = entry.path();
        if path.is_dir() {
            collect_missing_std_export_docs(root, &path, missing);
            continue;
        }
        if path.extension().is_none_or(|ext| ext != "ms")
            || path
                .file_name()
                .is_some_and(|name| name.to_string_lossy().ends_with(".test.ms"))
        {
            continue;
        }
        collect_missing_std_export_docs_in_file(root, &path, missing);
    }
}

fn collect_missing_std_export_docs_in_file(root: &Path, path: &Path, missing: &mut Vec<String>) {
    let text = fs::read_to_string(path).expect("std module should be readable");
    let lines = text.lines().collect::<Vec<_>>();
    for (index, line) in lines.iter().enumerate() {
        if !line.trim_start().starts_with("export ") {
            continue;
        }
        let has_doc = lines[..index]
            .iter()
            .rev()
            .find(|line| !line.trim().is_empty())
            .is_some_and(|line| {
                let trimmed = line.trim_start();
                trimmed.starts_with("---") || matches!(trimmed.as_bytes(), [b'/', b'-', b'-', ..])
            });
        if !has_doc {
            let relative = path
                .strip_prefix(root)
                .expect("std path should be under root");
            missing.push(format!("{}:{}", relative.display(), index + 1));
        }
    }
}

mod success {
    use super::*;

    #[test]
    fn builtin_std_file_paths_match_builtin_catalog() {
        let std_paths = STD_FILES.iter().map(|(path, _)| *path).collect::<Vec<_>>();
        let catalog_paths = music_builtin::all_std_package_files()
            .iter()
            .map(|file| file.path)
            .collect::<Vec<_>>();
        assert_eq!(std_paths, catalog_paths);
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
            r#"import "util"; export let result : Int := 42;"#,
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
        assert!(output.text.contains("@app@1.0.0/index.ms::result"));
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
        write_file(temp.path(), "index.ms", "export let result : Int := 42;");
        write_file(
            temp.path(),
            "src/main.ms",
            "export let main () : Int := 42;",
        );

        let project = crate::load_project_ancestor(
            temp.path().join("src/main.ms"),
            ProjectOptions::default(),
        )
        .expect("ancestor project should load");

        assert_eq!(
            project.root_dir(),
            temp.path()
                .canonicalize()
                .expect("temp path should canonicalize")
        );
    }

    #[test]
    fn resolves_registry_dependency_and_caches_it_locally() {
        let temp = TempDir::new();
        let registry_root = temp.path().join("registry");
        let global_cache_root = temp.path().join("global-cache");

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
            r#"import "ext"; export let result : Int := 42;"#,
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
            r"export let ext_result : Int := 7;",
        );

        let project = Project::load(
            temp.path(),
            ProjectOptions::new()
                .with_registry_root(registry_root)
                .with_global_cache_root(global_cache_root),
        )
        .expect("project loads");
        let output = project.compile_root_entry().expect("project compiles");

        assert!(output.artifact.validate().is_ok());
        assert!(project.global_cache_dir().join("registry").is_dir());
        assert!(
            project
                .modules_dir()
                .expect("modules dir should be enabled")
                .join("ext/1.2.0/musi.json")
                .is_file()
        );
        assert!(project.lockfile_needs_write());
        let ext = project
            .package("ext")
            .expect("registry package should resolve");
        assert!(matches!(ext.source, PackageSource::Registry { .. }));
    }

    #[test]
    fn manifest_configures_musi_modules_dir() {
        let default_manifest: PackageManifest = serde_json::from_str(
            r#"{
  "name": "app",
  "version": "1.0.0"
}"#,
        )
        .expect("default manifest should parse");
        assert_eq!(default_manifest.modules_dir(), Some("musi_modules"));

        let custom_manifest: PackageManifest = serde_json::from_str(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "musiModulesDir": "vendor/musi"
}"#,
        )
        .expect("custom manifest should parse");
        assert_eq!(custom_manifest.modules_dir(), Some("vendor/musi"));

        let disabled_manifest: PackageManifest = serde_json::from_str(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "musiModulesDir": false
}"#,
        )
        .expect("disabled manifest should parse");
        assert_eq!(disabled_manifest.modules_dir(), None);
    }

    #[test]
    fn modules_dir_false_resolves_from_global_cache_only() {
        let temp = TempDir::new();
        let registry_root = temp.path().join("registry");
        let global_cache_root = temp.path().join("global-cache");

        write_file(
            temp.path(),
            "musi.json",
            r#"{
  "name": "app",
  "version": "1.0.0",
  "musiModulesDir": false,
  "dependencies": { "ext": "1.0.0" }
}"#,
        );
        write_file(temp.path(), "index.ms", r#"import "ext";"#);
        write_file(
            &registry_root,
            "ext/1.0.0/musi.json",
            r#"{
  "name": "ext",
  "version": "1.0.0",
  "exports": "./index.ms"
}"#,
        );
        write_file(&registry_root, "ext/1.0.0/index.ms", "");

        let project = Project::load(
            temp.path(),
            ProjectOptions::new()
                .with_registry_root(registry_root)
                .with_global_cache_root(global_cache_root),
        )
        .expect("project loads");

        assert_eq!(project.modules_dir(), None);
        assert!(!temp.path().join("musi_modules").exists());
        let ext = project.package("ext").expect("dependency should resolve");
        assert!(ext.root_dir.starts_with(project.global_cache_dir()));
    }

    #[test]
    fn resolves_git_dependency_through_global_cache_and_modules_dir() {
        let temp = TempDir::new();
        let git_root = temp.path().join("git-ext");
        let global_cache_root = temp.path().join("global-cache");
        fs::create_dir_all(&git_root).expect("git package dir should exist");
        write_file(
            &git_root,
            "musi.json",
            r#"{
  "name": "ext",
  "version": "1.0.0",
  "exports": "./index.ms"
}"#,
        );
        write_file(&git_root, "index.ms", r"export let ext_result : Int := 7;");
        run_git(&git_root, &["init", "--initial-branch=main"]);
        run_git(&git_root, &["add", "."]);
        run_git(
            &git_root,
            &[
                "-c",
                "user.name=Musi Test",
                "-c",
                "user.email=musi@example.invalid",
                "commit",
                "-m",
                "initial",
            ],
        );

        let git_url = format!("git+file://{}#main", git_root.display());
        write_file(
            temp.path(),
            "musi.json",
            &format!(
                r#"{{
  "name": "app",
  "version": "1.0.0",
  "dependencies": {{ "ext": "{git_url}" }}
}}"#
            ),
        );
        write_file(temp.path(), "index.ms", r#"import "ext";"#);

        let project = Project::load(
            temp.path(),
            ProjectOptions::new().with_global_cache_root(global_cache_root),
        )
        .expect("project loads");
        let ext = project.package("ext").expect("git package should resolve");

        assert!(matches!(ext.source, PackageSource::Git { .. }));
        assert!(project.global_cache_dir().join("git").is_dir());
        assert!(
            ext.root_dir.starts_with(
                project
                    .modules_dir()
                    .expect("modules dir should be enabled")
            )
        );
        assert!(project.lockfile().packages.iter().any(|package| {
            matches!(
                &package.source,
                crate::LockedPackageSource::Git {
                    url,
                    reference,
                    commit
                } if url == &format!("file://{}", git_root.display())
                    && reference == "main"
                    && !commit.is_empty()
            )
        }));
    }

    #[test]
    fn manifest_accepts_single_license_field_shapes() {
        let spdx: PackageManifest = serde_json::from_str(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "license": "MIT"
}"#,
        )
        .expect("SPDX license manifest should parse");
        assert_eq!(spdx.license, Some(License::Spdx("MIT".into())));

        let file: PackageManifest = serde_json::from_str(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "license": { "file": "LICENSE" }
}"#,
        )
        .expect("license file manifest should parse");
        assert_eq!(
            file.license,
            Some(License::File(LicenseFile {
                file: "LICENSE".into()
            }))
        );
    }

    #[test]
    fn manifest_accepts_publish_false_and_object() {
        let disabled: PackageManifest = serde_json::from_str(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "publish": false
}"#,
        )
        .expect("publish false should parse");
        assert_eq!(disabled.publish, Some(PublishConfig::Disabled(false)));

        let settings: PackageManifest = serde_json::from_str(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "publish": {
    "registry": "main",
    "include": ["src/**"],
    "exclude": ["target/**"]
  }
}"#,
        )
        .expect("publish settings should parse");
        let Some(PublishConfig::Settings(settings)) = settings.publish else {
            panic!("publish settings should deserialize as object");
        };
        assert_eq!(settings.registry.as_deref(), Some("main"));
        assert_eq!(settings.include, ["src/**"]);
        assert_eq!(settings.exclude, ["target/**"]);
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
        write_file(temp.path(), "index.ms", r"export let result : Int := 42;");

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
            r#"import "alias"; export let result : Int := 42;"#,
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
    fn manifest_imports_resolve_relative_targets_from_package_root() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            r##"{
  "name": "app",
  "version": "1.0.0",
  "entry": "./features/root.ms",
  "imports": { "#internal": "./internal/index.ms" }
}"##,
        );
        write_file(
            temp.path(),
            "features/root.ms",
            r##"let Internal := import "#internal";
export let result : Int := Internal.value;
"##,
        );
        write_file(
            temp.path(),
            "internal/index.ms",
            r"export let value : Int := 42;",
        );

        let output = Project::load(temp.path(), ProjectOptions::default())
            .expect("project loads")
            .compile_root_entry()
            .expect("module compiles");

        assert!(output.artifact.validate().is_ok());
    }

    #[test]
    fn discovers_nested_project_test_targets() {
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
        write_file(temp.path(), "index.ms", r"export let result : Int := 42;");
        write_file(
            temp.path(),
            "packages/std/musi.json",
            r#"{
  "name": "@std",
  "version": "0.1.0",
  "entry": "./std.ms",
  "exports": "./std.ms"
}"#,
        );
        write_file(
            temp.path(),
            "packages/std/std.ms",
            r#"
export let version := "0.1.0";
"#,
        );
        write_file(
            temp.path(),
            "packages/std/testing.ms",
            r#"
export let pass := { passed := .True, message := "" };
export let describe (_name, _body) : Unit ~> Unit := _body();
export let it (_name, _body) : Unit ~> Unit := _body();
"#,
        );
        write_file(
            temp.path(),
            "packages/std/__tests__/math.test.ms",
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
                .contains("@@std@0.1.0/__tests__/math.test.ms")
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
native let musi_true () : Bool;

export let Console := effect {
  let readLine () : String;
  law total () := unsafe { musi_true(); };
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
        let app_targets = targets
            .iter()
            .filter(|target| target.package.name == "app")
            .collect::<Vec<_>>();

        assert_eq!(app_targets.len(), 2);
        assert_eq!(app_targets[0].kind, ProjectTestTargetKind::Module);
        assert_eq!(
            app_targets[1].kind,
            ProjectTestTargetKind::SyntheticLawSuite
        );
        assert_eq!(
            app_targets[1].module_key,
            ModuleKey::new("@app@1.0.0/index.ms::__laws")
        );
        assert_eq!(
            app_targets[1].source_module_key,
            ModuleKey::new("@app@1.0.0/index.ms")
        );
        assert_eq!(app_targets[1].export_name.as_ref(), "musiLawsTest");
        let ProjectTestTargetSource::SyntheticModule = &app_targets[1].source else {
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
    fn std_public_exports_have_doc_comments() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root should resolve");
        let std_root = repo_root.join("packages/std");
        let mut missing = Vec::<String>::new();
        collect_missing_std_export_docs(&std_root, &std_root, &mut missing);

        assert!(
            missing.is_empty(),
            "std exports missing doc comments:\n{}",
            missing.join("\n")
        );
    }

    #[test]
    fn std_manifest_uses_restructured_public_paths() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root should resolve");
        let manifest = fs::read_to_string(repo_root.join("packages/std/musi.json"))
            .expect("std manifest should be readable");

        for legacy in [
            "\"./array\"",
            "\"./list\"",
            "\"./slice\"",
            "\"./iter\"",
            "\"./time\"",
            "\"./io/prompt\"",
            "\"./sys\"",
        ] {
            assert!(
                !manifest.contains(legacy),
                "legacy std export remains: {legacy}"
            );
        }
        for current in [
            "\"./collections/array\"",
            "\"./collections/list\"",
            "\"./collections/slice\"",
            "\"./collections/iter\"",
            "\"./datetime\"",
            "\"./encoding\"",
            "\"./cli/prompt\"",
            "\"./crypto\"",
            "\"./uuid\"",
            "\"./semver\"",
        ] {
            assert!(manifest.contains(current), "std export missing: {current}");
        }
    }

    #[test]
    fn std_sys_is_private_implementation_module() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            r#"{
  "name": "app",
  "version": "1.0.0",
  "dependencies": { "@std": "*" }
}"#,
        );
        write_file(
            temp.path(),
            "index.ms",
            r#"let Sys := import "@std/sys";
export let result : Int := 1;
"#,
        );

        let error = Project::load(temp.path(), ProjectOptions::default())
            .expect_err("@std/sys should not resolve as public export");

        assert_eq!(
            error.diag_code(),
            Some(ProjectDiagKind::SourceImportUnresolved.code())
        );
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
export let result () : Bool := Hub.Dep.equals([1, 2], [1, 2]);
"#,
        );
        write_file(
            temp.path(),
            "packages/hub/musi.json",
            r#"{
  "name": "hub",
  "version": "0.1.0",
  "entry": "./index.ms",
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
  "entry": "./index.ms",
  "exports": {
    ".": "./index.ms"
  }
}"#,
        );
        write_file(
            temp.path(),
            "packages/dep/index.ms",
            r"
export let equals (left : []Int, right : []Int) : Bool := left = right;
",
        );

        let project = Project::load(temp.path(), ProjectOptions::default()).expect("project loads");
        let artifact = project
            .compile_root_entry_artifact()
            .expect("root entry compiles through static reexport chain");

        assert!(artifact.validate().is_ok());
    }

    #[test]
    fn std_root_exports_keep_static_import_record_targets() {
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
            .exported_value("bytes")
            .expect("bytes export should exist");
        let encoding = surface
            .exported_value("encoding")
            .expect("encoding export should exist");
        let math = surface
            .exported_value("math")
            .expect("math export should exist");
        let option = surface
            .exported_value("option")
            .expect("option export should exist");

        assert_eq!(
            bytes.import_record_target.as_ref(),
            Some(&ModuleKey::new("@@std@0.1.0/bytes.ms"))
        );
        assert_eq!(
            encoding.import_record_target.as_ref(),
            Some(&ModuleKey::new("@@std@0.1.0/encoding.ms"))
        );
        assert_eq!(
            math.import_record_target.as_ref(),
            Some(&ModuleKey::new("@@std@0.1.0/math.ms"))
        );
        assert_eq!(
            option.import_record_target.as_ref(),
            Some(&ModuleKey::new("@@std@0.1.0/option.ms"))
        );
    }

    #[test]
    fn std_root_test_import_resolves_to_std_entry() {
        let repo_root = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .expect("repo root should resolve");
        let project = Project::load(&repo_root, ProjectOptions::default()).expect("project loads");
        let test_key = ModuleKey::new("@@std@0.1.0/__tests__/std.test.ms");
        let mut session = project.build_session().expect("project session builds");
        let resolved = session
            .resolve_module(&test_key)
            .expect("@std __tests__/std.test should resolve")
            .clone();

        assert!(
            resolved
                .imports
                .iter()
                .any(|import| import.to == ModuleKey::new("@@std@0.1.0/std.ms"))
        );
    }

    #[test]
    fn std_root_member_alias_keeps_import_record_target() {
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
export let bytes := Std.bytes;
"#,
        );
        write_file(
            temp.path(),
            "packages/std/musi.json",
            r#"{
  "name": "@std",
  "version": "0.1.0",
  "entry": "./std.ms",
  "exports": {
    ".": "./std.ms",
    "./bytes": "./bytes.ms"
  }
}"#,
        );
        write_file(
            temp.path(),
            "packages/std/std.ms",
            r#"
export let bytes := import "@std/bytes";
"#,
        );
        write_file(
            temp.path(),
            "packages/std/bytes.ms",
            r"
export let equals (left : []Int, right : []Int) : Bool := left = right;
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
            .exported_value("bytes")
            .expect("bytes export should exist");

        assert_eq!(
            bytes.import_record_target.as_ref(),
            Some(&ModuleKey::new("@@std@0.1.0/bytes.ms"))
        );
    }

    #[test]
    fn root_package_gets_auto_std_prelude() {
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
        write_option_prelude_entry(temp.path());
        write_file(
            temp.path(),
            "packages/std/musi.json",
            r#"{
  "name": "@std",
  "version": "0.1.0",
  "entry": "./std.ms",
  "exports": {
    ".": "./std.ms",
    "./prelude": "./prelude.ms",
    "./option": "./option.ms"
  }
}"#,
        );
        write_file(
            temp.path(),
            "packages/std/std.ms",
            r#"
export let Prelude := import "@std/prelude";
export let Option := import "@std/option";
"#,
        );
        write_file(
            temp.path(),
            "packages/std/prelude.ms",
            r#"
let OptionPkg := import "@std/option";
export let Int := Int;
export opaque let Option := OptionPkg.Option;
export let someOf := OptionPkg.someOf;
export let none := OptionPkg.none;
"#,
        );
        write_file(
            temp.path(),
            "packages/std/option.ms",
            r"
export opaque let Option[T] := data {
  | Some(T)
  | None
};
export let someOf[T] (value : T) : Option[T] := .Some(value);
export let none[T] () : Option[T] := .None;
",
        );

        check_root_entry(&temp).expect("root module should typecheck with auto prelude");
    }

    #[test]
    fn explicit_std_dependency_uses_builtin_std() {
        assert_builtin_std_root_compiles(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "dependencies": {
    "@std": "*"
  }
}"#,
            "explicit std",
        );
    }

    #[test]
    fn empty_lib_disables_builtin_std() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            r#"{
  "name": "app",
  "version": "1.0.0",
  "lib": []
}"#,
        );
        write_file(
            temp.path(),
            "index.ms",
            r#"let Testing := import "@std/testing";
export let test () := Testing.it("adds values", Testing.toBe(1 + 2, 3));
"#,
        );

        let error = Project::load(temp.path(), ProjectOptions::default())
            .expect_err("std should be disabled");

        assert_eq!(error.diag_code(), Some(DiagCode::new(5044)));
        let context = DiagContext::new().with("spec", "@std/testing");
        assert_eq!(
            error.diag_message().as_deref(),
            Some(
                ProjectDiagKind::SourceImportUnresolved
                    .message_with(&context)
                    .as_str()
            )
        );
    }

    #[test]
    fn empty_lib_disables_auto_std_prelude() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            r#"{
  "name": "app",
  "version": "1.0.0",
  "lib": []
}"#,
        );
        write_option_prelude_entry(temp.path());

        let _error = check_root_entry(&temp).expect_err("std prelude should be disabled");
    }
}

mod failure {
    use super::*;

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
        write_file(temp.path(), "index.ms", r"export let result : Int := 42;");

        let error =
            Project::load(temp.path(), ProjectOptions::default()).expect_err("load should fail");
        assert!(matches!(error, ProjectError::MissingFrozenLockfile { .. }));
        assert_eq!(
            error.diag_code(),
            Some(ProjectDiagKind::MissingFrozenLockfile.code())
        );
    }

    #[test]
    fn validation_error_carries_typed_diag_identity() {
        let validation_message = ProjectDiagKind::ManifestPackageNameMissing.message();
        let error = ProjectError::ManifestValidationFailed {
            message: validation_message.into(),
        };

        assert_eq!(
            error.diag_code(),
            Some(ProjectDiagKind::ManifestValidationFailed.code())
        );
        let context = DiagContext::new().with("message", validation_message);
        assert_eq!(
            error.diag_message().as_deref(),
            Some(
                ProjectDiagKind::ManifestValidationFailed
                    .message_with(&context)
                    .as_str()
            )
        );
    }

    #[test]
    fn manifest_rejects_legacy_private_field() {
        let error = serde_json::from_str::<PackageManifest>(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "private": true
}"#,
        )
        .expect_err("private should not be accepted");

        assert!(error.to_string().contains("unknown field `private`"));
    }

    #[test]
    fn manifest_rejects_invalid_fmt_enum_value() {
        let error = serde_json::from_str::<PackageManifest>(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "fmt": {
    "trailingCommas": "sometimes"
  }
}"#,
        )
        .expect_err("invalid formatter enum should not parse");

        assert!(error.to_string().contains("unknown variant `sometimes`"));

        let error = serde_json::from_str::<PackageManifest>(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "fmt": {
    "matchArmIndent": "deep"
  }
}"#,
        )
        .expect_err("invalid formatter match indent enum should not parse");

        assert!(error.to_string().contains("unknown variant `deep`"));
    }

    #[test]
    fn manifest_accepts_advanced_fmt_config() {
        let manifest = serde_json::from_str::<PackageManifest>(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "fmt": {
    "profile": "expanded",
    "matchArmArrowAlignment": "block",
    "callArgumentLayout": "block",
    "declarationParameterLayout": "block",
    "recordFieldLayout": "block",
    "effectMemberParameterLayout": "block",
    "operatorBreak": "after"
  }
}"#,
        )
        .expect("advanced formatter config should parse");

        let config = manifest.fmt.expect("fmt config should exist");
        assert_eq!(config.profile, Some(FmtProfile::Expanded));
        assert_eq!(
            config.match_arm_arrow_alignment,
            Some(FmtMatchArmArrowAlignment::Block)
        );
        assert_eq!(config.call_argument_layout, Some(FmtGroupLayout::Block));
        assert_eq!(config.operator_break, Some(FmtOperatorBreak::After));
    }

    #[test]
    fn fmt_line_width_must_be_positive() {
        assert_manifest_validation_error(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "fmt": {
    "lineWidth": 0
  }
}"#,
            "load should fail",
            ProjectDiagKind::ManifestFmtLineWidthInvalid,
            &DiagContext::new().with("value", 0),
        );
    }

    #[test]
    fn fmt_indent_width_must_be_positive() {
        assert_manifest_validation_error(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "fmt": {
    "indentWidth": 0
  }
}"#,
            "load should fail",
            ProjectDiagKind::ManifestFmtIndentWidthInvalid,
            &DiagContext::new().with("value", 0),
        );
    }

    #[test]
    fn fmt_include_and_exclude_patterns_must_be_unique() {
        assert_manifest_validation_error(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "fmt": {
    "include": ["src/**", "src/**"]
  }
}"#,
            "load should fail",
            ProjectDiagKind::ManifestFmtIncludeDuplicate,
            &DiagContext::new().with("pattern", "src/**"),
        );
        assert_manifest_validation_error(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "fmt": {
    "exclude": ["target/**", "target/**"]
  }
}"#,
            "load should fail",
            ProjectDiagKind::ManifestFmtExcludeDuplicate,
            &DiagContext::new().with("pattern", "target/**"),
        );
    }

    #[test]
    fn publish_true_is_invalid() {
        assert_manifest_validation_error(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "publish": true
}"#,
            "load should fail",
            ProjectDiagKind::ManifestPublishUnsupported,
            &DiagContext::new().with("value", true),
        );
    }

    #[test]
    fn unresolved_static_import_carries_source_diag() {
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
            "let Missing := import \"missing\";\nexport let result : Int := 42;\n",
        );

        let error =
            Project::load(temp.path(), ProjectOptions::default()).expect_err("load should fail");
        assert_eq!(
            error.diag_code(),
            Some(ProjectDiagKind::SourceImportUnresolved.code())
        );
        let context = DiagContext::new().with("spec", "missing");
        assert_eq!(
            error.diag_message().as_deref(),
            Some(
                ProjectDiagKind::SourceImportUnresolved
                    .message_with(&context)
                    .as_str()
            )
        );
        let diag = error.source_diag().expect("source diagnostic expected");
        assert!(diag.path().ends_with("index.ms"));
        assert_eq!(
            diag.diag().labels()[0].message(),
            ProjectDiagKind::SourceImportUnresolved
                .label_with(&context)
                .as_str()
        );
        assert_eq!(
            diag.diag().hint(),
            ProjectDiagKind::SourceImportUnresolved.hint()
        );
    }

    #[test]
    fn missing_package_entry_uses_unknown_package_code() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            r#"{
  "name": "app",
  "version": "1.0.0"
}"#,
        );
        write_file(temp.path(), "index.ms", "export let result : Int := 42;\n");

        let project = Project::load(temp.path(), ProjectOptions::default()).expect("project loads");
        let error = project
            .package_entry("missing")
            .expect_err("package should be missing");
        assert_eq!(
            error.diag_code(),
            Some(ProjectDiagKind::UnknownPackage.code())
        );
        let context = DiagContext::new().with("name", "missing");
        assert_eq!(
            error.diag_message().as_deref(),
            Some(
                ProjectDiagKind::UnknownPackage
                    .message_with(&context)
                    .as_str()
            )
        );
    }

    #[test]
    fn missing_lib_defaults_to_builtin_std() {
        assert_builtin_std_root_compiles(
            r#"{
  "name": "app",
  "version": "1.0.0"
}"#,
            "default std",
        );
    }

    #[test]
    fn unknown_lib_fails_manifest_validation() {
        assert_manifest_validation_error(
            r#"{
  "name": "app",
  "version": "1.0.0",
  "lib": ["!std"]
}"#,
            "lib should be invalid",
            ProjectDiagKind::ManifestLibUnknown,
            &DiagContext::new().with("lib", "!std"),
        );
    }
}
