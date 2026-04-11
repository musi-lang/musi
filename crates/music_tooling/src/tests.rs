use std::env::temp_dir;
use std::fs;
use std::mem::drop;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use music_module::ModuleKey;
use music_session::{Session, SessionOptions};

use musi_project::ProjectError;

use crate::{
    ToolingError, load_direct_graph, project_error_report, session_error_report,
    tooling_error_report,
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
        let path = temp_dir().join(format!("music-tooling-test-{unique}-{sequence}"));
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
fn loads_direct_graph_with_relative_imports() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "main.ms",
        r#"
        import "./dep";
        export let main () : Int := 42;
        "#,
    );
    write_file(temp.path(), "dep.ms", "export let base : Int := 41;");

    let graph = load_direct_graph(&temp.path().join("main.ms")).expect("graph should load");
    let texts = graph.module_texts().collect::<Vec<_>>();
    let expected = temp
        .path()
        .join("main.ms")
        .canonicalize()
        .expect("main source should canonicalize");

    assert_eq!(
        graph.entry_key(),
        &ModuleKey::new(expected.display().to_string())
    );
    assert_eq!(texts.len(), 2);
}

#[test]
fn direct_graph_rejects_package_imports() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "main.ms",
        r#"
        import "@std/math";
        export let main () : Int := 42;
        "#,
    );

    let err =
        load_direct_graph(&temp.path().join("main.ms")).expect_err("package import should fail");
    assert!(matches!(
        err,
        ToolingError::PackageImportRequiresMusi { .. }
    ));
    assert_eq!(err.diag_code().expect("tooling diag code").raw(), 3631);
}

#[test]
fn session_error_report_carries_file_and_phase() {
    let mut session = Session::new(SessionOptions::default());
    session
        .set_module_text(&ModuleKey::new("main"), "let x := 1")
        .expect("module text should register");
    let err = session
        .check_module(&ModuleKey::new("main"))
        .expect_err("parse failure expected");
    let report = session_error_report("music", "check", None, None, &session, &err);

    assert_eq!(report.tool, "music");
    assert_eq!(report.command, "check");
    assert_eq!(report.status, "error");
    assert_eq!(report.diagnostics[0].phase, "parse");
    assert!(report.diagnostics[0].file.is_some());
}

#[test]
fn tooling_error_report_carries_typed_code() {
    let error = ToolingError::PackageImportRequiresMusi {
        spec: "@std/math".into(),
    };

    let report = tooling_error_report("music", "check", None, None, &error);

    assert_eq!(report.diagnostics[0].phase, "tooling");
    assert_eq!(report.diagnostics[0].code.as_deref(), Some("ms3631"));
}

#[test]
fn project_error_report_carries_typed_code() {
    let error = ProjectError::Validation {
        message: "name is required".into(),
    };

    let report = project_error_report("musi", "check", None, None, &error);

    assert_eq!(report.diagnostics[0].phase, "project");
    assert_eq!(report.diagnostics[0].code.as_deref(), Some("ms3606"));
}
