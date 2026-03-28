#![allow(clippy::tests_outside_test_module, clippy::panic, clippy::unwrap_used)]

use std::fs;
use std::io::ErrorKind;
use std::io::Write;
use std::path::Path;
use std::process::Command;

use music_hir::{TypedModule, analyze_project, compile};
use music_shared::SourceMap;
use music_shared::diag::{Diag, emit};

fn render_diag(diag: &Diag, sources: &SourceMap) -> String {
    let mut buf = Vec::new();
    emit(&mut buf, diag, sources, false).unwrap();
    String::from_utf8(buf).unwrap()
}

#[test]
fn check_valid_file() {
    let mut file = tempfile::NamedTempFile::with_suffix(".ms").expect("tempfile");
    write!(file, "let _x := 42;").expect("write");
    let result = compile(file.path()).expect("compile");
    assert!(!result.has_errors, "expected no errors, got: {:?}", {
        result
            .diagnostics
            .iter()
            .map(|d| d.message.as_str())
            .collect::<Vec<_>>()
    });
}

#[test]
fn check_parse_error() {
    let mut file = tempfile::NamedTempFile::with_suffix(".ms").expect("tempfile");
    write!(file, "let x :=").expect("write");
    let result = compile(file.path()).expect("compile");
    assert!(result.has_errors, "expected parse error");
    assert!(!result.diagnostics.is_empty());
}

#[test]
fn missing_file_returns_io_error() {
    let result = compile(Path::new("/nonexistent/path/file.ms"));
    match result {
        Ok(_) => panic!("expected Err for missing file"),
        Err(e) => assert_eq!(e.kind(), ErrorKind::NotFound),
    }
}

#[test]
fn diagnostic_has_line_col() {
    let mut file = tempfile::NamedTempFile::with_suffix(".ms").expect("tempfile");
    write!(file, "let x :=").expect("write");
    let result = compile(file.path()).expect("compile");
    assert!(!result.diagnostics.is_empty());
    let rendered = render_diag(&result.diagnostics[0], &result.db.source);
    assert!(
        rendered.contains("error[ms"),
        "expected diagnostic code in rendered diagnostic: {rendered}"
    );
    assert!(
        rendered.contains(':'),
        "expected line:col in rendered diagnostic: {rendered}"
    );
}

#[test]
fn build_produces_seam_bytes() {
    use music_codegen::{emit, write_seam};

    let mut file = tempfile::NamedTempFile::with_suffix(".ms").expect("tempfile");
    write!(file, "let _x := 42;").expect("write");
    let result = compile(file.path()).expect("compile");
    assert!(!result.has_errors, "expected no errors, got: {:?}", {
        result
            .diagnostics
            .iter()
            .map(|d| d.message.as_str())
            .collect::<Vec<_>>()
    });

    let bundle = TypedModule::new(result.db, result.resolution, result.type_env);
    let module = emit(&bundle).expect("emit failed");
    let seam = write_seam(&module);
    assert!(!seam.is_empty(), "seam output should not be empty");
}

#[test]
fn analyze_project_resolves_workspace_package_names() {
    let dir = tempfile::tempdir().expect("tempdir");
    fs::write(
        dir.path().join("musi.json"),
        r#"{ "name": "@std", "main": "./index.ms", "exports": { "./testing": "./testing/index.ms", "./option": "./option/index.ms" } }"#,
    )
    .expect("workspace config");
    fs::write(
        dir.path().join("index.ms"),
        "export let version := \"0.1.0\";",
    )
    .expect("root source");

    fs::create_dir_all(dir.path().join("testing")).expect("testing dir");
    fs::write(
        dir.path().join("testing/index.ms"),
        r#"export let pass := 1;
export let describe := (_name, _body) => _body();
export let it := (_name, _body) => _body();
"#,
    )
    .expect("testing source");

    fs::create_dir_all(dir.path().join("option")).expect("option dir");
    fs::write(dir.path().join("option/index.ms"), "export let some := 1;").expect("option source");
    fs::write(
        dir.path().join("option/index.test.ms"),
        r#"import "@std/testing" as _;
import "@std/option" as _;

export let test := () => describe("option", () => (
    it("loads package root", () => some)
));
"#,
    )
    .expect("option test");

    let result = analyze_project(&dir.path().join("option/index.test.ms")).expect("analyze");
    assert!(!result.has_errors, "expected no errors");
}

#[test]
fn test_command_requires_canonical_musi_test_effect_metadata() {
    let dir = tempfile::tempdir().expect("tempdir");
    let test_file = dir.path().join("local_effect.test.ms");
    fs::write(
        &test_file,
        r#"export let Test := effect {
    let emit(payload : Int) : Unit
};

export let test := () : Unit ~> Unit => perform Test.emit(0);
"#,
    )
    .expect("write test file");

    let output = Command::new(env!("CARGO_BIN_EXE_musi"))
        .arg("test")
        .arg(&test_file)
        .output()
        .expect("run musi test");

    assert!(!output.status.success(), "expected musi test to fail");
    let stderr = String::from_utf8(output.stderr).expect("utf8 stderr");
    assert!(
        stderr.contains("missing emitted `musi:test::Test` effect metadata"),
        "unexpected stderr: {stderr}"
    );
}
