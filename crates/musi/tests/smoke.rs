#![allow(clippy::tests_outside_test_module, clippy::panic, clippy::unwrap_used)]

use std::io::ErrorKind;
use std::io::Write;
use std::path::Path;

use musi::driver::compile;
use music_found::diag::{emit, Diag};
use music_found::SourceMap;

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
        rendered.contains("error:"),
        "expected 'error:' in rendered diagnostic: {rendered}"
    );
    assert!(
        rendered.contains(':'),
        "expected line:col in rendered diagnostic: {rendered}"
    );
}

#[test]
fn build_produces_seam_bytes() {
    use music_emit::{emit, write_seam};
    use music_hir::HirBundle;

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

    let bundle = HirBundle::new(result.db, result.resolution, result.type_env);
    let module = emit(&bundle).expect("emit failed");
    let seam = write_seam(&module);
    assert!(!seam.is_empty(), "seam output should not be empty");
}
