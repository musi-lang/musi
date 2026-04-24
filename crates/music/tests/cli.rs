use std::env::temp_dir;
use std::fs;
use std::mem::drop;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::process::Output;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use musi_tooling::ToolingDiagKind;
use serde_json::Value;

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
        let path = temp_dir().join(format!("music-cli-test-{unique}-{sequence}"));
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

fn run_music(args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_music"))
        .args(args)
        .output()
        .expect("music command should run")
}

fn parse_json(output: &[u8]) -> Value {
    serde_json::from_slice(output).expect("stdout should be valid JSON")
}

fn golden_json(text: &str) -> Value {
    serde_json::from_str(text).expect("golden JSON should parse")
}

fn tooling_diag_code(kind: ToolingDiagKind) -> String {
    format!("MS{:04}", kind.code().raw())
}

fn assert_success(output: &Output) {
    assert!(
        output.status.success(),
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[cfg(test)]
mod success {
    use super::*;

    #[test]
    fn help_lists_info_not_inspect() {
        let output = run_music(&["--help"]);

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("info"));
        assert!(!stdout.contains("inspect"));
    }

    #[test]
    fn info_prints_built_artifact_metadata() {
        let test_dir = TempDir::new();
        let source_path = test_dir.path().join("main.ms");
        let artifact_path = test_dir.path().join("main.seam");
        write_file(
            test_dir.path(),
            "main.ms",
            "export let main () : Int := 42;\n",
        );

        let build_output = run_music(&[
            "build",
            source_path.to_str().expect("utf-8 source path"),
            "--out",
            artifact_path.to_str().expect("utf-8 artifact path"),
        ]);
        assert_success(&build_output);

        let output = run_music(&["info", artifact_path.to_str().expect("utf-8 artifact path")]);

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("binaryVersion:"));
        assert!(stdout.contains("exports:"));
    }

    #[test]
    fn info_accepts_source_file() {
        let test_dir = TempDir::new();
        let source_path = test_dir.path().join("main.ms");
        write_file(
            test_dir.path(),
            "main.ms",
            "export let main () : Int := 42;\n",
        );

        let output = run_music(&["info", source_path.to_str().expect("utf-8 source path")]);

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("binaryVersion:"));
        assert!(stdout.contains("exports:"));
    }

    #[test]
    fn info_accepts_extensionless_source_file() {
        let test_dir = TempDir::new();
        let source_stem = test_dir.path().join("main");
        write_file(
            test_dir.path(),
            "main.ms",
            "export let main () : Int := 42;\n",
        );

        let output = run_music(&["info", source_stem.to_str().expect("utf-8 source stem")]);

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("binaryVersion:"));
        assert!(stdout.contains("exports:"));
    }

    #[test]
    fn disasm_accepts_source_file() {
        let test_dir = TempDir::new();
        let source_path = test_dir.path().join("main.ms");
        write_file(
            test_dir.path(),
            "main.ms",
            "export let main () : Int := 42;\n",
        );

        let output = run_music(&["disasm", source_path.to_str().expect("utf-8 source path")]);

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("module seam.projection"));
        assert!(stdout.contains("fn "));
        assert!(stdout.contains("::main"));
    }

    #[test]
    fn disasm_accepts_extensionless_artifact_file() {
        let test_dir = TempDir::new();
        let source_path = test_dir.path().join("main.ms");
        let source_stem = test_dir.path().join("main");
        let artifact_path = test_dir.path().join("main.seam");
        write_file(
            test_dir.path(),
            "main.ms",
            "export let main () : Int := 42;\n",
        );

        let build_output = run_music(&[
            "build",
            source_path.to_str().expect("utf-8 source path"),
            "--out",
            artifact_path.to_str().expect("utf-8 artifact path"),
        ]);
        assert_success(&build_output);
        fs::remove_file(source_path).expect("source should be removable");

        let output = run_music(&["disasm", source_stem.to_str().expect("utf-8 source stem")]);

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("module seam.projection"));
        assert!(stdout.contains("fn "));
        assert!(stdout.contains("::main"));
    }

    #[test]
    fn disasm_seam_level_prints_lowered_seam_il() {
        let test_dir = TempDir::new();
        let source_path = test_dir.path().join("main.ms");
        write_file(
            test_dir.path(),
            "main.ms",
            "export let main () : Int := 42;\n",
        );

        let output = run_music(&[
            "disasm",
            source_path.to_str().expect("utf-8 source path"),
            "--level",
            "seam",
        ]);

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains(".procedure"));
        assert!(stdout.contains("::main"));
    }

    #[test]
    fn json_check_success_writes_only_json_to_stdout() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "main.ms",
            "export let main () : Int := 42;\n",
        );

        let output = run_music(&[
            "check",
            test_dir
                .path()
                .join("main.ms")
                .to_str()
                .expect("utf-8 path"),
            "--diagnostics-format",
            "json",
        ]);

        assert!(output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload, golden_json(include_str!("success/check-ok.json")));
    }
}

#[cfg(test)]
mod failure {
    use super::*;

    #[test]
    fn json_check_tooling_failure_writes_only_json_to_stdout() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "main.ms",
            "import \"@std/math\"; export let main () : Int := 42;\n",
        );

        let output = run_music(&[
            "check",
            test_dir
                .path()
                .join("main.ms")
                .to_str()
                .expect("utf-8 path"),
            "--diagnostics-format",
            "json",
        ]);

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload["status"], "error");
        assert_eq!(payload["diagnostics"][0]["phase"], "tooling");
        assert_eq!(
            payload["diagnostics"][0]["code"],
            tooling_diag_code(ToolingDiagKind::PackageImportRequiresMusi)
        );
    }

    #[test]
    fn json_check_parse_failure_writes_only_json_to_stdout() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "main.ms", "let x := 1");

        let output = run_music(&[
            "check",
            test_dir
                .path()
                .join("main.ms")
                .to_str()
                .expect("utf-8 path"),
            "--diagnostics-format",
            "json",
        ]);

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload["status"], "error");
        assert_eq!(payload["diagnostics"][0]["phase"], "parse");
        assert!(payload["diagnostics"][0]["code"].as_str().is_some());
    }
}
