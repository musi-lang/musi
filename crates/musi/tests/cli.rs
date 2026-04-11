use std::env::temp_dir;
use std::fs;
use std::mem::drop;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::process::Output;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

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
        let path = temp_dir().join(format!("musi-cli-test-{unique}-{sequence}"));
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

fn run_musi(args: &[&str], cwd: &Path) -> Output {
    Command::new(env!("CARGO_BIN_EXE_musi"))
        .args(args)
        .current_dir(cwd)
        .output()
        .expect("musi command should run")
}

fn parse_json(output: &[u8]) -> Value {
    serde_json::from_slice(output).expect("stdout should be valid JSON")
}

#[test]
fn json_check_success_writes_only_json_to_stdout() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
    );
    write_file(temp.path(), "index.ms", "export let main () : Int := 42;\n");

    let output = run_musi(&["check", "--diagnostics-format", "json"], temp.path());

    assert!(output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    let payload = parse_json(&output.stdout);
    assert_eq!(payload["status"], "ok");
    assert_eq!(payload["tool"], "musi");
}

#[test]
fn json_check_manifest_failure_writes_only_json_to_stdout() {
    let temp = TempDir::new();
    write_file(temp.path(), "musi.json", "{ invalid json\n");

    let output = run_musi(&["check", "--diagnostics-format", "json"], temp.path());

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    let payload = parse_json(&output.stdout);
    assert_eq!(payload["status"], "error");
    assert_eq!(payload["diagnostics"][0]["phase"], "project");
    assert_eq!(payload["diagnostics"][0]["code"], "ms3604");
    assert!(payload["diagnostics"][0]["range"].is_object());
}

#[test]
fn json_check_parse_failure_writes_only_json_to_stdout() {
    let temp = TempDir::new();
    write_file(
        temp.path(),
        "musi.json",
        "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
    );
    write_file(temp.path(), "index.ms", "let x := 1");

    let output = run_musi(&["check", "--diagnostics-format", "json"], temp.path());

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    let payload = parse_json(&output.stdout);
    assert_eq!(payload["status"], "error");
    assert_eq!(payload["diagnostics"][0]["phase"], "parse");
    assert!(payload["diagnostics"][0]["code"].as_str().is_some());
}

#[test]
fn json_check_root_package_missing_uses_direct_message_and_range() {
    let temp = TempDir::new();
    write_file(temp.path(), "musi.json", "{\n  \"version\": \"0.1.0\"\n}\n");

    let output = run_musi(&["check", "--diagnostics-format", "json"], temp.path());

    assert!(!output.status.success());
    assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    let payload = parse_json(&output.stdout);
    assert_eq!(payload["status"], "error");
    assert_eq!(payload["diagnostics"][0]["phase"], "project");
    assert_eq!(payload["diagnostics"][0]["code"], "ms3610");
    assert_eq!(
        payload["diagnostics"][0]["message"],
        "root package entry missing"
    );
    assert!(
        payload["diagnostics"][0]["message"]
            .as_str()
            .expect("project message should be string")
            .find("manifest validation failed")
            .is_none()
    );
    assert!(payload["diagnostics"][0]["range"].is_object());
}
