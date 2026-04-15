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

fn assert_success(output: &Output) {
    assert!(
        output.status.success(),
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn help_lists_init_and_reserved_commands() {
        let temp = TempDir::new();

        let output = run_musi(&["--help"], temp.path());

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("init"));
        assert!(stdout.contains("compile"));
        assert!(stdout.contains("fmt"));
        assert!(!stdout.contains("new"));
    }

    #[test]
    fn init_creates_package_in_named_directory() {
        let temp = TempDir::new();

        let output = run_musi(&["init", "sample"], temp.path());

        assert_success(&output);
        assert!(temp.path().join("sample/musi.json").exists());
        assert!(temp.path().join("sample/index.ms").exists());
        assert!(temp.path().join("sample/add.test.ms").exists());
        assert!(temp.path().join("sample/.gitignore").exists());
        let manifest = fs::read_to_string(temp.path().join("sample/musi.json"))
            .expect("manifest should be readable");
        let index = fs::read_to_string(temp.path().join("sample/index.ms"))
            .expect("index should be readable");
        let test = fs::read_to_string(temp.path().join("sample/add.test.ms"))
            .expect("test should be readable");
        assert!(manifest.contains("\"name\""));
        assert!(manifest.contains("sample"));
        assert!(manifest.contains("\"entry\""));
        assert!(!manifest.contains("\"main\""));
        assert!(index.contains("\"Hello, world!\""));
        assert!(!index.contains("export let main"));
        assert!(test.contains("import \"@std/testing\""));
        assert!(test.contains("let add"));
        assert!(test.contains("export let test"));
    }

    #[test]
    fn init_creates_package_in_current_directory() {
        let temp = TempDir::new();

        let output = run_musi(&["init"], temp.path());

        assert_success(&output);
        assert!(temp.path().join("musi.json").exists());
        assert!(temp.path().join("index.ms").exists());
        assert!(temp.path().join("add.test.ms").exists());
    }

    #[test]
    fn init_creates_package_that_checks_and_tests() {
        let temp = TempDir::new();

        let output = run_musi(&["init", "sample"], temp.path());

        assert_success(&output);
        assert_success(&run_musi(&["check"], &temp.path().join("sample")));
        let test_output = run_musi(&["test"], &temp.path().join("sample"));
        assert_success(&test_output);
        let stdout = String::from_utf8_lossy(&test_output.stdout);
        assert!(stdout.contains("pass"));
        assert!(stdout.contains("adds values"));
    }

    #[test]
    fn init_dot_uses_current_directory_name() {
        let temp = TempDir::new();

        let output = run_musi(&["init", "."], temp.path());

        assert_success(&output);
        let manifest =
            fs::read_to_string(temp.path().join("musi.json")).expect("manifest should be readable");
        let expected_name = temp
            .path()
            .file_name()
            .and_then(|name| name.to_str())
            .expect("temp dir should have utf-8 name");
        assert!(manifest.contains(expected_name));
    }

    #[test]
    fn init_refuses_existing_package_markers() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", "{}\n");

        let output = run_musi(&["init"], temp.path());

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("already initialized"));
    }

    #[test]
    fn init_refuses_existing_test_marker() {
        let temp = TempDir::new();
        write_file(temp.path(), "add.test.ms", "\n");

        let output = run_musi(&["init"], temp.path());

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("already initialized"));
    }

    #[test]
    fn reserved_command_reports_unavailable_feature() {
        let temp = TempDir::new();

        let output = run_musi(&["fmt", "--check"], temp.path());

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("command `fmt` unavailable"));
        assert!(stderr.contains("formatter not implemented"));
    }

    #[test]
    fn project_info_prints_manifest_metadata() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "index.ms", "export let main () : Int := 42;\n");

        let output = run_musi(&["info"], temp.path());

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("package: app"));
        assert!(stdout.contains("manifest:"));
        assert!(stdout.contains("modules:"));
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
        assert_eq!(payload["diagnostics"][0]["code"], "MS3604");
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
        assert_eq!(payload["diagnostics"][0]["code"], "MS3610");
        assert_eq!(
            payload["diagnostics"][0]["message"],
            "root package entry missing"
        );
        assert!(
            !payload["diagnostics"][0]["message"]
                .as_str()
                .expect("project message should be string")
                .contains("manifest validation failed")
        );
        assert!(payload["diagnostics"][0]["range"].is_object());
    }

    #[test]
    fn json_check_unresolved_import_carries_file_and_range() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(
            temp.path(),
            "index.ms",
            "let Missing := import \"missing\";\nexport let answer : Int := 42;\n",
        );

        let output = run_musi(&["check", "--diagnostics-format", "json"], temp.path());

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload["diagnostics"][0]["phase"], "project");
        assert_eq!(payload["diagnostics"][0]["code"], "MS3615");
        assert_eq!(
            payload["diagnostics"][0]["message"],
            "unresolved import `missing`"
        );
        assert!(payload["diagnostics"][0]["file"].as_str().is_some());
        assert!(payload["diagnostics"][0]["range"].is_object());
    }
}
