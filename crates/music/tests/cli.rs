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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn json_check_success_writes_only_json_to_stdout() {
        let temp = TempDir::new();
        write_file(temp.path(), "main.ms", "export let main () : Int := 42;\n");

        let output = run_music(&[
            "check",
            temp.path().join("main.ms").to_str().expect("utf-8 path"),
            "--diagnostics-format",
            "json",
        ]);

        assert!(output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload["status"], "ok");
        assert_eq!(payload["tool"], "music");
    }

    #[test]
    fn json_check_tooling_failure_writes_only_json_to_stdout() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "main.ms",
            "import \"@std/math\"; export let main () : Int := 42;\n",
        );

        let output = run_music(&[
            "check",
            temp.path().join("main.ms").to_str().expect("utf-8 path"),
            "--diagnostics-format",
            "json",
        ]);

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload["status"], "error");
        assert_eq!(payload["diagnostics"][0]["phase"], "tooling");
        assert_eq!(payload["diagnostics"][0]["code"], "MS3631");
    }

    #[test]
    fn json_check_parse_failure_writes_only_json_to_stdout() {
        let temp = TempDir::new();
        write_file(temp.path(), "main.ms", "let x := 1");

        let output = run_music(&[
            "check",
            temp.path().join("main.ms").to_str().expect("utf-8 path"),
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
