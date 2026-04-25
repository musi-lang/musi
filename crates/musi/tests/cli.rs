use std::env::temp_dir;
use std::fs;
use std::io::Write;
use std::mem::drop;
use std::path::{Path, PathBuf};
use std::process::Output;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use musi_project::ProjectDiagKind;
use music_base::diag::DiagContext;
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

fn run_musi_with_input(args: &[&str], cwd: &Path, input: &str) -> Output {
    let mut child = Command::new(env!("CARGO_BIN_EXE_musi"))
        .args(args)
        .current_dir(cwd)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("musi command should spawn");
    child
        .stdin
        .as_mut()
        .expect("stdin should be piped")
        .write_all(input.as_bytes())
        .expect("stdin should be written");
    child
        .wait_with_output()
        .expect("musi command should finish")
}

fn parse_json(output: &[u8]) -> Value {
    serde_json::from_slice(output).expect("stdout should be valid JSON")
}

fn golden_json(text: &str) -> Value {
    serde_json::from_str(text).expect("golden JSON should parse")
}

fn diag_code(kind: ProjectDiagKind) -> String {
    format!("MS{:04}", kind.code().raw())
}

fn normalize_project_paths(mut payload: Value) -> Value {
    payload["package_root"] = Value::String("<package-root>".into());
    payload["manifest"] = Value::String("<manifest>".into());
    payload
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
    fn help_lists_init_and_reserved_commands() {
        let test_dir = TempDir::new();

        let output = run_musi(&["--help"], test_dir.path());

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("init"));
        assert!(stdout.contains("compile"));
        assert!(stdout.contains("fmt"));
        assert!(!stdout.contains("new"));
    }

    #[test]
    fn init_creates_package_in_named_directory() {
        let test_dir = TempDir::new();

        let output = run_musi(&["init", "sample"], test_dir.path());

        assert_success(&output);
        assert!(test_dir.path().join("sample/musi.json").exists());
        assert!(test_dir.path().join("sample/index.ms").exists());
        assert!(
            test_dir
                .path()
                .join("sample/__tests__/add.test.ms")
                .exists()
        );
        assert!(test_dir.path().join("sample/.gitignore").exists());
        let manifest = fs::read_to_string(test_dir.path().join("sample/musi.json"))
            .expect("manifest should be readable");
        let index = fs::read_to_string(test_dir.path().join("sample/index.ms"))
            .expect("index should be readable");
        let test = fs::read_to_string(test_dir.path().join("sample/__tests__/add.test.ms"))
            .expect("test should be readable");
        assert!(manifest.contains("\"name\""));
        assert!(manifest.contains("sample"));
        assert!(manifest.contains("\"entry\""));
        assert!(!manifest.contains("\"main\""));
        assert_eq!(
            index,
            "let io := import \"@std/io\";\n\nlet message := \"Hello, world!\";\nio.writeLine(message);\n"
        );
        assert!(!index.contains("export let main"));
        assert!(test.contains("import \"@std/testing\""));
        assert!(test.contains("let add"));
        assert!(test.contains("export let test"));
    }

    #[test]
    fn init_creates_package_in_current_directory() {
        let test_dir = TempDir::new();

        let output = run_musi(&["init"], test_dir.path());

        assert_success(&output);
        assert!(test_dir.path().join("musi.json").exists());
        assert!(test_dir.path().join("index.ms").exists());
        assert!(test_dir.path().join("__tests__/add.test.ms").exists());
    }

    #[test]
    fn init_dot_uses_current_directory_name() {
        let test_dir = TempDir::new();

        let output = run_musi(&["init", "."], test_dir.path());

        assert_success(&output);
        let manifest = fs::read_to_string(test_dir.path().join("musi.json"))
            .expect("manifest should be readable");
        let expected_name = test_dir
            .path()
            .file_name()
            .and_then(|name| name.to_str())
            .expect("temp dir should have utf-8 name");
        assert!(manifest.contains(expected_name));
    }

    #[test]
    fn project_info_prints_manifest_metadata() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "index.ms",
            "export let main () : Int := 42;\n",
        );

        let output = run_musi(&["info"], test_dir.path());

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("package: app"));
        assert!(stdout.contains("manifest:"));
        assert!(stdout.contains("modules:"));
    }

    #[test]
    fn check_accepts_explicit_relative_entry_file() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "index.ms",
            "let message := \"Hello\";\nmessage;\n",
        );

        let output = run_musi(&["check", "index.ms"], test_dir.path());

        assert_success(&output);
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    }

    #[test]
    fn run_evaluates_module_without_main_export() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "index.ms",
            "let message := \"Hello\";\nmessage;\n",
        );

        let output = run_musi(&["run", "index.ms"], test_dir.path());

        assert_success(&output);
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    }

    #[test]
    fn run_executes_top_level_write_line_without_main_export() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "index.ms",
            "let io := import \"@std/io\";\nio.writeLine(\"Hello\");\n",
        );

        let output = run_musi(&["run", "index.ms"], test_dir.path());

        assert_success(&output);
        assert_eq!(String::from_utf8_lossy(&output.stdout), "Hello\n");
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    }

    #[test]
    fn test_uses_vitest_style_output_without_std_dependency_suites() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "index.ms",
            "let message := \"Hello\";\nmessage;\n",
        );
        write_file(
            test_dir.path(),
            "add.test.ms",
            r#"let Testing := import "@std/testing";

let add (left : Int, right : Int) : Int := left + right;

export let test () :=
  (
    Testing.describe("add");
    Testing.it("adds values", Testing.toBe(add(2, 3), 5));
    Testing.endDescribe()
  );
"#,
        );

        let output = run_musi(&["test"], test_dir.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("✓ add.test.ms (1)"));
        assert!(stdout.contains("✓ add > adds values"));
        assert!(stdout.contains("Test Files  1 passed (1)"));
        assert!(stdout.contains("Tests  1 passed (1)"));
        assert!(!stdout.contains("@@std"));
        assert!(!stdout.contains("pass "));
    }

    #[test]
    fn check_does_not_create_package_target_dir() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let value := 1;\n");

        let output = run_musi(&["check"], test_dir.path());

        assert_success(&output);
        assert!(!test_dir.path().join("target").exists());
    }

    #[test]
    fn test_does_not_create_package_target_dir() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let value := 1;\n");
        write_file(
            test_dir.path(),
            "index.test.ms",
            r#"let Testing := import "@std/testing";

export let test () :=
  (
    Testing.describe("target");
    Testing.it("passes", Testing.toBe(1, 1));
    Testing.endDescribe()
  );
"#,
        );

        let output = run_musi(&["test"], test_dir.path());

        assert_success(&output);
        assert!(!test_dir.path().join("target").exists());
    }

    #[test]
    fn build_default_writes_entry_artifact_without_target_dir() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let value := 1;\n");

        let output = run_musi(&["build"], test_dir.path());

        assert_success(&output);
        assert!(test_dir.path().join("index.seam").exists());
        assert!(!test_dir.path().join("target").exists());
    }

    #[test]
    fn test_captures_passing_module_output() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "index.ms",
            "let message := \"Hello\";\nmessage;\n",
        );
        write_file(
            test_dir.path(),
            "io.test.ms",
            r#"let Testing := import "@std/testing";
let Io := import "musi:io";
let Log := import "musi:log";

export let test () :=
  (
    Testing.describe("io");
    Io.printLine("hidden stdout");
    Io.printErrorLine("hidden stderr");
    Log.write(40, "hidden log");
    Testing.it("passes", Testing.toBe(1, 1));
    Testing.endDescribe()
  );
"#,
        );

        let output = run_musi(&["test", "io.test.ms"], test_dir.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stdout.contains("✓ io.test.ms (1)"));
        assert!(!stdout.contains("hidden stdout"));
        assert!(!stdout.contains("hidden stderr"));
        assert!(!stdout.contains("hidden log"));
        assert!(!stderr.contains("hidden stdout"));
        assert!(!stderr.contains("hidden stderr"));
        assert!(!stderr.contains("hidden log"));
    }

    #[test]
    fn fmt_rewrites_project_file() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let x:=1;");

        let output = run_musi(&["fmt"], test_dir.path());

        assert_success(&output);
        assert_eq!(
            fs::read_to_string(test_dir.path().join("index.ms")).expect("file should be readable"),
            "let x := 1;\n"
        );
    }

    #[test]
    fn fmt_stdin_writes_formatted_stdout() {
        let test_dir = TempDir::new();

        let output = run_musi_with_input(&["fmt", "-"], test_dir.path(), "let x:=1;");

        assert_success(&output);
        assert_eq!(String::from_utf8_lossy(&output.stdout), "let x := 1;\n");
    }

    #[test]
    fn fmt_stdin_uses_manifest_config_and_cli_indent_overrides() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"useTabs\": true, \"indentWidth\": 8 }\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "export let result : Int := 1;");

        let manifest_tabs = run_musi_with_input(
            &["fmt", "--ext", "ms", "-"],
            test_dir.path(),
            "let x:=data{| A};",
        );
        let editor_spaces = run_musi_with_input(
            &[
                "fmt",
                "--ext",
                "ms",
                "--indent-width",
                "4",
                "--use-spaces",
                "-",
            ],
            test_dir.path(),
            "let x:=data{| A};",
        );

        assert_success(&manifest_tabs);
        assert_success(&editor_spaces);
        assert_eq!(
            String::from_utf8_lossy(&manifest_tabs.stdout),
            "let x := data {\n\t| A\n};\n"
        );
        assert_eq!(
            String::from_utf8_lossy(&editor_spaces.stdout),
            "let x := data {\n    | A\n};\n"
        );
    }

    #[test]
    fn fmt_stdin_uses_profile_and_match_alignment_overrides() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"profile\": \"expanded\", \"matchArmIndent\": \"pipeAligned\" }\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "export let result : Int := 1;");
        let source = "export let describe (target : Ordering) : String := match target(| .Less => \"less\" | .GreaterThanEverything => \"greater\" | _ => \"same\");";

        let output = run_musi_with_input(&["fmt", "--ext", "ms", "-"], test_dir.path(), source);

        assert_success(&output);
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "export let describe (\n  target : Ordering\n) : String :=\n  match target (\n  | .Less                  => \"less\"\n  | .GreaterThanEverything => \"greater\"\n  | _                      => \"same\"\n  );\n"
        );
    }

    #[test]
    fn fmt_cli_match_alignment_overrides_manifest_profile() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"profile\": \"expanded\" }\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "export let result : Int := 1;");
        let source = "export let describe (target : Ordering) : String := match target(| .Less => \"less\" | .GreaterThanEverything => \"greater\" | _ => \"same\");";

        let output = run_musi_with_input(
            &[
                "fmt",
                "--ext",
                "ms",
                "--match-arm-indent",
                "pipe-aligned",
                "--match-arm-arrow-alignment",
                "none",
                "-",
            ],
            test_dir.path(),
            source,
        );

        assert_success(&output);
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "export let describe (\n  target : Ordering\n) : String :=\n  match target (\n  | .Less => \"less\"\n  | .GreaterThanEverything => \"greater\"\n  | _ => \"same\"\n  );\n"
        );
    }

    #[test]
    fn fmt_markdown_formats_musi_fences_only() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "README.md",
            "```musi\nlet x:=1;\n```\n\n```ts\nlet x=1\n```\n",
        );

        let output = run_musi(&["fmt", "README.md"], test_dir.path());

        assert_success(&output);
        assert_eq!(
            fs::read_to_string(test_dir.path().join("README.md")).expect("file should be readable"),
            "```musi\nlet x := 1;\n```\n\n```ts\nlet x=1\n```\n"
        );
    }

    #[test]
    fn fmt_explicit_relative_file_uses_invocation_directory() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"pkg\"] }\n}\n",
        );
        write_file(
            test_dir.path(),
            "pkg/musi.json",
            "{\n  \"name\": \"pkg\",\n  \"entry\": \"./index.ms\"\n}\n",
        );
        write_file(test_dir.path(), "pkg/index.ms", "let x:=1;");

        let output = run_musi(&["fmt", "pkg/index.ms"], test_dir.path());

        assert_success(&output);
        assert_eq!(
            fs::read_to_string(test_dir.path().join("pkg/index.ms"))
                .expect("file should be readable"),
            "let x := 1;\n"
        );
    }

    #[test]
    fn fmt_all_formats_workspace_members() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\", \"util\"] }\n}\n",
        );
        write_file(
            test_dir.path(),
            "app/musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "app/index.ms", "let app:=1;");
        write_file(
            test_dir.path(),
            "util/musi.json",
            "{\n  \"name\": \"util\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "util/index.ms", "let util:=2;");

        let output = run_musi(&["fmt", "--all"], test_dir.path());

        assert_success(&output);
        assert_eq!(
            fs::read_to_string(test_dir.path().join("app/index.ms"))
                .expect("file should be readable"),
            "let app := 1;\n"
        );
        assert_eq!(
            fs::read_to_string(test_dir.path().join("util/index.ms"))
                .expect("file should be readable"),
            "let util := 2;\n"
        );
    }

    #[test]
    fn fmt_all_rejects_explicit_path() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let x:=1;");

        let output = run_musi(&["fmt", "--all", "index.ms"], test_dir.path());

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("incompatible command arguments `--all` and `PATH`"));
    }

    #[test]
    fn check_workspace_checks_member_entries() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\", \"util\"] }\n}\n",
        );
        write_file(
            test_dir.path(),
            "app/musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "app/index.ms", "let app := 1;\n");
        write_file(
            test_dir.path(),
            "util/musi.json",
            "{\n  \"name\": \"util\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "util/index.ms", "let util := 2;\n");

        let output = run_musi(&["check", "--workspace"], test_dir.path());

        assert_success(&output);
    }

    #[test]
    fn check_virtual_workspace_defaults_to_workspace_members() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\"] }\n}\n",
        );
        write_file(
            test_dir.path(),
            "app/musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "app/index.ms", "let app := 1;\n");

        let output = run_musi(&["check"], test_dir.path());

        assert_success(&output);
    }

    #[test]
    fn check_explicit_workspace_root_checks_root_and_members_by_default() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"root\",\n  \"version\": \"0.1.0\",\n  \"workspace\": { \"members\": [\"member\"] }\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let root := 1;\n");
        write_file(
            test_dir.path(),
            "member/musi.json",
            "{\n  \"name\": \"member\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "member/index.ms", "let member := 2;\n");

        let output = run_musi(&["check"], test_dir.path());

        assert_success(&output);
    }

    #[test]
    fn test_workspace_runs_member_tests() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\", \"util\"] }\n}\n",
        );
        for package in ["app", "util"] {
            write_file(
                test_dir.path(),
                &format!("{package}/musi.json"),
                &format!("{{\n  \"name\": \"{package}\",\n  \"version\": \"0.1.0\"\n}}\n"),
            );
            write_file(
                test_dir.path(),
                &format!("{package}/index.ms"),
                "let value := 1;\n",
            );
            write_file(
                test_dir.path(),
                &format!("{package}/add.test.ms"),
                r#"let Testing := import "@std/testing";

export let test () :=
  (
    Testing.describe("workspace");
    Testing.it("passes", Testing.toBe(1, 1));
    Testing.endDescribe()
  );
"#,
            );
        }

        let output = run_musi(&["test", "--workspace"], test_dir.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("✓ app/add.test.ms (1)"));
        assert!(stdout.contains("✓ util/add.test.ms (1)"));
    }

    #[test]
    fn test_package_directory_target_runs_that_package_tests() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"packages/std\"] }\n}\n",
        );
        write_file(
            test_dir.path(),
            "packages/std/musi.json",
            "{\n  \"name\": \"@std\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "packages/std/index.ms",
            "let value := 1;\n",
        );
        write_file(
            test_dir.path(),
            "packages/std/__tests__/std.test.ms",
            "export let test () := ();\n",
        );

        let output = run_musi(&["test", "packages/std"], test_dir.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("✓ __tests__/std.test.ms (0)"));
    }

    #[test]
    fn test_package_file_target_runs_that_test_module() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"packages/std\"] }\n}\n",
        );
        write_file(
            test_dir.path(),
            "packages/std/musi.json",
            "{\n  \"name\": \"@std\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "packages/std/index.ms",
            "let value := 1;\n",
        );
        write_file(
            test_dir.path(),
            "packages/std/__tests__/std.test.ms",
            "export let test () := ();\n",
        );

        let output = run_musi(
            &["test", "packages/std/__tests__/std.test.ms"],
            test_dir.path(),
        );

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("✓ __tests__/std.test.ms (0)"));
    }

    #[test]
    fn test_explicit_workspace_root_runs_root_and_member_tests_by_default() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"root\",\n  \"version\": \"0.1.0\",\n  \"workspace\": { \"members\": [\"member\"] }\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let root := 1;\n");
        write_file(
            test_dir.path(),
            "root.test.ms",
            r#"let Testing := import "@std/testing";

export let test () :=
  (
    Testing.describe("root");
    Testing.it("passes", Testing.toBe(1, 1));
    Testing.endDescribe()
  );
"#,
        );
        write_file(
            test_dir.path(),
            "member/musi.json",
            "{\n  \"name\": \"member\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "member/index.ms", "let member := 2;\n");
        write_file(
            test_dir.path(),
            "member/member.test.ms",
            r#"let Testing := import "@std/testing";

export let test () :=
  (
    Testing.describe("member");
    Testing.it("passes", Testing.toBe(1, 1));
    Testing.endDescribe()
  );
"#,
        );

        let output = run_musi(&["test"], test_dir.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("✓ root.test.ms (1)"));
        assert!(stdout.contains("✓ member/member.test.ms (1)"));
    }

    #[test]
    fn build_workspace_writes_member_artifacts() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\", \"util\"] }\n}\n",
        );
        write_file(
            test_dir.path(),
            "app/musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "app/index.ms", "let app := 1;\n");
        write_file(
            test_dir.path(),
            "util/musi.json",
            "{\n  \"name\": \"util\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "util/index.ms", "let util := 2;\n");

        let output = run_musi(&["build", "--workspace"], test_dir.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("app/index.seam"));
        assert!(stdout.contains("util/index.seam"));
        assert!(test_dir.path().join("app/index.seam").exists());
        assert!(test_dir.path().join("util/index.seam").exists());
    }

    #[test]
    fn build_explicit_workspace_root_writes_root_and_member_artifacts_by_default() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"root\",\n  \"version\": \"0.1.0\",\n  \"workspace\": { \"members\": [\"member\"] }\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let root := 1;\n");
        write_file(
            test_dir.path(),
            "member/musi.json",
            "{\n  \"name\": \"member\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "member/index.ms", "let member := 2;\n");

        let output = run_musi(&["build"], test_dir.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("index.seam"));
        assert!(stdout.contains("member/index.seam"));
        assert!(test_dir.path().join("index.seam").exists());
        assert!(test_dir.path().join("member/index.seam").exists());
    }

    #[test]
    fn json_check_success_writes_only_json_to_stdout() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "index.ms",
            "export let main () : Int := 42;\n",
        );

        let output = run_musi(&["check", "--diagnostics-format", "json"], test_dir.path());

        assert!(output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(
            normalize_project_paths(payload),
            golden_json(include_str!("success/check-ok.json"))
        );
    }
}

#[cfg(test)]
mod failure {
    use super::*;

    #[test]
    fn init_refuses_existing_package_markers() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", "{}\n");

        let output = run_musi(&["init"], test_dir.path());

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("already initialized"));
    }

    #[test]
    fn init_refuses_existing_test_marker() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "add.test.ms", "\n");

        let output = run_musi(&["init"], test_dir.path());

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("already initialized"));
    }

    #[test]
    fn fmt_check_reports_unformatted_file_without_writing() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let x:=1;");

        let output = run_musi(&["fmt", "--check"], test_dir.path());

        assert!(!output.status.success());
        assert_eq!(
            fs::read_to_string(test_dir.path().join("index.ms")).expect("file should be readable"),
            "let x:=1;"
        );
        assert!(String::from_utf8_lossy(&output.stdout).contains("index.ms"));
    }

    #[test]
    fn fmt_check_stdin_reports_unformatted_without_stdout() {
        let test_dir = TempDir::new();

        let output = run_musi_with_input(&["fmt", "--check", "-"], test_dir.path(), "let x:=1;");

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stdout).is_empty());
    }

    #[test]
    fn fmt_rejects_unknown_extension_override() {
        let test_dir = TempDir::new();

        let output =
            run_musi_with_input(&["fmt", "--ext", "txt", "-"], test_dir.path(), "let x:=1;");

        assert!(!output.status.success());
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("unsupported formatter extension")
        );
    }

    #[test]
    fn fmt_rejects_musi_extension_override() {
        let test_dir = TempDir::new();

        let output =
            run_musi_with_input(&["fmt", "--ext", "musi", "-"], test_dir.path(), "let x:=1;");

        assert!(!output.status.success());
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("unsupported formatter extension")
        );
    }

    #[test]
    fn json_check_manifest_failure_writes_only_json_to_stdout() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", "{ invalid json\n");

        let output = run_musi(&["check", "--diagnostics-format", "json"], test_dir.path());

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload["status"], "error");
        assert_eq!(payload["diagnostics"][0]["phase"], "project");
        assert_eq!(
            payload["diagnostics"][0]["code"],
            diag_code(ProjectDiagKind::InvalidManifestJson)
        );
        assert!(payload["diagnostics"][0]["range"].is_object());
    }

    #[test]
    fn json_check_parse_failure_writes_only_json_to_stdout() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(test_dir.path(), "index.ms", "let x := 1");

        let output = run_musi(&["check", "--diagnostics-format", "json"], test_dir.path());

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload["status"], "error");
        assert_eq!(payload["diagnostics"][0]["phase"], "parse");
        assert!(payload["diagnostics"][0]["code"].as_str().is_some());
    }

    #[test]
    fn json_check_root_package_missing_uses_direct_message_and_range() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"version\": \"0.1.0\"\n}\n",
        );

        let output = run_musi(&["check", "--diagnostics-format", "json"], test_dir.path());

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload["status"], "error");
        assert_eq!(payload["diagnostics"][0]["phase"], "project");
        assert_eq!(
            payload["diagnostics"][0]["code"],
            diag_code(ProjectDiagKind::ManifestPackageNameMissing)
        );
        let manifest_path = test_dir
            .path()
            .join("musi.json")
            .canonicalize()
            .expect("manifest path should canonicalize");
        let context = DiagContext::new().with("path", manifest_path.display());
        assert_eq!(
            payload["diagnostics"][0]["message"],
            ProjectDiagKind::ManifestPackageNameMissing.message_with(&context)
        );
        let fallback_context = DiagContext::new().with(
            "message",
            ProjectDiagKind::ManifestPackageNameMissing.message_with(&context),
        );
        assert_ne!(
            payload["diagnostics"][0]["message"],
            ProjectDiagKind::ManifestValidationFailed.message_with(&fallback_context)
        );
        assert!(payload["diagnostics"][0]["range"].is_object());
    }

    #[test]
    fn json_check_unresolved_import_carries_file_and_range() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "index.ms",
            "let Missing := import \"missing\";\nexport let result : Int := 42;\n",
        );

        let output = run_musi(&["check", "--diagnostics-format", "json"], test_dir.path());

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
        let payload = parse_json(&output.stdout);
        assert_eq!(payload["diagnostics"][0]["phase"], "project");
        assert_eq!(
            payload["diagnostics"][0]["code"],
            diag_code(ProjectDiagKind::SourceImportUnresolved)
        );
        let context = DiagContext::new().with("spec", "missing");
        assert_eq!(
            payload["diagnostics"][0]["message"],
            ProjectDiagKind::SourceImportUnresolved.message_with(&context)
        );
        assert!(payload["diagnostics"][0]["file"].as_str().is_some());
        assert!(payload["diagnostics"][0]["range"].is_object());
    }
}

#[cfg(test)]
mod e2e {
    use super::*;

    #[test]
    fn init_creates_package_that_checks_and_tests() {
        let test_dir = TempDir::new();

        let output = run_musi(&["init", "sample"], test_dir.path());

        assert_success(&output);
        assert_success(&run_musi(&["check"], &test_dir.path().join("sample")));
        let run_output = run_musi(&["run", "index.ms"], &test_dir.path().join("sample"));
        assert_success(&run_output);
        assert_eq!(
            String::from_utf8_lossy(&run_output.stdout),
            "Hello, world!\n"
        );
        let test_output = run_musi(&["test"], &test_dir.path().join("sample"));
        assert_success(&test_output);
        let stdout = String::from_utf8_lossy(&test_output.stdout);
        assert!(stdout.contains("✓ __tests__/add.test.ms (1)"));
        assert!(stdout.contains("adds values"));
        assert!(!stdout.contains("@@std"));
    }
}
