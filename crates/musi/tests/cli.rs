use std::env::temp_dir;
use std::fs;
use std::io::Write;
use std::mem::drop;
use std::path::{Path, PathBuf};
use std::process::Output;
use std::process::{Command, Stdio};
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
        let temp = TempDir::new();

        let output = run_musi(&["init"], temp.path());

        assert_success(&output);
        assert!(temp.path().join("musi.json").exists());
        assert!(temp.path().join("index.ms").exists());
        assert!(temp.path().join("add.test.ms").exists());
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
    fn check_accepts_explicit_relative_entry_file() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            temp.path(),
            "index.ms",
            "let message := \"Hello\";\nmessage;\n",
        );

        let output = run_musi(&["check", "index.ms"], temp.path());

        assert_success(&output);
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    }

    #[test]
    fn run_evaluates_module_without_main_export() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            temp.path(),
            "index.ms",
            "let message := \"Hello\";\nmessage;\n",
        );

        let output = run_musi(&["run", "index.ms"], temp.path());

        assert_success(&output);
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    }

    #[test]
    fn run_executes_top_level_write_line_without_main_export() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            temp.path(),
            "index.ms",
            "let io := import \"@std/io\";\nio.writeLine(\"Hello\");\n",
        );

        let output = run_musi(&["run", "index.ms"], temp.path());

        assert_success(&output);
        assert_eq!(String::from_utf8_lossy(&output.stdout), "Hello\n");
        assert!(String::from_utf8_lossy(&output.stderr).is_empty());
    }

    #[test]
    fn test_uses_vitest_style_output_without_std_dependency_suites() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            temp.path(),
            "index.ms",
            "let message := \"Hello\";\nmessage;\n",
        );
        write_file(
            temp.path(),
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

        let output = run_musi(&["test"], temp.path());

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
    fn test_captures_passing_module_output() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n",
        );
        write_file(
            temp.path(),
            "index.ms",
            "let message := \"Hello\";\nmessage;\n",
        );
        write_file(
            temp.path(),
            "io.test.ms",
            r#"let Testing := import "@std/testing";
let Runtime := import "musi:runtime";

export let test () :=
  (
    Testing.describe("io");
    Runtime.ioPrintLine("hidden stdout");
    Runtime.ioPrintErrorLine("hidden stderr");
    Runtime.logWrite(40, "hidden log");
    Testing.it("passes", Testing.toBe(1, 1));
    Testing.endDescribe()
  );
"#,
        );

        let output = run_musi(&["test", "io.test.ms"], temp.path());

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
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "index.ms", "let x:=1;");

        let output = run_musi(&["fmt"], temp.path());

        assert_success(&output);
        assert_eq!(
            fs::read_to_string(temp.path().join("index.ms")).expect("file should be readable"),
            "let x := 1;\n"
        );
    }

    #[test]
    fn fmt_stdin_writes_formatted_stdout() {
        let temp = TempDir::new();

        let output = run_musi_with_input(&["fmt", "-"], temp.path(), "let x:=1;");

        assert_success(&output);
        assert_eq!(String::from_utf8_lossy(&output.stdout), "let x := 1;\n");
    }

    #[test]
    fn fmt_stdin_uses_manifest_config_and_cli_indent_overrides() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"useTabs\": true, \"indentWidth\": 8 }\n}\n",
        );
        write_file(temp.path(), "index.ms", "export let result : Int := 1;");

        let manifest_tabs = run_musi_with_input(
            &["fmt", "--ext", "ms", "-"],
            temp.path(),
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
            temp.path(),
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
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"profile\": \"expanded\", \"matchArmIndent\": \"pipeAligned\" }\n}\n",
        );
        write_file(temp.path(), "index.ms", "export let result : Int := 1;");
        let source = "export let describe (target : Ordering) : String := match target(| .Less => \"less\" | .GreaterThanEverything => \"greater\" | _ => \"same\");";

        let output = run_musi_with_input(&["fmt", "--ext", "ms", "-"], temp.path(), source);

        assert_success(&output);
        assert_eq!(
            String::from_utf8_lossy(&output.stdout),
            "export let describe (\n  target : Ordering\n) : String :=\n  match target (\n  | .Less                  => \"less\"\n  | .GreaterThanEverything => \"greater\"\n  | _                      => \"same\"\n  );\n"
        );
    }

    #[test]
    fn fmt_cli_match_alignment_overrides_manifest_profile() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"profile\": \"expanded\" }\n}\n",
        );
        write_file(temp.path(), "index.ms", "export let result : Int := 1;");
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
            temp.path(),
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
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(
            temp.path(),
            "README.md",
            "```musi\nlet x:=1;\n```\n\n```ts\nlet x=1\n```\n",
        );

        let output = run_musi(&["fmt", "README.md"], temp.path());

        assert_success(&output);
        assert_eq!(
            fs::read_to_string(temp.path().join("README.md")).expect("file should be readable"),
            "```musi\nlet x := 1;\n```\n\n```ts\nlet x=1\n```\n"
        );
    }

    #[test]
    fn fmt_explicit_relative_file_uses_invocation_directory() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"pkg\"] }\n}\n",
        );
        write_file(
            temp.path(),
            "pkg/musi.json",
            "{\n  \"name\": \"pkg\",\n  \"entry\": \"./index.ms\"\n}\n",
        );
        write_file(temp.path(), "pkg/index.ms", "let x:=1;");

        let output = run_musi(&["fmt", "pkg/index.ms"], temp.path());

        assert_success(&output);
        assert_eq!(
            fs::read_to_string(temp.path().join("pkg/index.ms")).expect("file should be readable"),
            "let x := 1;\n"
        );
    }

    #[test]
    fn fmt_all_formats_workspace_members() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\", \"util\"] }\n}\n",
        );
        write_file(
            temp.path(),
            "app/musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "app/index.ms", "let app:=1;");
        write_file(
            temp.path(),
            "util/musi.json",
            "{\n  \"name\": \"util\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "util/index.ms", "let util:=2;");

        let output = run_musi(&["fmt", "--all"], temp.path());

        assert_success(&output);
        assert_eq!(
            fs::read_to_string(temp.path().join("app/index.ms")).expect("file should be readable"),
            "let app := 1;\n"
        );
        assert_eq!(
            fs::read_to_string(temp.path().join("util/index.ms")).expect("file should be readable"),
            "let util := 2;\n"
        );
    }

    #[test]
    fn fmt_all_rejects_explicit_path() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "index.ms", "let x:=1;");

        let output = run_musi(&["fmt", "--all", "index.ms"], temp.path());

        assert!(!output.status.success());
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains("incompatible command arguments `--all` and `PATH`"));
    }

    #[test]
    fn check_workspace_checks_member_entries() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\", \"util\"] }\n}\n",
        );
        write_file(
            temp.path(),
            "app/musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "app/index.ms", "let app := 1;\n");
        write_file(
            temp.path(),
            "util/musi.json",
            "{\n  \"name\": \"util\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "util/index.ms", "let util := 2;\n");

        let output = run_musi(&["check", "--workspace"], temp.path());

        assert_success(&output);
    }

    #[test]
    fn check_virtual_workspace_defaults_to_workspace_members() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\"] }\n}\n",
        );
        write_file(
            temp.path(),
            "app/musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "app/index.ms", "let app := 1;\n");

        let output = run_musi(&["check"], temp.path());

        assert_success(&output);
    }

    #[test]
    fn check_explicit_workspace_root_checks_root_and_members_by_default() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"root\",\n  \"version\": \"0.1.0\",\n  \"workspace\": { \"members\": [\"member\"] }\n}\n",
        );
        write_file(temp.path(), "index.ms", "let root := 1;\n");
        write_file(
            temp.path(),
            "member/musi.json",
            "{\n  \"name\": \"member\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "member/index.ms", "let member := 2;\n");

        let output = run_musi(&["check"], temp.path());

        assert_success(&output);
    }

    #[test]
    fn test_workspace_runs_member_tests() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\", \"util\"] }\n}\n",
        );
        for package in ["app", "util"] {
            write_file(
                temp.path(),
                &format!("{package}/musi.json"),
                &format!("{{\n  \"name\": \"{package}\",\n  \"version\": \"0.1.0\"\n}}\n"),
            );
            write_file(
                temp.path(),
                &format!("{package}/index.ms"),
                "let value := 1;\n",
            );
            write_file(
                temp.path(),
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

        let output = run_musi(&["test", "--workspace"], temp.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("✓ app/add.test.ms (1)"));
        assert!(stdout.contains("✓ util/add.test.ms (1)"));
    }

    #[test]
    fn test_explicit_workspace_root_runs_root_and_member_tests_by_default() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"root\",\n  \"version\": \"0.1.0\",\n  \"workspace\": { \"members\": [\"member\"] }\n}\n",
        );
        write_file(temp.path(), "index.ms", "let root := 1;\n");
        write_file(
            temp.path(),
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
            temp.path(),
            "member/musi.json",
            "{\n  \"name\": \"member\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "member/index.ms", "let member := 2;\n");
        write_file(
            temp.path(),
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

        let output = run_musi(&["test"], temp.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("✓ root.test.ms (1)"));
        assert!(stdout.contains("✓ member/member.test.ms (1)"));
    }

    #[test]
    fn build_workspace_writes_member_artifacts() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"workspace\": { \"members\": [\"app\", \"util\"] }\n}\n",
        );
        write_file(
            temp.path(),
            "app/musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "app/index.ms", "let app := 1;\n");
        write_file(
            temp.path(),
            "util/musi.json",
            "{\n  \"name\": \"util\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "util/index.ms", "let util := 2;\n");

        let output = run_musi(&["build", "--workspace"], temp.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("app/index.seam"));
        assert!(stdout.contains("util/index.seam"));
        assert!(temp.path().join("app/index.seam").exists());
        assert!(temp.path().join("util/index.seam").exists());
    }

    #[test]
    fn build_explicit_workspace_root_writes_root_and_member_artifacts_by_default() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"root\",\n  \"version\": \"0.1.0\",\n  \"workspace\": { \"members\": [\"member\"] }\n}\n",
        );
        write_file(temp.path(), "index.ms", "let root := 1;\n");
        write_file(
            temp.path(),
            "member/musi.json",
            "{\n  \"name\": \"member\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "member/index.ms", "let member := 2;\n");

        let output = run_musi(&["build"], temp.path());

        assert_success(&output);
        let stdout = String::from_utf8_lossy(&output.stdout);
        assert!(stdout.contains("index.seam"));
        assert!(stdout.contains("member/index.seam"));
        assert!(temp.path().join("index.seam").exists());
        assert!(temp.path().join("member/index.seam").exists());
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
    fn fmt_check_reports_unformatted_file_without_writing() {
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\"\n}\n",
        );
        write_file(temp.path(), "index.ms", "let x:=1;");

        let output = run_musi(&["fmt", "--check"], temp.path());

        assert!(!output.status.success());
        assert_eq!(
            fs::read_to_string(temp.path().join("index.ms")).expect("file should be readable"),
            "let x:=1;"
        );
        assert!(String::from_utf8_lossy(&output.stdout).contains("index.ms"));
    }

    #[test]
    fn fmt_check_stdin_reports_unformatted_without_stdout() {
        let temp = TempDir::new();

        let output = run_musi_with_input(&["fmt", "--check", "-"], temp.path(), "let x:=1;");

        assert!(!output.status.success());
        assert!(String::from_utf8_lossy(&output.stdout).is_empty());
    }

    #[test]
    fn fmt_rejects_unknown_extension_override() {
        let temp = TempDir::new();

        let output = run_musi_with_input(&["fmt", "--ext", "txt", "-"], temp.path(), "let x:=1;");

        assert!(!output.status.success());
        assert!(
            String::from_utf8_lossy(&output.stderr).contains("unsupported formatter extension")
        );
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
            "missing root package entry"
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
            "let Missing := import \"missing\";\nexport let result : Int := 42;\n",
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

#[cfg(test)]
mod e2e {
    use super::*;

    #[test]
    fn init_creates_package_that_checks_and_tests() {
        let temp = TempDir::new();

        let output = run_musi(&["init", "sample"], temp.path());

        assert_success(&output);
        assert_success(&run_musi(&["check"], &temp.path().join("sample")));
        let run_output = run_musi(&["run", "index.ms"], &temp.path().join("sample"));
        assert_success(&run_output);
        assert_eq!(
            String::from_utf8_lossy(&run_output.stdout),
            "Hello, world!\n"
        );
        let test_output = run_musi(&["test"], &temp.path().join("sample"));
        assert_success(&test_output);
        let stdout = String::from_utf8_lossy(&test_output.stdout);
        assert!(stdout.contains("✓ add.test.ms (1)"));
        assert!(stdout.contains("adds values"));
        assert!(!stdout.contains("@@std"));
    }
}
