#![allow(unused_imports)]

use std::env::temp_dir;
use std::fs;
use std::io;
use std::mem::drop;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use music_base::diag::DiagContext;
use music_module::ModuleKey;
use music_sema::SemaDiagKind;
use music_session::{Session, SessionOptions};

use musi_project::{Project, ProjectDiagKind, ProjectError, ProjectOptions};

use crate::{
    NavigationWorkspace, ToolCompletionKind, ToolDocumentHighlightKind, ToolFoldingRangeKind,
    ToolInlayHintKind, ToolMonikerKind, ToolPosition, ToolSemanticModifier, ToolSemanticTokenKind,
    ToolingDiagKind, ToolingError, artifact::write_output,
    collect_project_diagnostics_with_overlay, completions_for_project_file_with_overlay,
    definition_for_project_file_with_overlay, document_highlights_for_project_file_with_overlay,
    document_links_for_project_file_with_overlay, document_symbols_for_project_file_with_overlay,
    folding_ranges_for_project_file_with_overlay, hover_for_project_file_with_overlay,
    implementation_for_project_file_with_overlay, inlay_hints_for_project_file_with_overlay,
    load_direct_graph, module_docs_for_project_file_with_overlay,
    moniker_for_project_file_with_overlay, outgoing_calls_for_project_file_with_overlay,
    prepare_rename_for_project_file_with_overlay, project_error_report,
    reference_lenses_for_project_file_with_overlay, references_for_project_file_with_overlay,
    rename_for_project_file_with_overlay, selection_ranges_for_project_file_with_overlay,
    semantic_tokens_for_project_file_with_overlay, session_error_report,
    signature_help_for_project_file_with_overlay, tooling_error_report,
    type_definition_for_project_file_with_overlay, workspace_symbols_for_project_file_with_overlay,
    workspace_symbols_for_project_root,
};

static NEXT_TEMP_ID: AtomicU64 = AtomicU64::new(0);

const APP_MANIFEST: &str =
    "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"index.ms\"\n}\n";

fn diag_code(raw: u16) -> String {
    format!("MS{raw:04}")
}

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

fn load_project_error(root: &Path) -> ProjectError {
    Project::load(root, ProjectOptions::default()).expect_err("project load should fail")
}

fn assert_session_error_report(
    source: &str,
    expected_phase: &str,
    expected_message: &str,
    expected_label: &str,
    expected_hint: Option<&str>,
) {
    let mut session = Session::new(SessionOptions::default());
    session
        .set_module_text(&ModuleKey::new("main"), source)
        .expect("module text should register");
    let err = session
        .check_module(&ModuleKey::new("main"))
        .expect_err("session failure expected");
    let report = session_error_report("music", "check", None, None, &session, &err);

    assert_eq!(report.diagnostics[0].phase, expected_phase);
    assert_eq!(report.diagnostics[0].message, expected_message);
    assert_eq!(report.diagnostics[0].labels[0].message, expected_label);
    assert_eq!(report.diagnostics[0].hint.as_deref(), expected_hint);
}

mod success {
    use super::*;

    #[test]
    fn failed_artifact_write_removes_empty_created_parent() {
        let test_dir = TempDir::new();
        let target = test_dir.path().join("target/debug/out.seam");

        let err = write_output(&target, |_| Err(io::Error::other("write failed")))
            .expect_err("write should fail");

        assert!(matches!(err, ToolingError::ToolingIoFailed { .. }));
        assert!(!test_dir.path().join("target").exists());
    }

    #[test]
    fn failed_artifact_write_keeps_existing_parent() {
        let test_dir = TempDir::new();
        let parent = test_dir.path().join("target");
        fs::create_dir_all(&parent).expect("target dir should be created");

        let err = write_output(&parent.join("out.seam"), |_| {
            Err(io::Error::other("write failed"))
        })
        .expect_err("write should fail");

        assert!(matches!(err, ToolingError::ToolingIoFailed { .. }));
        assert!(parent.exists());
    }

    #[test]
    fn loads_direct_graph_with_relative_imports() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "main.ms",
            r#"
        import "./dep";
        export let main () : Int := 42;
        "#,
        );
        write_file(test_dir.path(), "dep.ms", "export let base : Int := 41;");

        let graph = load_direct_graph(&test_dir.path().join("main.ms")).expect("graph should load");
        let texts = graph.module_texts();
        let expected = test_dir
            .path()
            .join("main.ms")
            .canonicalize()
            .expect("main source should canonicalize");

        assert_eq!(
            graph.entry_key(),
            &ModuleKey::new(expected.display().to_string())
        );
        assert_eq!(texts.count(), 2);
    }

    #[test]
    fn completions_include_current_terms_and_visible_bindings() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = r"--- before docs
let before := 1;
let current := bef;
";
        write_file(test_dir.path(), "index.ms", source);

        let completions = completions_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            3,
            19,
        );

        let before = completions
            .iter()
            .find(|item| item.label == "before")
            .expect("before completion should exist");
        assert_eq!(before.documentation.as_deref(), Some("before docs"));
        assert!(completions.iter().any(|item| item.label == "let"));
    }

    #[test]
    fn completions_replace_current_identifier_prefix() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = r"let before := 1;
let current := bef;
";
        write_file(test_dir.path(), "index.ms", source);

        let completions = completions_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            19,
        );
        let before = completions
            .iter()
            .find(|item| item.label == "before")
            .expect("before completion should exist");

        assert_eq!(before.replace_range.start_line, 2);
        assert_eq!(before.replace_range.start_col, 16);
        assert_eq!(before.replace_range.end_line, 2);
        assert_eq!(before.replace_range.end_col, 19);
    }

    #[test]
    fn completions_after_dot_return_record_members_without_keywords() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = r"let point := { x := 1, y := 2 };
point.
";
        write_file(test_dir.path(), "index.ms", source);

        let completions = completions_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            7,
        );

        assert!(completions.iter().any(|item| item.label == "x"));
        assert!(completions.iter().any(|item| item.label == "y"));
        assert!(!completions.iter().any(|item| item.label == "let"));
    }

    #[test]
    fn completions_after_dot_filter_member_prefix() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = r"let span := { lowerBound := 1, upperBound := 4 };
span.lower
";
        write_file(test_dir.path(), "index.ms", source);

        let completions = completions_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            11,
        );
        let labels: Vec<_> = completions.iter().map(|item| item.label.as_str()).collect();

        assert_eq!(labels, ["lowerBound"]);
        assert_eq!(completions[0].replace_range.start_line, 2);
        assert_eq!(completions[0].replace_range.start_col, 6);
        assert_eq!(completions[0].replace_range.end_line, 2);
        assert_eq!(completions[0].replace_range.end_col, 11);
    }

    #[test]
    fn completions_after_dot_classify_imported_function_exports() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(
            test_dir.path(),
            "dep.ms",
            "export let add (left : Int, right : Int) : Int := left + right;\n",
        );
        let source = "let dep := import \"./dep\";\ndep.";
        write_file(test_dir.path(), "index.ms", source);

        let completions = completions_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            5,
        );
        let add = completions
            .iter()
            .find(|completion| completion.label == "add")
            .expect("add completion should exist");

        assert_eq!(add.kind, ToolCompletionKind::Function);
        assert_eq!(add.detail.as_deref(), Some("function"));
    }

    #[test]
    fn completions_inside_import_string_return_project_modules() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"src/index.ms\"\n}\n",
        );
        let source = "let dep := import \"./l\";\n";
        write_file(
            test_dir.path(),
            "src/index.ms",
            "let dep := import \"./lib/dep\";\n",
        );
        write_file(
            test_dir.path(),
            "src/lib/dep.ms",
            "--! dep docs\nexport let value := 1;\n",
        );
        write_file(test_dir.path(), "src/local.ms", "export let local := 1;\n");

        let completions = completions_for_project_file_with_overlay(
            &test_dir.path().join("src/index.ms"),
            Some(source),
            1,
            22,
        );
        let labels = completions
            .iter()
            .map(|item| item.label.as_str())
            .collect::<Vec<_>>();

        assert_eq!(labels, ["./lib/dep", "./local"]);
        assert_eq!(completions[0].documentation.as_deref(), Some("dep docs"));
        assert_eq!(completions[0].replace_range.start_line, 1);
        assert_eq!(completions[0].replace_range.start_col, 20);
        assert_eq!(completions[0].replace_range.end_col, 23);
    }

    #[test]
    fn definition_resolves_local_binding_from_reference() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let before := 1;\nlet after := before;\n";
        write_file(test_dir.path(), "index.ms", source);

        let location = definition_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            14,
        )
        .expect("definition should resolve");

        assert_eq!(location.path, test_dir.path().join("index.ms"));
        assert_eq!(location.range.start_line, 1);
        assert_eq!(location.range.start_col, 5);
        assert_eq!(location.range.end_col, 11);
    }

    #[test]
    fn definition_resolves_import_record_member_from_reference() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(test_dir.path(), "core.ms", "export let Word := Type;\n");
        let source = "let core := import \"./core\";\nlet value := core.Word;\n";
        write_file(test_dir.path(), "index.ms", source);

        let location = definition_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            20,
        )
        .expect("import record member definition should resolve");

        assert_eq!(
            location.path,
            test_dir
                .path()
                .join("core.ms")
                .canonicalize()
                .expect("core path should canonicalize")
        );
        assert_eq!(location.range.start_line, 1);
        assert_eq!(location.range.start_col, 12);
        assert_eq!(location.range.end_col, 16);
    }

    #[test]
    fn definition_resolves_std_list_from_array_member_from_list_tests() {
        let test_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../../lib/std/__tests__/collections/list.test.ms");

        let location = definition_for_project_file_with_overlay(&test_path, None, 7, 24)
            .expect("fromArray definition should resolve");

        assert_eq!(
            location.path,
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../../lib/std/collections/list.ms")
                .canonicalize()
                .expect("list path should canonicalize")
        );
        assert_eq!(location.range.start_line, 19);
        assert_eq!(location.range.start_col, 12);
        assert_eq!(location.range.end_col, 21);
    }

    #[test]
    fn definition_resolves_std_ffi_ptr_member_from_ffi_tests() {
        let test_path =
            PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/__tests__/ffi.test.ms");

        let location = definition_for_project_file_with_overlay(&test_path, None, 8, 36)
            .expect("ptr definition should resolve");

        assert_eq!(
            location.path,
            PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join("../../lib/std/ffi.ms")
                .canonicalize()
                .expect("ffi path should canonicalize")
        );
        assert_eq!(location.range.start_line, 82);
        assert_eq!(location.range.start_col, 12);
        assert_eq!(location.range.end_col, 15);
    }

    #[test]
    fn definition_resolves_local_variant_from_variant_value() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let Option := data { | A | B };
let first := .A;
let second := .B;
";
        write_file(test_dir.path(), "index.ms", source);

        let location = definition_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            15,
        )
        .expect("variant definition should resolve");

        assert_eq!(location.path, test_dir.path().join("index.ms"));
        assert_eq!(location.range.start_line, 1);
        assert_eq!(location.range.start_col, 24);
        assert_eq!(location.range.end_col, 25);
    }

    #[test]
    fn type_definition_resolves_named_value_type() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let Box[T] := data {
  let value : T;
};
let boxedName : Box[String] := {
  value := \"Nora\"
};
boxedName.value;
";
        write_file(test_dir.path(), "index.ms", source);

        let location = type_definition_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            7,
            3,
        )
        .expect("type definition should resolve");

        assert_eq!(location.path, test_dir.path().join("index.ms"));
        assert_eq!(location.range.start_line, 1);
        assert_eq!(location.range.start_col, 5);
        assert_eq!(location.range.end_col, 8);
    }

    #[test]
    fn implementation_query_returns_no_shape_adapters() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let Eq [T] := shape {
  let equals (left : T, right : T) : Bit;
};
let intEq := 1;
let bitEq := 2;
";
        write_file(test_dir.path(), "index.ms", source);

        let implementations = implementation_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            1,
            5,
        );

        assert!(implementations.is_empty());
    }

    #[test]
    fn implementation_query_ignores_workspace_shape_adapters() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let shape_source = "\
export let Eq [T] := shape {
  let equals (left : T, right : T) : Bit;
};
";
        let adapter_source = "\
let shapes := import \"./shapes\";
let Eq := shapes.Eq;
let intEq := 1;
";
        let other_adapter_source = "\
let shapes := import \"./shapes\";
let Eq := shapes.Eq;
let bitEq := 2;
";
        write_file(test_dir.path(), "index.ms", "import \"./shapes\";\n");
        write_file(test_dir.path(), "shapes.ms", shape_source);
        write_file(test_dir.path(), "impls.ms", adapter_source);
        write_file(test_dir.path(), "more_impls.ms", other_adapter_source);

        let diagnostics = collect_project_diagnostics_with_overlay(
            &test_dir.path().join("impls.ms"),
            Some(adapter_source),
        );
        assert!(diagnostics.is_empty(), "{diagnostics:?}");

        let implementations = implementation_for_project_file_with_overlay(
            &test_dir.path().join("shapes.ms"),
            Some(shape_source),
            1,
            12,
        );

        assert!(implementations.is_empty());
    }

    #[test]
    fn references_include_definition_when_requested() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let before := 1;\nlet after := before;\n";
        write_file(test_dir.path(), "index.ms", source);

        let references = references_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            14,
            true,
        );

        assert_eq!(references.len(), 2);
        assert_eq!(references[0].range.start_line, 1);
        assert_eq!(references[1].range.start_line, 2);
    }

    #[test]
    fn references_include_import_record_member_definition_and_usage() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(test_dir.path(), "core.ms", "export let Word := Type;\n");
        let source = "let core := import \"./core\";\nlet value := core.Word;\n";
        write_file(test_dir.path(), "index.ms", source);

        let references = references_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            20,
            true,
        );

        assert!(references.iter().any(|location| {
            location.path
                == test_dir
                    .path()
                    .join("core.ms")
                    .canonicalize()
                    .expect("core path should canonicalize")
                && location.range.start_line == 1
                && location.range.start_col == 12
        }));
        assert!(references.iter().any(|location| {
            location.path
                == test_dir
                    .path()
                    .join("index.ms")
                    .canonicalize()
                    .expect("index path should canonicalize")
                && location.range.start_line == 2
                && location.range.start_col == 19
        }));
    }

    #[test]
    fn references_include_variant_definition_and_uses() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let Option := data { | A | B };
let first := .A;
match first (| .A => 1 | .B => 0);
";
        write_file(test_dir.path(), "index.ms", source);

        let references = references_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            15,
            true,
        );

        assert!(references.iter().any(|location| {
            location.path == test_dir.path().join("index.ms")
                && location.range.start_line == 1
                && location.range.start_col == 24
        }));
        assert!(references.iter().any(|location| {
            location.path == test_dir.path().join("index.ms")
                && location.range.start_line == 2
                && location.range.start_col == 15
        }));
    }

    #[test]
    fn references_include_workspace_import_record_member_uses() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "export let value := 1;\n";
        write_file(test_dir.path(), "index.ms", source);
        write_file(
            test_dir.path(),
            "use.ms",
            "let app := import \"./index\";\nlet result := app.value;\n",
        );

        let references = references_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            1,
            12,
            false,
        );

        assert_eq!(references.len(), 1);
        assert_eq!(
            references[0].path,
            test_dir
                .path()
                .join("use.ms")
                .canonicalize()
                .expect("use path should canonicalize")
        );
        assert_eq!(references[0].range.start_line, 2);
        assert_eq!(references[0].range.start_col, 19);
    }

    #[test]
    fn reference_lenses_count_references_for_document_bindings() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let before := 1;\nlet after := before + before;\n";
        write_file(test_dir.path(), "index.ms", source);

        let lenses = reference_lenses_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        let before = lenses
            .iter()
            .find(|lens| lens.range.start_line == 1 && lens.range.start_col == 5)
            .expect("before reference lens should exist");
        assert_eq!(before.reference_count, 2);
        let after = lenses
            .iter()
            .find(|lens| lens.range.start_line == 2 && lens.range.start_col == 5)
            .expect("after reference lens should exist");
        assert_eq!(after.reference_count, 0);
    }

    #[test]
    fn navigation_workspace_reuses_analysis_for_reference_requests() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(
            test_dir.path(),
            "index.ms",
            "let before := 1;\nlet after := before + before;\n",
        );
        let path = test_dir.path().join("index.ms");
        let mut workspace = NavigationWorkspace::new();

        let references =
            workspace.references_for_project_file_with_overlay(&path, None, 1, 5, false);
        assert_eq!(
            workspace.cached_reference_data_len(&path),
            Some((1, 0, false))
        );
        let lenses = workspace.reference_lenses_for_project_file_with_overlay(&path, None);
        assert_eq!(
            workspace.cached_reference_data_len(&path),
            Some((1, 0, true))
        );
        let lenses_again = workspace.reference_lenses_for_project_file_with_overlay(&path, None);

        assert_eq!(references.len(), 2);
        assert_eq!(workspace.cached_paths_len(), 1);
        assert!(lenses.iter().any(|lens| lens.reference_count == 2));
        assert_eq!(lenses_again, lenses);

        workspace.invalidate_path(&path);

        assert_eq!(workspace.cached_paths_len(), 0);
    }

    #[test]
    fn navigation_workspace_invalidation_clears_cross_file_analysis() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(test_dir.path(), "index.ms", "export let value := 1;\n");
        write_file(
            test_dir.path(),
            "use.ms",
            "let app := import \"./index\";\nlet result := app.value;\n",
        );
        let index_path = test_dir.path().join("index.ms");
        let use_path = test_dir.path().join("use.ms");
        let mut workspace = NavigationWorkspace::new();

        let index_lenses =
            workspace.reference_lenses_for_project_file_with_overlay(&index_path, None);
        let use_lenses = workspace.reference_lenses_for_project_file_with_overlay(&use_path, None);

        assert_eq!(index_lenses[0].reference_count, 1);
        assert!(use_lenses.iter().any(|lens| lens.reference_count == 1));
        assert_eq!(workspace.cached_paths_len(), 2);

        workspace.invalidate_path(&use_path);

        assert_eq!(workspace.cached_paths_len(), 0);
    }

    #[test]
    fn navigation_workspace_reuses_workspace_member_reference_data() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "export let value := 1;\n";
        write_file(test_dir.path(), "index.ms", source);
        write_file(
            test_dir.path(),
            "use.ms",
            "let app := import \"./index\";\nlet result := app.value;\n",
        );
        let path = test_dir.path().join("index.ms");
        let mut workspace = NavigationWorkspace::new();

        let references =
            workspace.references_for_project_file_with_overlay(&path, None, 1, 12, false);
        assert_eq!(references.len(), 1);
        assert_eq!(
            workspace.cached_reference_data_len(&path),
            Some((1, 1, false))
        );

        let lenses = workspace.reference_lenses_for_project_file_with_overlay(&path, None);
        assert_eq!(lenses.len(), 1);
        assert_eq!(lenses[0].reference_count, 1);
        assert_eq!(
            workspace.cached_reference_data_len(&path),
            Some((1, 1, true))
        );

        let references_with_declaration =
            workspace.references_for_project_file_with_overlay(&path, None, 1, 12, true);
        assert_eq!(references_with_declaration.len(), 2);
        assert_eq!(
            workspace.cached_reference_data_len(&path),
            Some((1, 1, true))
        );
    }

    #[test]
    fn document_highlights_kind_declaration_and_references() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let before := 1;\nlet after := before;\n";
        write_file(test_dir.path(), "index.ms", source);

        let highlights = document_highlights_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            14,
        );

        assert_eq!(highlights.len(), 2);
        assert_eq!(highlights[0].location.range.start_line, 1);
        assert_eq!(highlights[0].kind, ToolDocumentHighlightKind::Write);
        assert_eq!(highlights[1].location.range.start_line, 2);
        assert_eq!(highlights[1].kind, ToolDocumentHighlightKind::Read);
    }

    #[test]
    fn rename_returns_workspace_edits_for_definition_and_references() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let before := 1;\nlet after := before;\n";
        write_file(test_dir.path(), "index.ms", source);

        let prepared = prepare_rename_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            14,
        )
        .expect("rename should prepare");
        let edit = rename_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            14,
            "renamed",
        )
        .expect("rename should produce edits");
        let edits = edit
            .changes
            .get(&test_dir.path().join("index.ms"))
            .expect("file edits should exist");

        assert_eq!(prepared.1, "before");
        assert_eq!(edits.len(), 2);
        assert!(edits.iter().all(|edit| edit.new_text == "renamed"));
    }

    #[test]
    fn prepare_rename_on_reference_returns_reference_range() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let before := 1;\nlet after := before;\n";
        write_file(test_dir.path(), "index.ms", source);

        let prepared = prepare_rename_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            14,
        )
        .expect("rename should prepare");

        assert_eq!(prepared.1, "before");
        assert_eq!(prepared.0.start_line, 2);
        assert_eq!(prepared.0.start_col, 14);
        assert_eq!(prepared.0.end_col, 20);
    }

    #[test]
    fn member_references_include_dot_callable_uses() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
let result := one.inc(2);
";
        write_file(test_dir.path(), "index.ms", source);

        let references = references_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            3,
            20,
            true,
        );

        assert_eq!(references.len(), 2);
        assert_eq!(references[0].range.start_line, 1);
        assert_eq!(references[0].range.start_col, 5);
        assert_eq!(references[1].range.start_line, 3);
        assert_eq!(references[1].range.start_col, 19);
    }

    #[test]
    fn prepare_rename_on_member_reference_returns_member_range() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
let result := one.inc(2);
";
        write_file(test_dir.path(), "index.ms", source);

        let prepared = prepare_rename_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            3,
            20,
        )
        .expect("rename should prepare");
        let edit = rename_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            3,
            20,
            "increase",
        )
        .expect("rename should produce edits");
        let edits = edit
            .changes
            .get(&test_dir.path().join("index.ms"))
            .expect("file edits should exist");

        assert_eq!(prepared.1, "inc");
        assert_eq!(prepared.0.start_line, 3);
        assert_eq!(prepared.0.start_col, 19);
        assert_eq!(prepared.0.end_col, 22);
        assert_eq!(edits.len(), 2);
        assert!(edits.iter().all(|edit| edit.new_text == "increase"));
    }

    #[test]
    fn moniker_marks_project_local_bindings() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let value := 1;\nlet other := value;\n";
        write_file(test_dir.path(), "index.ms", source);

        let moniker = moniker_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            15,
        )
        .expect("moniker should resolve");

        assert_eq!(moniker.kind, ToolMonikerKind::Local);
        assert_eq!(moniker.location.range.start_line, 1);
        assert_eq!(moniker.location.range.start_col, 5);
    }

    #[test]
    fn moniker_marks_imported_bindings() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "import \"./dep\";\nlet result := base;\n";
        write_file(test_dir.path(), "index.ms", source);
        write_file(test_dir.path(), "dep.ms", "export let base := 1;\n");

        let moniker = moniker_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            15,
        )
        .expect("import moniker should resolve");

        assert_eq!(moniker.kind, ToolMonikerKind::Import);
        assert_eq!(moniker.location.range.start_line, 1);
        assert_eq!(moniker.location.range.start_col, 8);
    }

    #[test]
    fn document_and_workspace_symbols_include_local_defs() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let before := 1;\nlet after := before;\n";
        write_file(test_dir.path(), "index.ms", source);

        let document_symbols = document_symbols_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );
        let workspace_symbols = workspace_symbols_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            "bef",
        );

        assert!(
            document_symbols
                .iter()
                .any(|symbol| symbol.name == "before")
        );
        assert!(
            workspace_symbols
                .iter()
                .any(|symbol| symbol.name == "before")
        );
    }

    #[test]
    fn document_symbols_use_declaration_range_and_name_selection() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let before :=\n  1;\nlet after := before;\n";
        write_file(test_dir.path(), "index.ms", source);

        let symbols = document_symbols_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );
        let before = symbols
            .iter()
            .find(|symbol| symbol.name == "before")
            .expect("before symbol should exist");

        assert_eq!(before.range.start_line, 1);
        assert_eq!(before.range.start_col, 1);
        assert_eq!(before.range.end_line, 2);
        assert_eq!(before.selection_range.start_line, 1);
        assert_eq!(before.selection_range.start_col, 5);
        assert_eq!(before.selection_range.end_col, 11);
    }

    #[test]
    fn document_symbols_nest_bindings_inside_declarations() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let outer (value : Int) : Int := value;\nlet after := outer(1);\n";
        write_file(test_dir.path(), "index.ms", source);

        let symbols = document_symbols_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );
        let outer = symbols
            .iter()
            .find(|symbol| symbol.name == "outer")
            .expect("outer symbol should exist");

        assert!(
            outer.children.iter().any(|symbol| symbol.name == "value"),
            "{outer:?}"
        );
        assert!(
            symbols.iter().any(|symbol| symbol.name == "after"),
            "{symbols:?}"
        );
        assert!(
            !symbols.iter().any(|symbol| symbol.name == "value"),
            "{symbols:?}"
        );
    }

    #[test]
    fn outgoing_calls_include_direct_name_callees() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let target () := 1;\nlet caller () := target();\n";
        write_file(test_dir.path(), "index.ms", source);

        let calls = outgoing_calls_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            5,
        );

        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].to.name, "target");
        assert_eq!(calls[0].from_ranges.len(), 1);
        assert_eq!(calls[0].from_ranges[0].start_line, 2);
        assert_eq!(calls[0].from_ranges[0].start_col, 18);
        assert_eq!(calls[0].from_ranges[0].end_col, 24);
    }

    #[test]
    fn outgoing_calls_include_member_callees() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
let caller () := one.inc(2);
";
        write_file(test_dir.path(), "index.ms", source);

        let calls = outgoing_calls_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            3,
            5,
        );

        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].to.name, "inc");
        assert_eq!(calls[0].from_ranges.len(), 1);
        assert_eq!(calls[0].from_ranges[0].start_line, 3);
        assert_eq!(calls[0].from_ranges[0].start_col, 22);
        assert_eq!(calls[0].from_ranges[0].end_col, 25);
    }

    #[test]
    fn workspace_symbols_from_project_root_include_workspace_modules_without_open_file() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"entry\": \"src/index.ms\"\n}\n",
        );
        write_file(
            test_dir.path(),
            "src/index.ms",
            "let extra := import \"./extra\";\nlet entryValue := extra.extraValue;\n",
        );
        write_file(
            test_dir.path(),
            "src/extra.ms",
            "export let extraValue := 2;\n",
        );

        let symbols = workspace_symbols_for_project_root(test_dir.path(), "Value");
        let names = symbols
            .iter()
            .map(|symbol| symbol.name.as_str())
            .collect::<Vec<_>>();
        let module_symbols = workspace_symbols_for_project_root(test_dir.path(), "src/extra");

        assert!(names.contains(&"entryValue"));
        assert!(names.contains(&"extraValue"));
        assert!(module_symbols.iter().any(|symbol| {
            symbol.name == "src/extra" && symbol.kind == crate::ToolSymbolKind::Module
        }));
    }

    #[test]
    fn document_links_resolve_static_imports_to_module_paths() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let dep := import \"./dep\";\n";
        write_file(test_dir.path(), "index.ms", source);
        write_file(test_dir.path(), "dep.ms", "export let value := 1;\n");

        let links = document_links_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].specifier, "./dep");
        assert_eq!(links[0].resolved, "@app@0.1.0/dep.ms");
        assert_eq!(
            links[0].target,
            fs::canonicalize(test_dir.path().join("dep.ms")).expect("dep path should canonicalize")
        );
        assert_eq!(links[0].range.start_line, 1);
        assert_eq!(links[0].range.start_col, 19);
        assert_eq!(links[0].range.end_col, 26);
    }

    #[test]
    fn document_links_resolve_package_imports_to_builtin_paths() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let math := import \"@std/math\";\n";
        write_file(test_dir.path(), "index.ms", source);

        let links = document_links_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].specifier, "@std/math");
        assert_eq!(links[0].resolved, "@@std@0.1.0/math.ms");
        assert_eq!(links[0].target.to_string_lossy(), "builtin:/@std/math.ms");
        assert_eq!(links[0].tooltip.as_deref(), Some("Open `@std/math`"));
        assert_eq!(links[0].range.start_line, 1);
        assert_eq!(links[0].range.start_col, 20);
        assert_eq!(links[0].range.end_col, 31);
    }

    #[test]
    fn folding_ranges_include_multiline_nodes_and_block_comments() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = concat!(
            "/",
            "\
-- docs
    more docs -/
let Pair := data {
  let left : Int;
  let right : Int;
};
let value := match 1 (
| 1 => 10
| _ => 0
);
",
        );
        write_file(test_dir.path(), "index.ms", source);

        let ranges = folding_ranges_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(ranges.iter().any(|range| {
            range.kind == Some(ToolFoldingRangeKind::Comment)
                && range.range.start_line == 1
                && range.range.end_line == 2
        }));
        assert!(
            ranges
                .iter()
                .any(|range| range.range.start_line == 3 && range.range.end_line >= 5)
        );
        assert!(
            ranges
                .iter()
                .any(|range| range.range.start_line == 7 && range.range.end_line == 10)
        );
    }

    #[test]
    fn selection_ranges_expand_from_token_to_parent_nodes() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let value := 1;
let other := value + 2;
";
        write_file(test_dir.path(), "index.ms", source);

        let ranges = selection_ranges_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            &[ToolPosition::new(2, 14)],
        );
        let selection = ranges[0]
            .as_ref()
            .expect("selection range should exist for identifier");

        assert_eq!(selection.range.start_line, 2);
        assert_eq!(selection.range.start_col, 14);
        assert_eq!(selection.range.end_line, 2);
        assert_eq!(selection.range.end_col, 19);
        assert!(
            selection
                .parent
                .as_ref()
                .is_some_and(|parent| parent.range.start_line == 2
                    && parent.range.end_col >= selection.range.end_col)
        );
    }

    #[test]
    fn signature_help_returns_callable_signature_and_active_parameter() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let render (port : Int, secure : Bit) : Int := port;
render(8080, 1 = 1);
";
        write_file(test_dir.path(), "index.ms", source);

        let help = signature_help_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            14,
        )
        .expect("signature help should exist inside call");

        assert_eq!(help.active_signature, 0);
        assert_eq!(help.active_parameter, 1);
        assert_eq!(help.signatures.len(), 1);
        assert_eq!(help.signatures[0].label, "render(Int, Bit) -> Int");
        assert_eq!(help.signatures[0].parameters[0].label, "Int");
        assert_eq!(help.signatures[0].parameters[1].label, "Bit");
    }

    #[test]
    fn signature_help_returns_dot_callable_signature() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
one.inc(2);
";
        write_file(test_dir.path(), "index.ms", source);

        let help = signature_help_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            3,
            10,
        )
        .expect("signature help should exist inside dot call");

        assert_eq!(help.active_signature, 0);
        assert_eq!(help.active_parameter, 0);
        assert_eq!(help.signatures.len(), 1);
        assert_eq!(help.signatures[0].label, "one.inc(Int) -> Int");
        assert_eq!(help.signatures[0].parameters[0].label, "Int");
    }

    #[test]
    fn signature_help_returns_imported_member_signature() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(
            test_dir.path(),
            "dep.ms",
            "export let add (left : Int, right : Int) : Int := left + right;\n",
        );
        let source = "\
let dep := import \"./dep\";
dep.add(1, 2);
";
        write_file(test_dir.path(), "index.ms", source);

        let help = signature_help_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            12,
        )
        .expect("signature help should resolve");

        assert_eq!(
            help.signatures[0].label,
            "dep.add(left : Int, right : Int) -> Int"
        );
        assert_eq!(help.active_parameter, 1);
    }

    #[test]
    fn semantic_tokens_complete_textmate_without_lexical_overrides() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "--- greeting value\nlet message : String := \"Hello\";\nmessage;\n";
        write_file(test_dir.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(!tokens.iter().any(|token| matches!(
            token.kind,
            ToolSemanticTokenKind::Keyword
                | ToolSemanticTokenKind::Modifier
                | ToolSemanticTokenKind::Comment
                | ToolSemanticTokenKind::String
                | ToolSemanticTokenKind::Number
                | ToolSemanticTokenKind::Operator
        )));
        assert!(
            tokens
                .iter()
                .any(|token| token.kind == ToolSemanticTokenKind::Variable)
        );
        assert!(tokens.windows(2).all(|pair| {
            if let [left, right] = pair {
                left.range.start_line < right.range.start_line
                    || (left.range.start_line == right.range.start_line
                        && left.range.start_col <= right.range.start_col)
            } else {
                false
            }
        }));
    }

    #[test]
    fn semantic_tokens_mark_attribute_names_as_decorators() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "@link(name := \"m\")\nlet message : String := \"Hello\";\n";
        write_file(test_dir.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Decorator
                && token.range.start_line == 1
                && token.range.start_col == 2
        }));
    }

    #[test]
    fn semantic_tokens_mark_variants_as_enum_members() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let Option := data { | Some(value : Int) | None };
let value := .Some(value := 1);
match value (| .Some(inner) => inner | .None => 0);
";
        write_file(test_dir.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::EnumMember
                && token.range.start_line == 1
                && token.range.start_col == 24
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::EnumMember
                && token.range.start_line == 2
                && token.range.start_col == 15
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::EnumMember
                && token.range.start_line == 3
                && token.range.start_col == 17
        }));
    }

    #[test]
    fn hover_uses_resolved_symbol_range_and_markdown_shape() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(
            test_dir.path(),
            "index.ms",
            "--- greeting value\nlet message : String := \"Hello\";\nmessage;\n",
        );

        let hover = hover_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some("--- greeting value\nlet message : String := \"Hello\";\nmessage;\n"),
            3,
            2,
        )
        .expect("message hover should resolve");

        assert_eq!(hover.range.start_line, 3);
        assert_eq!(hover.range.start_col, 1);
        assert_eq!(hover.range.end_col, 8);
        assert!(
            hover
                .contents
                .starts_with("```musi\n(variable) message : String\n```")
        );
        assert!(hover.contents.contains("greeting value"));
    }

    #[test]
    fn hover_uses_block_doc_but_not_module_doc() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source =
            "--! module docs\n/-- item docs -/\nlet message : String := \"Hello\";\nmessage;\n";
        write_file(test_dir.path(), "index.ms", source);

        let module_docs = module_docs_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        )
        .expect("module docs should be extracted");
        assert!(module_docs.contains("module docs"));

        let hover = hover_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            4,
            2,
        )
        .expect("message hover should resolve");

        assert!(hover.contents.contains("item docs"));
        assert!(!hover.contents.contains("module docs"));
    }

    #[test]
    fn hover_on_module_doc_returns_module_docs() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "--! module docs\n--! more module docs\nlet message : String := \"Hello\";\n";
        write_file(test_dir.path(), "index.ms", source);

        let hover = hover_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            1,
            4,
        )
        .expect("module doc hover should resolve");

        assert_eq!(hover.range.start_line, 1);
        assert_eq!(hover.range.start_col, 1);
        assert_eq!(hover.range.end_line, 2);
        assert!(hover.contents.contains("module docs"));
        assert!(hover.contents.contains("more module docs"));
    }

    #[test]
    fn hover_uses_member_facts_for_record_properties() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let record := { result := 42 };\nrecord.result;\n";
        write_file(test_dir.path(), "index.ms", source);

        let hover = hover_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            9,
        )
        .expect("field hover should resolve");

        assert_eq!(hover.range.start_line, 2);
        assert_eq!(hover.range.start_col, 8);
        assert!(hover.contents.starts_with("```musi\n(property) result : "));
    }

    #[test]
    fn hover_uses_member_facts_for_dot_callable_procedures() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
one.inc(2);
";
        write_file(test_dir.path(), "index.ms", source);

        let hover = hover_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            3,
            6,
        )
        .expect("dot-callable hover should resolve");

        assert_eq!(hover.range.start_line, 3);
        assert_eq!(hover.range.start_col, 5);
        assert!(hover.contents.starts_with("```musi\n(procedure) inc : "));
    }

    #[test]
    fn hover_renders_imported_attached_method_return_type() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(
            test_dir.path(),
            "methods.ms",
            "export let(self : String).byteSize () : Int := 1;\n",
        );
        let source = "import \"./methods.ms\";\n\"abc\".byteSize();\n";
        write_file(test_dir.path(), "index.ms", source);

        let hover = hover_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            8,
        )
        .expect("attached method hover should resolve");

        assert_eq!(hover.range.start_line, 2);
        assert_eq!(hover.range.start_col, 7);
        assert!(
            hover
                .contents
                .starts_with("```musi\n(procedure) byteSize : () -> Int\n```"),
            "{}",
            hover.contents
        );
        assert!(!hover.contents.contains("<error>"));
    }

    #[test]
    fn hover_names_imported_member_parameters() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(
            test_dir.path(),
            "dep.ms",
            "--- Add two numbers.\nexport let add (left : Int, right : Int) : Int := left + right;\n",
        );
        let source = "\
let dep := import \"./dep\";
dep.add(1, 2);
";
        write_file(test_dir.path(), "index.ms", source);

        let hover = hover_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            5,
        )
        .expect("imported member hover should resolve");

        assert!(
            hover
                .contents
                .starts_with("```musi\n(function) add : (left : Int, right : Int) -> Int\n```"),
            "{}",
            hover.contents
        );
        assert!(hover.contents.contains("Add two numbers."));
    }

    #[test]
    fn hover_renders_bits_width_for_std_word_exports() {
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../lib/std/word.ms");

        let hover = hover_for_project_file_with_overlay(&path, None, 86, 13)
            .expect("std word export hover should resolve");

        assert!(
            hover
                .contents
                .starts_with("```musi\n(function) logicalNot32 : Bits[32] -> Bits[32]\n```"),
            "{}",
            hover.contents
        );
        assert!(hover.contents.contains("Invert 32-bit machine word bits."));
    }

    #[test]
    fn semantic_tokens_use_member_facts_for_properties_and_dot_callables() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let record := { result := 42 };
record.result;
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
one.inc(2);
";
        write_file(test_dir.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(
            tokens
                .iter()
                .any(|token| token.kind == ToolSemanticTokenKind::Property)
        );
        assert!(
            tokens
                .iter()
                .any(|token| token.kind == ToolSemanticTokenKind::Procedure)
        );
    }

    #[test]
    fn semantic_tokens_classify_type_context_without_variable_override() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let id[T] (value : T) : T := value;\nlet message : String := \"Hello\";\n";
        write_file(test_dir.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(
            tokens
                .iter()
                .any(|token| token.kind == ToolSemanticTokenKind::TypeParameter)
        );
        assert!(
            tokens
                .iter()
                .any(|token| token.kind == ToolSemanticTokenKind::Type)
        );
        assert!(!tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Variable
                && token.range.start_line == 2
                && token.range.start_col == 15
        }));
    }

    #[test]
    fn semantic_tokens_classify_generic_apply_args_as_types() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let ptr := import \"@std/ffi\";
let pointer := ptr.null[Int]();
let samePointer := ptr.offset[Int](pointer, 0);
";
        write_file(test_dir.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 2
                && token.range.start_col == 25
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 3
                && token.range.start_col == 31
        }));
        assert!(!tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Variable
                && token.range.start_line == 2
                && token.range.start_col == 25
        }));
    }

    #[test]
    fn hover_classifies_generic_apply_args_as_types() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let ptr := import \"@std/ffi\";
let pointer := ptr.null[Int]();
";
        write_file(test_dir.path(), "index.ms", source);

        let hover = hover_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
            2,
            25,
        )
        .expect("type arg should hover");

        assert!(
            hover.contents.starts_with("```musi\n(type) Int"),
            "{}",
            hover.contents
        );
    }

    #[test]
    fn semantic_tokens_work_for_direct_file_outside_package() {
        let test_dir = TempDir::new();
        let source = "let id (value : String) : String := value;\nlet message : String := \"Hello\";\nmessage;\n";
        write_file(test_dir.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(!tokens.iter().any(|token| {
            matches!(
                token.kind,
                ToolSemanticTokenKind::Keyword | ToolSemanticTokenKind::String
            )
        }));
        assert!(
            tokens
                .iter()
                .any(|token| token.kind == ToolSemanticTokenKind::Type)
        );
        assert!(
            tokens
                .iter()
                .any(|token| token.kind == ToolSemanticTokenKind::Variable)
        );
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Parameter
                && token.range.start_line == 1
                && token.range.start_col == 9
        }));
        assert!(!tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 1
                && token.range.start_col == 9
        }));
    }

    #[test]
    fn diagnostics_work_for_direct_file_outside_package() {
        let test_dir = TempDir::new();
        let source = "missing;\n";
        write_file(test_dir.path(), "index.ms", source);

        let diagnostics = collect_project_diagnostics_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(!diagnostics.is_empty());
        assert_eq!(diagnostics[0].phase, "resolve");
        assert!(
            diagnostics[0]
                .file
                .as_deref()
                .is_some_and(|file| file.ends_with("index.ms"))
        );
    }

    #[test]
    fn foundation_core_path_uses_canonical_module_identity() {
        let test_dir = TempDir::new();
        let source = r#"
let Intrinsics := import "musi:intrinsics";
@musi.builtin(name := "Type")
export let Type := Type;
"#;
        write_file(
            test_dir.path(),
            "crates/musi_foundation/modules/core.ms",
            source,
        );
        let path = test_dir
            .path()
            .join("crates/musi_foundation/modules/core.ms");

        let diagnostics = collect_project_diagnostics_with_overlay(&path, Some(source));

        assert!(
            diagnostics.iter().all(|diag| {
                !diag
                    .message
                    .contains(SemaDiagKind::AttrBuiltinRequiresFoundationModule.message())
                    && !diag.message.contains(
                        ProjectDiagKind::SourceImportUnresolved
                            .message_with(&DiagContext::new().with("spec", "musi:intrinsics"))
                            .as_str(),
                    )
            }),
            "{diagnostics:?}"
        );
    }

    #[test]
    fn semantic_tokens_mark_foundation_external_declarations_as_functions() {
        let test_dir = TempDir::new();
        let source = r#"
let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;

@foreign(abi := .musi)
let envGet (name : String) : String;
@foreign(abi := .musi)
let envHas (name : String) : Int;
@foreign(abi := .musi)
let envSet (name : String, value : String) : Int;
"#;
        write_file(
            test_dir.path(),
            "crates/musi_foundation/modules/_env.ms",
            source,
        );
        let path = test_dir
            .path()
            .join("crates/musi_foundation/modules/_env.ms");

        let tokens = semantic_tokens_for_project_file_with_overlay(&path, Some(source));

        assert_eq!(
            tokens
                .iter()
                .filter(|token| token.kind == ToolSemanticTokenKind::Function)
                .count(),
            3
        );
        assert_eq!(
            tokens
                .iter()
                .filter(|token| token.kind == ToolSemanticTokenKind::Parameter)
                .count(),
            4
        );
    }

    #[test]
    fn semantic_tokens_mark_foundation_builtin_return_annotations_as_types() {
        let test_dir = TempDir::new();
        let source = "\
let Core := import \"musi:core\";
let Int := Core.Int;
let String := Core.String;
let Float := Core.Float;
let Unit := Core.Unit;

@foreign(abi := .musi)
let bool () : Int;
@foreign(abi := .musi)
let float01Intrinsic () : Float;

export let float01 () : Float := unsafe (float01Intrinsic());
";
        write_file(
            test_dir.path(),
            "crates/musi_foundation/modules/_env.ms",
            source,
        );
        let path = test_dir
            .path()
            .join("crates/musi_foundation/modules/_env.ms");

        let tokens = semantic_tokens_for_project_file_with_overlay(&path, Some(source));

        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 8
                && token.range.start_col == 15
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 10
                && token.range.start_col == 27
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 12
                && token.range.start_col == 25
        }));
    }

    #[test]
    fn semantic_tokens_mark_foundation_builtin_rhs_names_as_types() {
        let test_dir = TempDir::new();
        let source = "\
let Intrinsics := import \"musi:intrinsics\";
@musi.builtin(name := \"Type\")
export let Type := Type;
@musi.builtin(name := \"Float\")
export let Float := Float;
";
        write_file(
            test_dir.path(),
            "crates/musi_foundation/modules/core.ms",
            source,
        );
        let path = test_dir
            .path()
            .join("crates/musi_foundation/modules/core.ms");

        let tokens = semantic_tokens_for_project_file_with_overlay(&path, Some(source));

        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 3
                && token.range.start_col == 20
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 5
                && token.range.start_col == 21
        }));
        assert!(!tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Variable
                && matches!(token.range.start_line, 3 | 5)
                && matches!(token.range.start_col, 20 | 21)
        }));
    }

    #[test]
    fn hover_marks_foundation_builtin_type_references_as_types() {
        let test_dir = TempDir::new();
        let source = "\
let Intrinsics := import \"musi:intrinsics\";
@musi.builtin(name := \"Type\")
export let Type := Type;
";
        write_file(
            test_dir.path(),
            "crates/musi_foundation/modules/core.ms",
            source,
        );
        let path = test_dir
            .path()
            .join("crates/musi_foundation/modules/core.ms");

        let hover = hover_for_project_file_with_overlay(&path, Some(source), 3, 20)
            .expect("builtin type reference should hover");

        assert!(
            hover.contents.starts_with("```musi\n(type) Type"),
            "{}",
            hover.contents
        );
    }

    #[test]
    fn hover_marks_foundation_return_annotations_as_types() {
        let test_dir = TempDir::new();
        let source = "\
let Core := import \"musi:core\";
let Int := Core.Int;
let String := Core.String;
let Float := Core.Float;

@foreign(abi := .musi)
let float01Intrinsic () : Float;
";
        write_file(
            test_dir.path(),
            "crates/musi_foundation/modules/_env.ms",
            source,
        );
        let path = test_dir
            .path()
            .join("crates/musi_foundation/modules/_env.ms");

        let hover = hover_for_project_file_with_overlay(&path, Some(source), 7, 27)
            .expect("return annotation should hover");

        assert!(
            hover.contents.starts_with("```musi\n(type) Float"),
            "{}",
            hover.contents
        );
    }

    #[test]
    fn inlay_hints_include_parameter_names_and_inferred_types() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(
            test_dir.path(),
            "index.ms",
            "let add (left : Int, right : Int) : Int := left + right;\nlet result := add(1, 2);\n",
        );

        let hints = inlay_hints_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(
                "let add (left : Int, right : Int) : Int := left + right;\nlet result := add(1, 2);\n",
            ),
        );

        assert!(hints.iter().any(|hint| {
            hint.kind == ToolInlayHintKind::Type && hint.label.starts_with(": Int")
        }));
        assert!(
            hints
                .iter()
                .any(|hint| { hint.kind == ToolInlayHintKind::Parameter && hint.label == "left:" })
        );
        assert!(
            hints.iter().any(|hint| {
                hint.kind == ToolInlayHintKind::Parameter && hint.label == "right:"
            })
        );
    }

    #[test]
    fn inlay_hints_place_inferred_function_types_after_parameter_clause() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let add (left : Int, right : Int) := left + right;\n";
        write_file(test_dir.path(), "index.ms", source);

        let hints = inlay_hints_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );
        let hint = hints
            .iter()
            .find(|hint| hint.kind == ToolInlayHintKind::Type)
            .expect("function should expose inferred type inlay hint");
        let close_paren_col0 = source.find(") :=").expect("parameter clause close");
        let bind_col0 = source.rfind(":=").expect("bind operator");
        let hint_col0 = hint.position.col.saturating_sub(1);

        assert_eq!(hint.position.line, 1);
        assert!(
            hint_col0 >= close_paren_col0.saturating_add(1) && hint_col0 <= bind_col0,
            "hint col={} should be between parameter close and := ({}..={})",
            hint_col0,
            close_paren_col0.saturating_add(1),
            bind_col0
        );
    }

    #[test]
    fn inlay_hints_include_receiver_prefix_functions() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "let(self : String).byteSize () := 1;\n";
        write_file(test_dir.path(), "index.ms", source);

        let hints = inlay_hints_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );
        let hint = hints
            .iter()
            .find(|hint| hint.kind == ToolInlayHintKind::Type)
            .expect("receiver-prefix function should expose inferred type inlay hint");
        let close_paren_col0 = source.rfind(") :=").expect("parameter clause close");
        let bind_col0 = source.rfind(":=").expect("bind operator");
        let hint_col0 = hint.position.col.saturating_sub(1);

        assert_eq!(hint.position.line, 1);
        assert!(
            hint_col0 >= close_paren_col0.saturating_add(1) && hint_col0 <= bind_col0,
            "hint col={} should be between parameter close and := ({}..={})",
            hint_col0,
            close_paren_col0.saturating_add(1),
            bind_col0
        );
    }

    #[test]
    fn inlay_hints_mark_literal_parameter_arguments() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "\
let add (left : Int, right : Int) : Int := left + right;
let value := 1;
add(value, 2);
";
        write_file(test_dir.path(), "index.ms", source);

        let hints = inlay_hints_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );
        let left = hints
            .iter()
            .find(|hint| hint.kind == ToolInlayHintKind::Parameter && hint.label == "left:")
            .expect("left parameter hint should exist");
        let right = hints
            .iter()
            .find(|hint| hint.kind == ToolInlayHintKind::Parameter && hint.label == "right:")
            .expect("right parameter hint should exist");

        assert!(!left.is_literal_argument);
        assert!(right.is_literal_argument);
    }

    #[test]
    fn inlay_hints_include_imported_member_parameter_names() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(
            test_dir.path(),
            "dep.ms",
            "export let add (left : Int, right : Int) : Int := left + right;\n",
        );
        let source = "\
let dep := import \"./dep\";
dep.add(1, 2);
";
        write_file(test_dir.path(), "index.ms", source);

        let hints = inlay_hints_for_project_file_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );
        let labels = hints
            .iter()
            .filter(|hint| hint.kind == ToolInlayHintKind::Parameter)
            .map(|hint| hint.label.as_str())
            .collect::<Vec<_>>();

        assert_eq!(labels, ["left:", "right:"]);
    }
}

mod failure {
    use super::*;

    #[test]
    fn direct_graph_rejects_package_imports() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "main.ms",
            r#"
        import "@std/math";
        export let main () : Int := 42;
        "#,
        );

        let err = load_direct_graph(&test_dir.path().join("main.ms"))
            .expect_err("package import should fail");
        assert!(matches!(
            err,
            ToolingError::PackageImportRequiresMusi { .. }
        ));
        assert_eq!(err.diag_code().expect("tooling diag code").raw(), 5101);
    }

    #[test]
    fn project_diagnostics_use_file_paths_instead_of_module_keys() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        let source = "missing;\n";
        write_file(test_dir.path(), "index.ms", source);

        let diagnostics = collect_project_diagnostics_with_overlay(
            &test_dir.path().join("index.ms"),
            Some(source),
        );

        assert!(!diagnostics.is_empty());
        assert!(
            diagnostics[0]
                .file
                .as_deref()
                .is_some_and(|file| file.ends_with("index.ms"))
        );
        assert!(
            diagnostics[0]
                .file
                .as_deref()
                .is_some_and(|file| !file.starts_with('@'))
        );
        assert!(diagnostics[0].labels.iter().all(|label| {
            label
                .file
                .as_deref()
                .is_none_or(|file| !file.starts_with('@'))
        }));
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
    fn session_error_report_carries_resolve_label() {
        assert_session_error_report(
            "missing;",
            "resolve",
            "unbound name `missing`",
            "name `missing` unresolved in this scope",
            None,
        );
    }

    #[test]
    fn session_error_report_carries_sema_type_mismatch() {
        assert_session_error_report(
            "let x : Int := \"no\";",
            "sema",
            "value expected `Int`, found `String`",
            "value has type `String` here",
            None,
        );
    }

    #[test]
    fn tooling_error_report_carries_typed_code() {
        let error = ToolingError::PackageImportRequiresMusi {
            spec: "@std/math".into(),
        };

        let report = tooling_error_report("music", "check", None, None, &error);

        assert_eq!(report.diagnostics[0].phase, "tooling");
        let code = diag_code(ToolingDiagKind::PackageImportRequiresMusi.code().raw());
        assert_eq!(report.diagnostics[0].code.as_deref(), Some(code.as_str()));
    }

    #[test]
    fn project_error_report_carries_typed_code() {
        let validation_message = ProjectDiagKind::ManifestPackageNameMissing.message();
        let error = ProjectError::ManifestValidationFailed {
            message: validation_message.into(),
        };

        let report = project_error_report("musi", "check", None, None, &error);

        assert_eq!(report.diagnostics[0].phase, "project");
        let code = diag_code(ProjectDiagKind::ManifestValidationFailed.code().raw());
        assert_eq!(report.diagnostics[0].code.as_deref(), Some(code.as_str()));
    }

    #[test]
    fn project_error_report_carries_manifest_source_range() {
        let test_dir = TempDir::new();
        write_file(
            test_dir.path(),
            "musi.json",
            r#"{
  "exports": {
    "bad": "./index.ms"
  }
}"#,
        );

        let error = load_project_error(test_dir.path());
        let report = project_error_report("musi", "check", None, None, &error);

        assert_eq!(report.diagnostics[0].phase, "project");
        let kind = ProjectDiagKind::ManifestExportKeyInvalid;
        let context = DiagContext::new().with("key", "bad");
        let code = diag_code(kind.code().raw());
        assert_eq!(report.diagnostics[0].code.as_deref(), Some(code.as_str()));
        assert!(report.diagnostics[0].file.is_some());
        assert!(report.diagnostics[0].range.is_some());
        assert_eq!(report.diagnostics[0].message, kind.message_with(&context));
    }

    #[test]
    fn project_error_report_carries_unresolved_import_range() {
        let test_dir = TempDir::new();
        write_file(test_dir.path(), "musi.json", APP_MANIFEST);
        write_file(
            test_dir.path(),
            "index.ms",
            "let Missing := import \"missing\";\nexport let result : Int := 42;\n",
        );

        let error = load_project_error(test_dir.path());
        let report = project_error_report("musi", "check", None, None, &error);

        assert_eq!(report.diagnostics[0].phase, "project");
        let kind = ProjectDiagKind::SourceImportUnresolved;
        let context = DiagContext::new().with("spec", "missing");
        let code = diag_code(kind.code().raw());
        assert_eq!(report.diagnostics[0].code.as_deref(), Some(code.as_str()));
        assert_eq!(report.diagnostics[0].message, kind.message_with(&context));
        assert!(report.diagnostics[0].file.is_some());
        assert!(report.diagnostics[0].range.is_some());
        assert_eq!(
            report.diagnostics[0].labels[0].message,
            kind.label_with(&context)
        );
    }
}
