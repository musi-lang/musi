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
    ToolInlayHintKind, ToolSemanticModifier, ToolSemanticTokenKind, ToolingDiagKind, ToolingError,
    artifact::write_output, collect_project_diagnostics_with_overlay,
    completions_for_project_file_with_overlay, hover_for_project_file_with_overlay,
    inlay_hints_for_project_file_with_overlay, load_direct_graph,
    module_docs_for_project_file_with_overlay, project_error_report,
    semantic_tokens_for_project_file_with_overlay, session_error_report, tooling_error_report,
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
        let temp = TempDir::new();
        let target = temp.path().join("target/debug/out.seam");

        let err = write_output(&target, |_| Err(io::Error::other("write failed")))
            .expect_err("write should fail");

        assert!(matches!(err, ToolingError::ToolingIoFailed { .. }));
        assert!(!temp.path().join("target").exists());
    }

    #[test]
    fn failed_artifact_write_keeps_existing_parent() {
        let temp = TempDir::new();
        let parent = temp.path().join("target");
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
        let texts = graph.module_texts();
        let expected = temp
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = r"let before := 1;
let current := bef;
";
        write_file(temp.path(), "index.ms", source);

        let completions = completions_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
            Some(source),
            2,
            19,
        );

        assert!(completions.iter().any(|item| item.label == "before"));
        assert!(completions.iter().any(|item| item.label == "let"));
    }

    #[test]
    fn completions_replace_current_identifier_prefix() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = r"let before := 1;
let current := bef;
";
        write_file(temp.path(), "index.ms", source);

        let completions = completions_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = r"let point := { x := 1, y := 2 };
point.
";
        write_file(temp.path(), "index.ms", source);

        let completions = completions_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = r"let span := 1 .. 4;
span.lower
";
        write_file(temp.path(), "index.ms", source);

        let completions = completions_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
    fn semantic_tokens_complete_textmate_without_lexical_overrides() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "--- greeting value\nlet message : String := \"Hello\";\nmessage;\n";
        write_file(temp.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "@link(symbol := \"data.tag\")\nlet message : String := \"Hello\";\n";
        write_file(temp.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
            Some(source),
        );

        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Decorator
                && token.range.start_line == 1
                && token.range.start_col == 2
        }));
    }

    #[test]
    fn semantic_tokens_mark_law_names_as_functions() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "let Eq[T] := shape { law reflexive(value : T) := eq(value, value); };\n";
        write_file(temp.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
            Some(source),
        );

        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Function
                && token.range.start_line == 1
                && token.range.start_col == 26
        }));
        assert!(!tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Variable
                && token.range.start_line == 1
                && token.range.start_col == 31
        }));
    }

    #[test]
    fn semantic_tokens_mark_variants_as_enum_members() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "\
let Option := data { | Some(value : Int) | None };
let value := .Some(value := 1);
match value (| .Some(inner) => inner | .None => 0);
";
        write_file(temp.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        write_file(
            temp.path(),
            "index.ms",
            "--- greeting value\nlet message : String := \"Hello\";\nmessage;\n",
        );

        let hover = hover_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source =
            "--! module docs\n/-- item docs -/\nlet message : String := \"Hello\";\nmessage;\n";
        write_file(temp.path(), "index.ms", source);

        let module_docs =
            module_docs_for_project_file_with_overlay(&temp.path().join("index.ms"), Some(source))
                .expect("module docs should be extracted");
        assert!(module_docs.contains("module docs"));

        let hover =
            hover_for_project_file_with_overlay(&temp.path().join("index.ms"), Some(source), 4, 2)
                .expect("message hover should resolve");

        assert!(hover.contents.contains("item docs"));
        assert!(!hover.contents.contains("module docs"));
    }

    #[test]
    fn hover_uses_member_facts_for_record_properties() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "let record := { result := 42 };\nrecord.result;\n";
        write_file(temp.path(), "index.ms", source);

        let hover =
            hover_for_project_file_with_overlay(&temp.path().join("index.ms"), Some(source), 2, 9)
                .expect("field hover should resolve");

        assert_eq!(hover.range.start_line, 2);
        assert_eq!(hover.range.start_col, 8);
        assert!(hover.contents.starts_with("```musi\n(property) result : "));
    }

    #[test]
    fn hover_uses_member_facts_for_dot_callable_procedures() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
one.inc(2);
";
        write_file(temp.path(), "index.ms", source);

        let hover =
            hover_for_project_file_with_overlay(&temp.path().join("index.ms"), Some(source), 3, 6)
                .expect("dot-callable hover should resolve");

        assert_eq!(hover.range.start_line, 3);
        assert_eq!(hover.range.start_col, 5);
        assert!(hover.contents.starts_with("```musi\n(procedure) inc : "));
    }

    #[test]
    fn hover_renders_imported_attached_method_return_type() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        write_file(
            temp.path(),
            "methods.ms",
            "export let(self : String).length () : Int := 1;\n",
        );
        let source = "import \"./methods.ms\";\n\"abc\".length();\n";
        write_file(temp.path(), "index.ms", source);

        let hover =
            hover_for_project_file_with_overlay(&temp.path().join("index.ms"), Some(source), 2, 8)
                .expect("attached method hover should resolve");

        assert_eq!(hover.range.start_line, 2);
        assert_eq!(hover.range.start_col, 7);
        assert!(
            hover
                .contents
                .starts_with("```musi\n(procedure) length : () -> Int\n```"),
            "{}",
            hover.contents
        );
        assert!(!hover.contents.contains("<error>"));
    }

    #[test]
    fn semantic_tokens_use_member_facts_for_properties_and_dot_callables() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "\
let record := { result := 42 };
record.result;
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
one.inc(2);
";
        write_file(temp.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "let id[T] (value : T) : T := value;\nlet message : String := \"Hello\";\n";
        write_file(temp.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "\
let ptr := import \"@std/ffi\";
let pointer := ptr.null[Int]();
let samePointer := ptr.offset[Int](pointer, 0);
";
        write_file(temp.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "\
let ptr := import \"@std/ffi\";
let pointer := ptr.null[Int]();
";
        write_file(temp.path(), "index.ms", source);

        let hover =
            hover_for_project_file_with_overlay(&temp.path().join("index.ms"), Some(source), 2, 25)
                .expect("type arg should hover");

        assert!(
            hover.contents.starts_with("```musi\n(type) Int"),
            "{}",
            hover.contents
        );
    }

    #[test]
    fn semantic_tokens_work_for_direct_file_outside_package() {
        let temp = TempDir::new();
        let source = "let id (value : String) : String := value;\nlet message : String := \"Hello\";\nmessage;\n";
        write_file(temp.path(), "index.ms", source);

        let tokens = semantic_tokens_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
        let temp = TempDir::new();
        let source = "missing;\n";
        write_file(temp.path(), "index.ms", source);

        let diagnostics =
            collect_project_diagnostics_with_overlay(&temp.path().join("index.ms"), Some(source));

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
        let temp = TempDir::new();
        let source = r#"
let Intrinsics := import "musi:intrinsics";
@musi.known(name := "Type")
export let Type := Type;
"#;
        write_file(
            temp.path(),
            "crates/musi_foundation/modules/core.ms",
            source,
        );
        let path = temp.path().join("crates/musi_foundation/modules/core.ms");

        let diagnostics = collect_project_diagnostics_with_overlay(&path, Some(source));

        assert!(
            diagnostics.iter().all(|diag| {
                !diag
                    .message
                    .contains(SemaDiagKind::AttrKnownRequiresFoundationModule.message())
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
    fn semantic_tokens_mark_foundation_effect_members_as_functions() {
        let temp = TempDir::new();
        let source = r#"
let Core := import "musi:core";
let Int := Core.Int;
let String := Core.String;

export opaque let Env := effect {
  let envGet (name : String) : String;
  let envHas (name : String) : Int;
  let envSet (name : String, value : String) : Int;
};
"#;
        write_file(temp.path(), "crates/musi_foundation/modules/env.ms", source);
        let path = temp.path().join("crates/musi_foundation/modules/env.ms");

        let tokens = semantic_tokens_for_project_file_with_overlay(&path, Some(source));

        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Function
                && token.range.start_line == 7
                && token.range.start_col == 7
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Function
                && token.range.start_line == 8
                && token.range.start_col == 7
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Function
                && token.range.start_line == 9
                && token.range.start_col == 7
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Parameter
                && token.range.start_line == 7
                && token.range.start_col == 15
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Parameter
                && token.range.start_line == 8
                && token.range.start_col == 15
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Parameter
                && token.range.start_line == 9
                && token.range.start_col == 30
        }));
        assert!(!tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && matches!(token.range.start_line, 7..=9)
                && matches!(token.range.start_col, 15 | 30)
        }));
    }

    #[test]
    fn semantic_tokens_mark_foundation_builtin_return_annotations_as_types() {
        let temp = TempDir::new();
        let source = "\
let Core := import \"musi:core\";
let Int := Core.Int;
let String := Core.String;
let Float := Core.Float;
let Unit := Core.Unit;

export opaque let Env := effect {
  let bool () : Int;
  let float01 () : Float;
};

export let float01 () : Float := ask Env.float01();
";
        write_file(temp.path(), "crates/musi_foundation/modules/env.ms", source);
        let path = temp.path().join("crates/musi_foundation/modules/env.ms");

        let tokens = semantic_tokens_for_project_file_with_overlay(&path, Some(source));

        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 9
                && token.range.start_col == 20
        }));
        assert!(tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Type
                && token.range.start_line == 12
                && token.range.start_col == 25
        }));
        assert!(!tokens.iter().any(|token| {
            token.kind == ToolSemanticTokenKind::Variable
                && matches!(token.range.start_line, 9 | 12)
                && matches!(token.range.start_col, 20 | 25)
        }));
    }

    #[test]
    fn semantic_tokens_mark_foundation_builtin_rhs_names_as_types() {
        let temp = TempDir::new();
        let source = "\
let Intrinsics := import \"musi:intrinsics\";
@musi.known(name := \"Type\")
export let Type := Type;
@musi.known(name := \"Float\")
export let Float := Float;
";
        write_file(
            temp.path(),
            "crates/musi_foundation/modules/core.ms",
            source,
        );
        let path = temp.path().join("crates/musi_foundation/modules/core.ms");

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
        let temp = TempDir::new();
        let source = "\
let Intrinsics := import \"musi:intrinsics\";
@musi.known(name := \"Type\")
export let Type := Type;
";
        write_file(
            temp.path(),
            "crates/musi_foundation/modules/core.ms",
            source,
        );
        let path = temp.path().join("crates/musi_foundation/modules/core.ms");

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
        let temp = TempDir::new();
        let source = "\
let Core := import \"musi:core\";
let Int := Core.Int;
let String := Core.String;
let Float := Core.Float;

export opaque let Env := effect {
  let float01 () : Float;
};
";
        write_file(temp.path(), "crates/musi_foundation/modules/env.ms", source);
        let path = temp.path().join("crates/musi_foundation/modules/env.ms");

        let hover = hover_for_project_file_with_overlay(&path, Some(source), 7, 20)
            .expect("return annotation should hover");

        assert!(
            hover.contents.starts_with("```musi\n(type) Float"),
            "{}",
            hover.contents
        );
    }

    #[test]
    fn inlay_hints_include_parameter_names_and_inferred_types() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        write_file(
            temp.path(),
            "index.ms",
            "let add (left : Int, right : Int) : Int := left + right;\nlet result := add(1, 2);\n",
        );

        let hints = inlay_hints_for_project_file_with_overlay(
            &temp.path().join("index.ms"),
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
}

mod failure {
    use super::*;

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

        let err = load_direct_graph(&temp.path().join("main.ms"))
            .expect_err("package import should fail");
        assert!(matches!(
            err,
            ToolingError::PackageImportRequiresMusi { .. }
        ));
        assert_eq!(err.diag_code().expect("tooling diag code").raw(), 5101);
    }

    #[test]
    fn project_diagnostics_use_file_paths_instead_of_module_keys() {
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        let source = "missing;\n";
        write_file(temp.path(), "index.ms", source);

        let diagnostics =
            collect_project_diagnostics_with_overlay(&temp.path().join("index.ms"), Some(source));

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
    fn session_error_report_carries_sema_hint() {
        assert_session_error_report(
            "let x := 1; ask x;",
            "sema",
            SemaDiagKind::InvalidRequestTarget.message(),
            SemaDiagKind::InvalidRequestTarget.label(),
            Some("write `ask Effect.op(...)`"),
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
        let temp = TempDir::new();
        write_file(
            temp.path(),
            "musi.json",
            r#"{
  "exports": {
    "bad": "./index.ms"
  }
}"#,
        );

        let error = load_project_error(temp.path());
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
        let temp = TempDir::new();
        write_file(temp.path(), "musi.json", APP_MANIFEST);
        write_file(
            temp.path(),
            "index.ms",
            "let Missing := import \"missing\";\nexport let result : Int := 42;\n",
        );

        let error = load_project_error(temp.path());
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
