use std::env;
use std::fs;
use std::path::PathBuf;
use std::slice;
use std::sync::atomic::{AtomicU64, Ordering};

use music_syntax::{Lexer, TokenKind, parse};

use super::*;

static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

fn options() -> FormatOptions {
    FormatOptions::default()
}

fn temp_dir() -> PathBuf {
    let id = TEMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    let path = env::temp_dir().join(format!("musi-fmt-test-{id}"));
    if path.exists() {
        fs::remove_dir_all(&path).unwrap();
    }
    fs::create_dir_all(&path).unwrap();
    path
}

fn token_sequence(text: &str) -> Vec<(TokenKind, String)> {
    let lexed = Lexer::new(text).lex();
    lexed
        .tokens()
        .iter()
        .enumerate()
        .filter(|(_, token)| token.kind != TokenKind::Eof)
        .map(|(index, token)| {
            (
                token.kind,
                lexed.token_text(index).unwrap_or_default().to_owned(),
            )
        })
        .collect()
}

fn assert_format_preserves_tokens(source: &str) {
    let result = format_source(source, &options()).unwrap();
    assert_eq!(token_sequence(source), token_sequence(&result.text));
    assert_formatted_text_is_stable(&result.text);
}

fn assert_format_is_stable(source: &str) {
    let result = format_source(source, &options()).unwrap();
    assert_formatted_text_is_stable(&result.text);
}

fn assert_formatted_text_is_stable(text: &str) {
    let formatted = Lexer::new(text).lex();
    assert!(formatted.errors().is_empty(), "{:?}", formatted.errors());
    let parsed = parse(formatted);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
    let second = format_source(text, &options()).unwrap();
    assert_eq!(second.text, text);
}

fn collect_musi_files(root: PathBuf, out: &mut Vec<PathBuf>) {
    for entry in fs::read_dir(root).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            collect_musi_files(path, out);
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("ms") {
            out.push(path);
        }
    }
}

mod success {
    use super::*;

    #[test]
    fn formats_basic_let_and_binary_spacing() {
        let result =
            format_source("let add(left:Int,right:Int):Int:=left+right;", &options()).unwrap();

        assert_eq!(
            result.text,
            "let add (left : Int, right : Int) : Int := left + right;\n"
        );
        assert!(result.changed);
    }

    #[test]
    fn indents_blocks_with_two_spaces() {
        let result = format_source("let X:=data{| A| B};", &options()).unwrap();

        assert_eq!(result.text, "let X := data {\n  | A\n  | B\n};\n");
    }

    #[test]
    fn keeps_semicolons_mandatory() {
        let result = format_source("let x:=1;", &options()).unwrap();

        assert_eq!(result.text, "let x := 1;\n");
    }

    #[test]
    fn wraps_regular_call_arguments_when_line_exceeds_width() {
        let mut options = options();
        options.line_width = 40;
        let source =
            "let value := foo(aaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbbbb);";

        let result = format_source(source, &options).unwrap();

        assert_eq!(
            result.text,
            "let value := foo(aaaaaaaaaaaaaaaaaaaaaaaaaaaa,\n  bbbbbbbbbbbbbbbbbbbbbbbbbbbb\n);\n"
        );
        let second = format_source(&result.text, &options).unwrap();
        assert_eq!(second.text, result.text);
    }

    #[test]
    fn wraps_call_arguments_before_exceeding_default_width() {
        let source = "let value := foo(aaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbbbb, cccccccccccccccccccccccccccc);";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "let value := foo(aaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbbbbbbbbbbbbbbbbbbbbbbbbbbb,\n  cccccccccccccccccccccccccccc\n);\n"
        );
        assert!(
            result
                .text
                .lines()
                .all(|line| line.chars().count() <= options().line_width)
        );
        let second = format_source(&result.text, &options()).unwrap();
        assert_eq!(second.text, result.text);
    }

    #[test]
    fn keeps_call_arguments_on_one_line_when_they_fit_width() {
        let source = "let ok := testing.it(\"adds values\", testing.toBeTruthy(add(1, 2)));";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "let ok := testing.it(\"adds values\", testing.toBeTruthy(add(1, 2)));\n"
        );
        assert!(
            result
                .text
                .lines()
                .all(|line| line.chars().count() <= options().line_width)
        );
    }

    #[test]
    fn keeps_function_parameters_on_one_line_when_they_fit_width() {
        let source =
            "let add (left : Int, right : Int, carry : Int) : Int := left + right + carry;";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "let add (left : Int, right : Int, carry : Int) : Int := left + right + carry;\n"
        );
    }

    #[test]
    fn respects_custom_line_width_for_call_arguments() {
        let mut options = options();
        options.line_width = 48;
        let source = "let value := foo(short, mediumLengthName, anotherMediumLengthName);";

        let result = format_source(source, &options).unwrap();

        assert_eq!(
            result.text,
            "let value := foo(short, mediumLengthName,\n  anotherMediumLengthName\n);\n"
        );
        assert!(
            result
                .text
                .lines()
                .all(|line| line.chars().count() <= options.line_width)
        );
    }

    #[test]
    fn keeps_attribute_attached_on_own_line_before_foreign() {
        let source = "@link(symbol := \"data.tag\")\nforeign \"musi\" let levelTagIntrinsic (level : Level) : Int;";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "@link(symbol := \"data.tag\")\nforeign \"musi\" let levelTagIntrinsic (level : Level) : Int;\n"
        );
        let second = format_source(&result.text, &options()).unwrap();
        assert_eq!(second.text, result.text);
    }

    #[test]
    fn formats_multiple_attributes_as_attached_lines() {
        let source = "@when(os := \"linux\") @link(name := \"c\") foreign \"c\" let puts (msg : CString) : Int;";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "@when(os := \"linux\")\n@link(name := \"c\")\nforeign \"c\" let puts (msg : CString) : Int;\n"
        );
    }

    #[test]
    fn removes_blank_line_between_doc_comment_and_let() {
        let source = "--- doc comment\n\nlet item := value;";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(result.text, "--- doc comment\nlet item := value;\n");
    }

    #[test]
    fn removes_blank_line_between_doc_comment_and_export() {
        let source = "--- doc comment\n\nexport let item := value;";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(result.text, "--- doc comment\nexport let item := value;\n");
    }

    #[test]
    fn keeps_doc_attribute_and_declaration_attached() {
        let source = "--- doc comment\n\n@tag\n\nlet item := value;";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(result.text, "--- doc comment\n@tag\nlet item := value;\n");
    }

    #[test]
    fn keeps_block_doc_attached_to_declaration() {
        let source = format!(
            "{}-- doc comment -/\n\nlet item := value;",
            char::from(b'/')
        );

        let result = format_source(&source, &options()).unwrap();

        let expected = format!(
            "{}-- doc comment -/\nlet item := value;\n",
            char::from(b'/')
        );
        assert_eq!(result.text, expected);
    }

    #[test]
    fn keeps_module_docs_separate_from_declaration() {
        let source = "--! module docs\n\n/-! block module docs -/\n\nlet item := value;";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "--! module docs\n\n/-! block module docs -/\n\nlet item := value;\n"
        );
    }

    #[test]
    fn sorts_static_top_level_imports_by_specifier() {
        let source = r#"let z := import "@std/testing";
let a := import "@std/io";
let local := import "./local";
"#;

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            r#"let local := import "./local";
let a := import "@std/io";
let z := import "@std/testing";
"#
        );
    }

    #[test]
    fn sorts_plain_static_imports() {
        let source = r#"import "@std/testing";
import "@std/io";
"#;

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            r#"import "@std/io";
import "@std/testing";
"#
        );
    }

    #[test]
    fn sorts_import_destructure_fields() {
        let source = r#"let { writeLine, readText, append } := import "@std/io";"#;

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "let {\n  append, readText, writeLine\n} := import \"@std/io\";\n"
        );
    }

    #[test]
    fn sorts_aliased_import_destructure_fields_by_imported_name() {
        let source = r#"let { writeLine: line, append, readText: read } := import "@std/io";"#;

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "let {\n  append, readText : read, writeLine : line\n} := import \"@std/io\";\n"
        );
    }

    #[test]
    fn does_not_sort_dynamic_or_exported_imports() {
        let source = r#"let z := import path;
let a := import "@std/io";
export let b := import "@std/testing";
"#;

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            r#"let z := import path;
let a := import "@std/io";

export let b := import "@std/testing";
"#
        );
    }

    #[test]
    fn attached_import_comments_move_with_sorted_imports() {
        let source = r#"-- testing helpers
let testing := import "@std/testing";
-- io helpers
let io := import "@std/io";
"#;

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            r#"-- io helpers
let io := import "@std/io";
-- testing helpers
let testing := import "@std/testing";
"#
        );
    }

    #[test]
    fn standalone_comments_split_import_sort_groups() {
        let source = r#"let testing := import "@std/testing";

-- local group
let io := import "@std/io";
"#;

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            r#"let testing := import "@std/testing";

-- local group
let io := import "@std/io";
"#
        );
    }

    #[test]
    fn markdown_fences_sort_imports() {
        let markdown = "# Example\n\n```musi\nlet testing:=import \"@std/testing\";\nlet io:=import \"@std/io\";\n```\n";

        let result = format_markdown(markdown, &options()).unwrap();

        assert_eq!(
            result.text,
            "# Example\n\n```musi\nlet io := import \"@std/io\";\nlet testing := import \"@std/testing\";\n```\n"
        );
    }

    #[test]
    fn preserves_single_top_level_blank_line_between_statements() {
        let source =
            "let io := import \"@std/io\";\n\nlet message := \"Hello\";\nio.writeLine(message);\n";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "let io := import \"@std/io\";\n\nlet message := \"Hello\";\nio.writeLine(message);\n"
        );
    }

    #[test]
    fn collapses_repeated_top_level_blank_lines() {
        let source = "let io := import \"@std/io\";\n\n\n\nlet message := \"Hello\";\n";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            "let io := import \"@std/io\";\n\nlet message := \"Hello\";\n"
        );
    }

    #[test]
    fn removes_blank_lines_inside_sequence_expr() {
        let source = "let value := (\n  1;\n\n  2\n);\n";

        let result = format_source(source, &options()).unwrap();

        assert_eq!(result.text, "let value :=\n  (\n    1;\n    2\n  );\n");
    }

    #[test]
    fn ignore_file_preserves_text_with_final_newline() {
        let result = format_source("-- musi-fmt-ignore-file\nlet   x:=1;", &options()).unwrap();

        assert_eq!(result.text, "-- musi-fmt-ignore-file\nlet   x:=1;\n");
    }

    #[test]
    fn ignore_preserves_next_source_line() {
        let result =
            format_source("-- musi-fmt-ignore\nlet   x:=1;\nlet y:=2;", &options()).unwrap();

        assert_eq!(
            result.text,
            "-- musi-fmt-ignore\nlet   x:=1;\nlet y := 2;\n"
        );
    }

    #[test]
    fn ignore_range_preserves_source_lines() {
        let result = format_source(
            "-- musi-fmt-ignore-start\nlet   x:=1;\n-- musi-fmt-ignore-end\nlet y:=2;",
            &options(),
        )
        .unwrap();

        assert_eq!(
            result.text,
            "-- musi-fmt-ignore-start\nlet   x:=1;\n-- musi-fmt-ignore-end\nlet y := 2;\n"
        );
    }

    #[test]
    fn formats_musi_markdown_fences() {
        let markdown = "# Example\n\n```musi\nlet x:=1;\n```\n\n```ts\nlet x=1\n```\n";
        let result = format_markdown(markdown, &options()).unwrap();

        assert_eq!(
            result.text,
            "# Example\n\n```musi\nlet x := 1;\n```\n\n```ts\nlet x=1\n```\n"
        );
    }

    #[test]
    fn markdown_ignore_skips_next_musi_fence() {
        let markdown = "<!-- musi-fmt-ignore -->\n```musi\nlet x:=1;\n```\n";
        let result = format_markdown(markdown, &options()).unwrap();

        assert_eq!(
            result.text,
            "<!-- musi-fmt-ignore -->\n```musi\nlet x:=1;\n```\n"
        );
    }

    #[test]
    fn format_file_writes_changed_file() {
        let root = temp_dir();
        let path = root.join("index.ms");
        fs::write(&path, "let x:=1;").unwrap();

        let change = format_file(&path, &options(), false).unwrap();

        assert!(change.changed);
        assert_eq!(fs::read_to_string(path).unwrap(), "let x := 1;\n");
    }

    #[test]
    fn format_paths_respects_include_and_exclude() {
        let root = temp_dir();
        fs::create_dir_all(root.join("src")).unwrap();
        fs::write(root.join("src/main.ms"), "let x:=1;").unwrap();
        fs::write(root.join("src/readme.md"), "```musi\nlet z:=3;\n```\n").unwrap();
        fs::write(root.join("src/skip.ms"), "let y:=2;").unwrap();
        let mut options = options();
        options.include = vec!["src/**".to_owned()];
        options.exclude = vec!["src/skip.ms".to_owned()];

        let summary = format_paths(slice::from_ref(&root), &root, &options, false).unwrap();

        assert_eq!(summary.files.len(), 2);
        assert_eq!(
            fs::read_to_string(root.join("src/main.ms")).unwrap(),
            "let x := 1;\n"
        );
        assert_eq!(
            fs::read_to_string(root.join("src/readme.md")).unwrap(),
            "```musi\nlet z := 3;\n```\n"
        );
        assert_eq!(
            fs::read_to_string(root.join("src/skip.ms")).unwrap(),
            "let y:=2;"
        );
    }

    #[test]
    fn formats_extensionless_file_with_assumed_extension() {
        let root = temp_dir();
        let path = root.join("script");
        fs::write(&path, "let x:=1;").unwrap();
        let mut options = options();
        options.assume_extension = Some(FormatInputKind::Musi);

        let summary = format_paths(slice::from_ref(&path), &root, &options, false).unwrap();

        assert_eq!(summary.files.len(), 1);
        assert_eq!(fs::read_to_string(path).unwrap(), "let x := 1;\n");
    }

    #[test]
    fn preserves_multiline_test_sequence_regression() {
        let source = r#"let testing := import "@std/testing";
let array := import "./index.ms";

export let test () :=
  (
    testing.describe("array");
    testing.it("'copy' clones sequence values", testing.toBeTruthy(array.equalsInt(array.copy[Int]([1, 2, 3]), [1, 2, 3])));
    testing.it("'concat' joins two sequences", testing.toBeTruthy(array.equalsInt(array.concat[Int]([1, 2], [3, 4]), [1, 2, 3, 4])));
    testing.it("'append' adds trailing value", testing.toBeTruthy(array.equalsInt(array.append[Int]([1, 2], 3), [1, 2, 3])));
    testing.it("'prepend' adds leading value", testing.toBeTruthy(array.equalsInt(array.prepend[Int](0, [1, 2]), [0, 1, 2])));
    testing.it("'isEmpty' detects empty arrays", testing.toBeTruthy(array.isEmpty[Int]([])));
    testing.it("'nonEmpty' detects non-empty arrays", testing.toBeTruthy(array.nonEmpty[Int]([1])));
    testing.endDescribe()
  );
"#;

        let result = format_source(source, &options()).unwrap();

        assert_eq!(
            result.text,
            r#"let array := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("array");
    testing.it("'copy' clones sequence values",
      testing.toBeTruthy(array.equalsInt(array.copy[Int]([1, 2, 3]), [1, 2, 3]))
    );
    testing.it("'concat' joins two sequences",
      testing.toBeTruthy(array.equalsInt(array.concat[Int]([1, 2], [3, 4]),
          [1, 2, 3, 4]
        ))
    );
    testing.it("'append' adds trailing value",
      testing.toBeTruthy(array.equalsInt(array.append[Int]([1, 2], 3),
          [1, 2, 3]
        ))
    );
    testing.it("'prepend' adds leading value",
      testing.toBeTruthy(array.equalsInt(array.prepend[Int](0, [1, 2]),
          [0, 1, 2]
        ))
    );
    testing.it("'isEmpty' detects empty arrays",
      testing.toBeTruthy(array.isEmpty[Int]([]))
    );
    testing.it("'nonEmpty' detects non-empty arrays",
      testing.toBeTruthy(array.nonEmpty[Int]([1]))
    );
    testing.endDescribe()
  );
"#
        );
        assert!(
            result
                .text
                .lines()
                .all(|line| line.chars().count() <= options().line_width)
        );
    }

    #[test]
    fn formats_constructs_without_changing_tokens() {
        const SOURCES: &[&str] = &[
            "export let toString  (self : Command) : String := match self (\n| .Command(value := value) => value\n);",
            "export let chance (percent : Int) : Bool := match () (\n| _ if percent <= 0 => 0 = 1\n| _ if percent >= 100 => 0 = 0\n| _ => nextIntInRange(0, 100) < percent\n);",
            "export let command (value : String) : Command := .Command(value := value);",
            "export let values : []Int := [1, 2, 3];",
            "export let cast [T] (raw : CPtr) : Ptr[T] := .Ptr(raw := raw);",
            "export foreign \"musi\" (\nlet offset[T] (pointer : Ptr[T], count : Int) : Ptr[T];\nlet read[T] (pointer : Ptr[T]) : T;\n);",
            "--- Documented value.\nexport let x : Int := 1;",
            "let x := 1; -- trailing\nlet y := /- inline -/ 2;",
        ];

        for source in SOURCES {
            assert_format_preserves_tokens(source);
        }
    }

    #[test]
    fn formats_repository_musi_corpus_idempotently() {
        let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .canonicalize()
            .unwrap();
        let mut files = Vec::new();
        collect_musi_files(repo.join("packages/std"), &mut files);
        collect_musi_files(repo.join("crates/musi_foundation/modules"), &mut files);

        for path in files {
            let source = fs::read_to_string(&path).unwrap();
            assert_format_is_stable(&source);
        }
    }
}

mod failure {
    use super::*;

    #[test]
    fn syntax_errors_fail_without_formatting() {
        let error = format_source("let := 1;", &options()).unwrap_err();

        assert!(matches!(error, FormatError::SyntaxErrors));
    }

    #[test]
    fn check_mode_does_not_write_changed_file() {
        let root = temp_dir();
        let path = root.join("index.ms");
        fs::write(&path, "let x:=1;").unwrap();

        let change = format_file(&path, &options(), true).unwrap();

        assert!(change.changed);
        assert_eq!(fs::read_to_string(path).unwrap(), "let x:=1;");
    }
}
