use std::env::temp_dir;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use async_lsp::lsp_types::{
    CompletionItemKind, CompletionTextEdit, DiagnosticSeverity, InlayHintKind, PartialResultParams,
    Position, SemanticToken, TextDocumentIdentifier, TextDocumentPositionParams,
    WorkDoneProgressParams,
};
use musi_tooling::{
    CliDiagnostic, CliDiagnosticLabel, CliDiagnosticRange, ToolInlayHint, ToolInlayHintKind,
    ToolPosition, ToolRange, ToolSemanticModifier, ToolSemanticToken, ToolSemanticTokenKind,
};

use super::convert::{
    default_range, diagnostic_matches_path, to_cli_range, to_lsp_diagnostic, to_lsp_inlay_hint,
    to_severity, truncate_hover_contents,
};
use super::*;

static NEXT_TEMP_ID: AtomicU64 = AtomicU64::new(0);

fn temp_project() -> PathBuf {
    let id = NEXT_TEMP_ID.fetch_add(1, Ordering::Relaxed);
    let path = temp_dir().join(format!("musi-lsp-test-{id}"));
    if path.exists() {
        fs::remove_dir_all(&path).expect("stale temp project should be removed");
    }
    fs::create_dir_all(&path).expect("temp project should be created");
    path
}

mod success {
    use super::*;

    #[test]
    fn initialize_result_advertises_full_sync_and_hover() {
        let result = MusiLanguageServer::initialize_result();

        assert_eq!(result.server_info.expect("server info").name, "musi_lsp");
        assert_eq!(
            result.capabilities.text_document_sync,
            Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL))
        );
        assert_eq!(
            result.capabilities.hover_provider,
            Some(HoverProviderCapability::Simple(true))
        );
        assert_eq!(
            result.capabilities.document_formatting_provider,
            Some(OneOf::Left(true))
        );
        assert!(result.capabilities.semantic_tokens_provider.is_some());
        assert!(result.capabilities.inlay_hint_provider.is_some());
        assert!(result.capabilities.completion_provider.is_some());
        assert_eq!(
            result.capabilities.definition_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(
            result.capabilities.references_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(
            result.capabilities.document_symbol_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(
            result.capabilities.workspace_symbol_provider,
            Some(OneOf::Left(true))
        );
        assert!(result.capabilities.rename_provider.is_some());
    }

    #[test]
    fn full_document_range_covers_complete_text() {
        assert_eq!(
            full_document_range("one\ntwo"),
            Range {
                start: Position::new(0, 0),
                end: Position::new(1, 3),
            }
        );
    }

    #[test]
    fn formatting_options_override_manifest_indentation() {
        let mut options = FormatOptions {
            use_tabs: true,
            indent_width: 8,
            ..FormatOptions::default()
        };

        apply_document_formatting_options(
            &mut options,
            &FormattingOptions {
                tab_size: 4,
                insert_spaces: true,
                ..FormattingOptions::default()
            },
        );

        assert!(!options.use_tabs);
        assert_eq!(options.indent_width, 4);
    }

    #[test]
    fn document_formatting_formats_multiline_match_like_cli_formatter() {
        let uri = Url::parse("file:///tmp/index.ms").expect("uri should parse");
        let source = r"export let isLess (target : Ordering) : Bool := match target(
    | .Less => 0 = 0
    | _ => 0 = 1);
";
        let expected = r"export let isLess (target : Ordering) : Bool :=
  match target (
  | .Less => 0 = 0
  | _ => 0 = 1
  );
";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let edits = server
            .document_formatting(DocumentFormattingParams {
                text_document: TextDocumentIdentifier { uri },
                options: FormattingOptions {
                    tab_size: 2,
                    insert_spaces: true,
                    ..FormattingOptions::default()
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("formatting should run");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, expected);
    }

    #[test]
    fn document_formatting_uses_manifest_profile_and_overrides() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"profile\": \"expanded\", \"matchArmIndent\": \"pipeAligned\" }\n}\n",
        )
        .expect("manifest should be written");
        let path = root.join("index.ms");
        fs::write(&path, "export let result : Int := 1;").expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "export let describe (target : Ordering) : String := match target(| .Less => \"less\" | .GreaterThanEverything => \"greater\" | _ => \"same\");";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let edits = server
            .document_formatting(DocumentFormattingParams {
                text_document: TextDocumentIdentifier { uri },
                options: FormattingOptions {
                    tab_size: 2,
                    insert_spaces: true,
                    ..FormattingOptions::default()
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("formatting should run");

        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].new_text,
            "export let describe (\n  target : Ordering\n) : String :=\n  match target (\n  | .Less                  => \"less\"\n  | .GreaterThanEverything => \"greater\"\n  | _                      => \"same\"\n  );\n"
        );
    }

    #[test]
    fn completion_returns_keywords_and_current_bindings() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "index.ms"
}
"#,
        )
        .expect("manifest should be written");
        let path = root.join("index.ms");
        let source = r"let before := 1;
let current := bef;
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let response = server
            .completions(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 18),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .expect("completion response should exist");
        assert!(matches!(response, CompletionResponse::List(_)));
        let items = match response {
            CompletionResponse::List(list) => {
                assert!(!list.is_incomplete);
                list.items
            }
            CompletionResponse::Array(items) => items,
        };
        let before = items
            .iter()
            .find(|item| item.label == "before")
            .expect("before completion should exist");

        assert!(items.iter().any(|item| item.label == "let"));
        assert_eq!(before.kind, Some(CompletionItemKind::VARIABLE));
        let edit = before
            .text_edit
            .as_ref()
            .and_then(|edit| match edit {
                CompletionTextEdit::Edit(edit) => Some(edit),
                CompletionTextEdit::InsertAndReplace(_) => None,
            })
            .expect("completion should provide replacement edit");
        assert_eq!(edit.range.start, Position::new(1, 15));
        assert_eq!(edit.range.end, Position::new(1, 18));
        assert_eq!(edit.new_text, "before");
    }

    #[test]
    fn completion_after_dot_returns_member_items() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "index.ms"
}
"#,
        )
        .expect("manifest should be written");
        let path = root.join("index.ms");
        let source = r"let point := { x := 1, y := 2 };
point.
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let response = server
            .completions(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 6),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .expect("completion response should exist");
        assert!(matches!(response, CompletionResponse::List(_)));
        let items = match response {
            CompletionResponse::List(list) => {
                assert!(!list.is_incomplete);
                list.items
            }
            CompletionResponse::Array(items) => items,
        };

        assert!(items.iter().any(|item| item.label == "x"));
        assert!(items.iter().any(|item| item.label == "y"));
        assert!(!items.iter().any(|item| item.label == "let"));
        assert!(
            items
                .iter()
                .all(|item| item.kind == Some(CompletionItemKind::PROPERTY))
        );
    }

    #[test]
    fn did_save_document_is_handled_without_routing_fallback() {
        let uri = Url::parse("file:///tmp/index.ms").expect("uri should parse");
        let params = DidSaveTextDocumentParams {
            text_document: TextDocumentIdentifier { uri },
            text: None,
        };
        let handler: fn(&mut MusiLanguageServer, DidSaveTextDocumentParams) -> NotifyResult =
            <MusiLanguageServer as LanguageServer>::did_save;

        let _ = params;
        let _ = handler;
    }

    #[test]
    fn cli_range_is_zero_based_lsp_range() {
        let range = to_cli_range(&CliDiagnosticRange {
            start_line: 3,
            start_col: 5,
            end_line: 3,
            end_col: 8,
        });

        assert_eq!(range.start, Position::new(2, 4));
        assert_eq!(range.end, Position::new(2, 7));
    }

    #[test]
    fn tool_range_is_zero_based_lsp_range() {
        let range = to_tool_range(&ToolRange::new(2, 3, 2, 8));

        assert_eq!(range.start, Position::new(1, 2));
        assert_eq!(range.end, Position::new(1, 7));
    }

    #[test]
    fn semantic_token_encoding_uses_relative_positions() {
        let tokens = vec![
            ToolSemanticToken::new(
                ToolRange::new(1, 1, 1, 4),
                ToolSemanticTokenKind::Keyword,
                Vec::new(),
            ),
            ToolSemanticToken::new(
                ToolRange::new(2, 3, 2, 10),
                ToolSemanticTokenKind::Variable,
                vec![
                    ToolSemanticModifier::Declaration,
                    ToolSemanticModifier::Definition,
                ],
            ),
        ];

        assert_eq!(
            encode_semantic_tokens(&tokens, None),
            vec![
                SemanticToken {
                    delta_line: 0,
                    delta_start: 0,
                    length: 3,
                    token_type: 10,
                    token_modifiers_bitset: 0,
                },
                SemanticToken {
                    delta_line: 1,
                    delta_start: 2,
                    length: 7,
                    token_type: 4,
                    token_modifiers_bitset: 0b11,
                },
            ]
        );
    }

    #[test]
    fn diagnostic_matching_normalizes_file_paths() {
        let path = temp_dir().join("project").join("index.ms");
        let dotted = temp_dir().join("project").join(".").join("index.ms");
        let diagnostic = CliDiagnostic::new("sema", "error", "type mismatch")
            .with_file(Some(dotted.display().to_string()));

        assert!(diagnostic_matches_path(Path::new(&path), &diagnostic));
    }

    #[test]
    fn lsp_diagnostic_uses_primary_range_and_related_file_uri() {
        let path = temp_dir().join("project").join("index.ms");
        let path_text = path.display().to_string();
        let diagnostic = CliDiagnostic::new("resolve", "error", "unbound name `missing`")
            .with_file(Some(path_text.clone()))
            .with_range(Some(CliDiagnosticRange {
                start_line: 2,
                start_col: 3,
                end_line: 2,
                end_col: 10,
            }))
            .with_labels(vec![CliDiagnosticLabel::new(
                Some(path_text),
                Some(CliDiagnosticRange {
                    start_line: 2,
                    start_col: 3,
                    end_line: 2,
                    end_col: 10,
                }),
                "unbound name `missing`".to_owned(),
            )]);

        let converted = to_lsp_diagnostic(diagnostic);

        assert_eq!(converted.range.start, Position::new(1, 2));
        assert_eq!(converted.range.end, Position::new(1, 9));
        let related = converted
            .related_information
            .expect("related information should exist");
        assert_eq!(related[0].location.uri.scheme(), "file");
    }

    #[test]
    fn inlay_hint_conversion_uses_lsp_kind_and_padding() {
        let hint = to_lsp_inlay_hint(ToolInlayHint::new(
            ToolPosition::new(2, 5),
            "value:",
            ToolInlayHintKind::Parameter,
        ));

        assert_eq!(hint.position, Position::new(1, 4));
        assert!(matches!(hint.kind, Some(InlayHintKind::PARAMETER)));
        assert_eq!(hint.padding_right, Some(true));
    }

    #[test]
    fn hover_contents_truncate_to_configured_limit() {
        assert_eq!(truncate_hover_contents("abcdef", 3), "abc…");
        assert_eq!(truncate_hover_contents("abc", 3), "abc");
    }
}

mod failure {
    use super::*;

    #[test]
    fn unknown_severity_defaults_to_error() {
        assert_eq!(to_severity("fatal"), DiagnosticSeverity::ERROR);
    }

    #[test]
    fn missing_cli_range_uses_strict_default_range() {
        assert_eq!(
            default_range(),
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, 1),
            }
        );
    }
}
