#![allow(clippy::panic, clippy::too_many_lines)]

use std::env::temp_dir;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use async_lsp::lsp_types::{
    ClientCapabilities, CodeActionContext, CodeActionKind, CodeActionOrCommand, CodeActionParams,
    CodeLensParams, CompletionItemKind, CompletionTextEdit, DeclarationCapability,
    DiagnosticOptions, DiagnosticServerCapabilities, DiagnosticSeverity,
    DidChangeConfigurationParams, DidChangeWorkspaceFoldersParams, DocumentDiagnosticParams,
    DocumentDiagnosticReport, DocumentDiagnosticReportResult, DocumentHighlightKind,
    DocumentLinkParams, DocumentOnTypeFormattingParams, DocumentRangeFormattingParams,
    DocumentSymbolParams, DocumentSymbolResponse, Documentation, ExecuteCommandParams,
    FileOperationPatternKind, FileRename, FoldingRangeKind, FoldingRangeParams,
    GotoDefinitionParams, GotoDefinitionResponse, ImplementationProviderCapability,
    InitializeParams, InlayHintKind, InlayHintLabel, InlayHintParams, InlayHintServerCapabilities,
    InlayHintTooltip, LinkedEditingRangeParams, MonikerKind, PartialResultParams, Position,
    PrepareRenameResponse, ReferenceContext, RenameFilesParams, SelectionRangeParams,
    SemanticToken, SemanticTokensDeltaParams, SemanticTokensFullDeltaResult, SignatureHelpParams,
    SymbolKind, TextDocumentIdentifier, TextDocumentPositionParams, TextDocumentSaveReason,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, TypeDefinitionProviderCapability, WillSaveTextDocumentParams,
    WorkDoneProgressParams, WorkspaceDiagnosticParams, WorkspaceDiagnosticReportResult,
    WorkspaceDocumentDiagnosticReport, WorkspaceFolder, WorkspaceFoldersChangeEvent,
    WorkspaceSymbolParams, WorkspaceSymbolResponse,
};
use musi_tooling::{
    CliDiagnostic, CliDiagnosticLabel, CliDiagnosticRange, ToolInlayHint, ToolInlayHintKind,
    ToolPosition, ToolRange, ToolSemanticModifier, ToolSemanticToken, ToolSemanticTokenKind,
};

use super::convert::{
    default_range, diagnostic_matches_path, resolve_lsp_inlay_hint, to_cli_range,
    to_lsp_diagnostic, to_lsp_inlay_hint, to_severity, truncate_hover_contents,
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
    fn initialize_result_advertises_non_blocking_sync_and_hover() {
        let initialize_result = MusiLanguageServer::initialize_result();

        assert_eq!(
            initialize_result.server_info.expect("server info").name,
            "musi_lsp"
        );
        assert_eq!(
            initialize_result.capabilities.text_document_sync,
            Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    will_save: None,
                    will_save_wait_until: None,
                    save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                }
            ))
        );
        assert_eq!(
            initialize_result.capabilities.hover_provider,
            Some(HoverProviderCapability::Simple(true))
        );
        let signature_help = initialize_result
            .capabilities
            .signature_help_provider
            .expect("signature help provider");
        assert_eq!(
            signature_help.trigger_characters.as_deref(),
            Some(&["(".to_owned(), ",".to_owned()][..])
        );
        assert_eq!(
            signature_help.retrigger_characters.as_deref(),
            Some(&[",".to_owned()][..])
        );
        assert_eq!(
            initialize_result.capabilities.document_formatting_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(
            initialize_result
                .capabilities
                .document_range_formatting_provider,
            Some(OneOf::Left(true))
        );
        let on_type_formatting = initialize_result
            .capabilities
            .document_on_type_formatting_provider
            .expect("on type formatting provider");
        assert_eq!(on_type_formatting.first_trigger_character, ";");
        assert_eq!(
            on_type_formatting.more_trigger_character.as_deref(),
            Some(&[")".to_owned(), "]".to_owned(), "}".to_owned()][..])
        );
        assert!(
            initialize_result
                .capabilities
                .semantic_tokens_provider
                .is_some()
        );
        let inlay_hint = initialize_result
            .capabilities
            .inlay_hint_provider
            .expect("inlay hint provider");
        let OneOf::Right(InlayHintServerCapabilities::Options(inlay_hint)) = inlay_hint else {
            panic!("inlay hint options expected");
        };
        assert_eq!(inlay_hint.resolve_provider, Some(true));
        let completion = initialize_result
            .capabilities
            .completion_provider
            .expect("completion provider");
        assert_eq!(completion.resolve_provider, Some(true));
        assert_eq!(
            completion.trigger_characters.as_deref(),
            Some(
                &[
                    ".".to_owned(),
                    "\"".to_owned(),
                    "/".to_owned(),
                    "@".to_owned()
                ][..]
            )
        );
        assert_eq!(
            initialize_result.capabilities.declaration_provider,
            Some(DeclarationCapability::Simple(true))
        );
        assert_eq!(
            initialize_result.capabilities.definition_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(
            initialize_result.capabilities.type_definition_provider,
            Some(TypeDefinitionProviderCapability::Simple(true))
        );
        assert_eq!(
            initialize_result.capabilities.implementation_provider,
            Some(ImplementationProviderCapability::Simple(true))
        );
        assert_eq!(
            initialize_result.capabilities.references_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(
            initialize_result.capabilities.moniker_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(
            initialize_result.capabilities.call_hierarchy_provider,
            Some(CallHierarchyServerCapability::Simple(true))
        );
        assert!(
            initialize_result
                .capabilities
                .linked_editing_range_provider
                .is_some()
        );
        let execute_command = initialize_result
            .capabilities
            .execute_command_provider
            .expect("execute command provider");
        assert_eq!(execute_command.commands, ["musi.references"]);
        let workspace = initialize_result
            .capabilities
            .workspace
            .expect("workspace capabilities");
        let folders = workspace
            .workspace_folders
            .expect("workspace folder capabilities");
        assert_eq!(folders.supported, Some(true));
        assert_eq!(folders.change_notifications, Some(OneOf::Left(true)));
        let file_operations = workspace
            .file_operations
            .expect("file operation capabilities");
        let will_rename = file_operations
            .will_rename
            .expect("will rename registration");
        assert_eq!(will_rename.filters.len(), 2);
        assert_eq!(will_rename.filters[0].scheme.as_deref(), Some("file"));
        assert_eq!(will_rename.filters[0].pattern.glob, "**/*.ms");
        assert_eq!(
            will_rename.filters[0].pattern.matches,
            Some(FileOperationPatternKind::File)
        );
        assert_eq!(will_rename.filters[1].scheme.as_deref(), Some("file"));
        assert_eq!(will_rename.filters[1].pattern.glob, "**");
        assert_eq!(
            will_rename.filters[1].pattern.matches,
            Some(FileOperationPatternKind::Folder)
        );
        assert_eq!(
            initialize_result.capabilities.document_highlight_provider,
            Some(OneOf::Left(true))
        );
        assert_eq!(
            initialize_result.capabilities.document_symbol_provider,
            Some(OneOf::Left(true))
        );
        let document_link = initialize_result
            .capabilities
            .document_link_provider
            .expect("document link provider");
        assert_eq!(document_link.resolve_provider, Some(true));
        let code_lens = initialize_result
            .capabilities
            .code_lens_provider
            .expect("code lens provider");
        assert_eq!(code_lens.resolve_provider, Some(true));
        assert_eq!(
            initialize_result.capabilities.diagnostic_provider,
            Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
                identifier: Some("musi".to_owned()),
                inter_file_dependencies: true,
                workspace_diagnostics: false,
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            }))
        );
        assert!(
            initialize_result
                .capabilities
                .folding_range_provider
                .is_some()
        );
        assert!(
            initialize_result
                .capabilities
                .selection_range_provider
                .is_some()
        );
        let code_action = initialize_result
            .capabilities
            .code_action_provider
            .expect("code action provider");
        let CodeActionProviderCapability::Options(code_action) = code_action else {
            panic!("code action options expected");
        };
        assert_eq!(code_action.resolve_provider, Some(true));
        let workspace_symbol = initialize_result
            .capabilities
            .workspace_symbol_provider
            .expect("workspace symbol provider");
        let OneOf::Right(workspace_symbol) = workspace_symbol else {
            panic!("workspace symbol options expected");
        };
        assert_eq!(workspace_symbol.resolve_provider, Some(true));
        assert!(initialize_result.capabilities.rename_provider.is_some());
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
    fn full_document_range_uses_utf16_characters() {
        assert_eq!(
            full_document_range("let icon := \"\u{1F600}\";"),
            Range {
                start: Position::new(0, 0),
                end: Position::new(0, 17),
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
        let source = r"export let isLess (target : Ordering) : Bit := match target(
    | .Less => 0 = 0
    | _ => 0 = 1);
";
        let expected = r"export let isLess (target : Ordering) : Bit :=
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
    fn range_formatting_uses_utf16_positions() {
        let uri = Url::parse("file:///tmp/index.ms").expect("uri should parse");
        let prefix = "let icon := \"\u{1F600}\"; ";
        let source = format!("{prefix}let x:=1;");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source);

        let edits = server
            .document_range_formatting(DocumentRangeFormattingParams {
                text_document: TextDocumentIdentifier { uri },
                range: Range {
                    start: Position::new(0, 18),
                    end: Position::new(0, 27),
                },
                options: FormattingOptions {
                    tab_size: 2,
                    insert_spaces: true,
                    ..FormattingOptions::default()
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("range formatting should run");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "let x := 1;\n");
    }

    #[test]
    fn will_save_wait_until_formats_open_document() {
        let uri = Url::parse("file:///tmp/index.ms").expect("uri should parse");
        let source = "let x:=1;";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let edits = server
            .will_save_formatting(WillSaveTextDocumentParams {
                text_document: TextDocumentIdentifier { uri },
                reason: TextDocumentSaveReason::MANUAL,
            })
            .expect("will save formatting should run");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].range, full_document_range(source));
        assert_eq!(edits[0].new_text, "let x := 1;\n");
    }

    #[test]
    fn will_save_notification_is_accepted() {
        let uri = Url::parse("file:///tmp/index.ms").expect("uri should parse");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());

        assert!(matches!(
            server.will_save(WillSaveTextDocumentParams {
                text_document: TextDocumentIdentifier { uri },
                reason: TextDocumentSaveReason::MANUAL,
            }),
            ControlFlow::Continue(())
        ));
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
    fn document_formatting_formats_musi_fences_in_markdown_documents() {
        let root = temp_project();
        let path = root.join("README.md");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "# Example\n\n```musi\nlet testing:=import \"@std/testing\";\nlet io:=import \"@std/io\";\n```\n";
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
            "# Example\n\n```musi\nlet io := import \"@std/io\";\nlet testing := import \"@std/testing\";\n```\n"
        );
    }

    #[test]
    fn document_range_formatting_formats_selected_source_range() {
        let uri = Url::parse("file:///tmp/index.ms").expect("uri should parse");
        let source = r#"let testing:=import "@std/testing";
let io:=import "@std/io";
let value:=1;
"#;
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let edits = server
            .document_range_formatting(DocumentRangeFormattingParams {
                text_document: TextDocumentIdentifier { uri },
                range: Range {
                    start: Position::new(0, 0),
                    end: Position::new(2, 0),
                },
                options: FormattingOptions {
                    tab_size: 2,
                    insert_spaces: true,
                    ..FormattingOptions::default()
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("range formatting should run");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].range.start, Position::new(0, 0));
        assert_eq!(edits[0].range.end, Position::new(2, 0));
        assert_eq!(
            edits[0].new_text,
            "let io := import \"@std/io\";\nlet testing := import \"@std/testing\";\n"
        );
    }

    #[test]
    fn document_range_formatting_formats_selected_musi_fence_body() {
        let root = temp_project();
        let path = root.join("README.md");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "# Example\n\n```musi\nlet x:=1;\n```\n";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let edits = server
            .document_range_formatting(DocumentRangeFormattingParams {
                text_document: TextDocumentIdentifier { uri },
                range: Range {
                    start: Position::new(3, 0),
                    end: Position::new(4, 0),
                },
                options: FormattingOptions {
                    tab_size: 2,
                    insert_spaces: true,
                    ..FormattingOptions::default()
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("range formatting should run");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].range.start, Position::new(3, 0));
        assert_eq!(edits[0].range.end, Position::new(4, 0));
        assert_eq!(edits[0].new_text, "let x := 1;\n");
    }

    #[test]
    fn document_on_type_formatting_formats_current_line_on_trigger() {
        let uri = Url::parse("file:///tmp/index.ms").expect("uri should parse");
        let source = "let x:=1;";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let edits = server
            .document_on_type_formatting(DocumentOnTypeFormattingParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 9),
                },
                ch: ";".to_owned(),
                options: FormattingOptions {
                    tab_size: 2,
                    insert_spaces: true,
                    ..FormattingOptions::default()
                },
            })
            .expect("on type formatting should run");

        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].range, full_document_range(source));
        assert_eq!(edits[0].new_text, "let x := 1;\n");
    }

    #[test]
    fn document_on_type_formatting_edits_only_triggered_line() {
        let uri = Url::parse("file:///tmp/index.ms").expect("uri should parse");
        let source = "let before:=1;\nlet current:=2;\nlet after:=3;\n";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let edits = server
            .document_on_type_formatting(DocumentOnTypeFormattingParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 15),
                },
                ch: ";".to_owned(),
                options: FormattingOptions {
                    tab_size: 2,
                    insert_spaces: true,
                    ..FormattingOptions::default()
                },
            })
            .expect("on type formatting should run");

        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].range,
            Range::new(Position::new(1, 0), Position::new(2, 0))
        );
        assert_eq!(edits[0].new_text, "let current := 2;\n");
    }

    #[test]
    fn document_on_type_formatting_uses_manifest_options() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"lineWidth\": 20 }\n}\n",
        )
        .expect("manifest should be written");
        let path = root.join("index.ms");
        fs::write(&path, "let placeholder := 0;").expect("source file should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "let value := foo(aaaaaaaaaa, bbbbbbbbbb);";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let edits = server
            .document_on_type_formatting(DocumentOnTypeFormattingParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 41),
                },
                ch: ";".to_owned(),
                options: FormattingOptions {
                    tab_size: 2,
                    insert_spaces: true,
                    ..FormattingOptions::default()
                },
            })
            .expect("on type formatting should run");

        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].new_text,
            "let value :=\n  foo(\n    aaaaaaaaaa,\n    bbbbbbbbbb,\n  );\n"
        );
    }

    #[test]
    fn document_formatting_caches_manifest_format_options() {
        let root = temp_project();
        let manifest_path = root.join("musi.json");
        fs::write(
            &manifest_path,
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"lineWidth\": 20 }\n}\n",
        )
        .expect("manifest should be written");
        let path = root.join("index.ms");
        fs::write(&path, "let placeholder := 0;").expect("source file should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "let value := foo(aaaaaaaaaa, bbbbbbbbbb);";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        for _ in 0..2 {
            let edits = server
                .document_on_type_formatting(DocumentOnTypeFormattingParams {
                    text_document_position: TextDocumentPositionParams {
                        text_document: TextDocumentIdentifier { uri: uri.clone() },
                        position: Position::new(0, 41),
                    },
                    ch: ";".to_owned(),
                    options: FormattingOptions {
                        tab_size: 2,
                        insert_spaces: true,
                        ..FormattingOptions::default()
                    },
                })
                .expect("on type formatting should run");
            assert_eq!(edits.len(), 1);
        }

        assert_eq!(server.format_options_cache.len(), 1);
        assert!(server.format_options_cache.contains_key(&manifest_path));
    }

    #[test]
    fn manifest_document_change_invalidates_format_options_cache() {
        let root = temp_project();
        let manifest_path = root.join("musi.json");
        fs::write(
            &manifest_path,
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"lineWidth\": 20 }\n}\n",
        )
        .expect("manifest should be written");
        let path = root.join("index.ms");
        fs::write(&path, "let placeholder := 0;").expect("source file should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "let value := foo(aaaaaaaaaa, bbbbbbbbbb);";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let _ = server.document_on_type_formatting(DocumentOnTypeFormattingParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(0, 41),
            },
            ch: ";".to_owned(),
            options: FormattingOptions {
                tab_size: 2,
                insert_spaces: true,
                ..FormattingOptions::default()
            },
        });

        assert_eq!(server.format_options_cache.len(), 1);
        let manifest_uri = Url::from_file_path(&manifest_path).expect("file URI should build");
        server.invalidate_format_options_cache_if_manifest(&manifest_uri);

        assert!(server.format_options_cache.is_empty());
    }

    #[test]
    fn source_document_change_keeps_format_options_cache() {
        let root = temp_project();
        let manifest_path = root.join("musi.json");
        fs::write(
            &manifest_path,
            "{\n  \"name\": \"app\",\n  \"version\": \"0.1.0\",\n  \"fmt\": { \"lineWidth\": 20 }\n}\n",
        )
        .expect("manifest should be written");
        let path = root.join("index.ms");
        fs::write(&path, "let placeholder := 0;").expect("source file should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "let value := foo(aaaaaaaaaa, bbbbbbbbbb);";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let _ = server.document_on_type_formatting(DocumentOnTypeFormattingParams {
            text_document_position: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position::new(0, 41),
            },
            ch: ";".to_owned(),
            options: FormattingOptions {
                tab_size: 2,
                insert_spaces: true,
                ..FormattingOptions::default()
            },
        });

        assert_eq!(server.format_options_cache.len(), 1);
        server.invalidate_format_options_cache_if_manifest(&uri);

        assert_eq!(server.format_options_cache.len(), 1);
        assert!(server.format_options_cache.contains_key(&manifest_path));
    }

    #[test]
    fn document_on_type_formatting_ignores_non_trigger_characters() {
        let uri = Url::parse("file:///tmp/index.ms").expect("uri should parse");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server
            .open_documents
            .insert(uri.clone(), "let x:=1;".to_owned());

        let edits = server
            .document_on_type_formatting(DocumentOnTypeFormattingParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 5),
                },
                ch: "x".to_owned(),
                options: FormattingOptions {
                    tab_size: 2,
                    insert_spaces: true,
                    ..FormattingOptions::default()
                },
            })
            .expect("on type formatting should run");

        assert!(edits.is_empty());
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
        let source = r"--- before docs
let before := 1;
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
                    position: Position::new(2, 18),
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
        assert_eq!(before.detail, None);
        assert_eq!(before.documentation, None);
        assert!(before.data.is_some());
        let before = MusiLanguageServer::resolve_completion(before.clone());
        assert_eq!(before.detail.as_deref(), Some("binding"));
        assert_eq!(
            before.documentation,
            Some(Documentation::String("before docs".to_owned()))
        );
        let edit = before
            .text_edit
            .as_ref()
            .and_then(|edit| match edit {
                CompletionTextEdit::Edit(edit) => Some(edit),
                CompletionTextEdit::InsertAndReplace(_) => None,
            })
            .expect("completion should provide replacement edit");
        assert_eq!(edit.range.start, Position::new(2, 15));
        assert_eq!(edit.range.end, Position::new(2, 18));
        assert_eq!(edit.new_text, "before");
    }

    #[test]
    fn completion_uses_utf16_positions_for_request_and_edit() {
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
        let source = "let before := 1;\nlet icon := \"\u{1F600}\"; bef;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let response = server
            .completions(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 21),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .expect("completion response should exist");
        let items = match response {
            CompletionResponse::List(list) => list.items,
            CompletionResponse::Array(items) => items,
        };
        let before = items
            .iter()
            .find(|item| item.label == "before")
            .expect("before completion should exist");
        let edit = before
            .text_edit
            .as_ref()
            .and_then(|edit| match edit {
                CompletionTextEdit::Edit(edit) => Some(edit),
                CompletionTextEdit::InsertAndReplace(_) => None,
            })
            .expect("completion should provide replacement edit");

        assert_eq!(edit.range.start, Position::new(1, 18));
        assert_eq!(edit.range.end, Position::new(1, 21));
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
    fn completion_after_dot_classifies_imported_function_exports() {
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
        fs::write(
            root.join("dep.ms"),
            "export let add (left : Int, right : Int) : Int := left + right;\n",
        )
        .expect("dep should be written");
        let path = root.join("index.ms");
        let source = "let dep := import \"./dep\";\ndep.";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let response = server
            .completions(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 4),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .expect("completion response should exist");
        let CompletionResponse::List(list) = response else {
            panic!("completion list expected");
        };
        let add = list
            .items
            .iter()
            .find(|item| item.label == "add")
            .expect("add completion should exist");
        let add = MusiLanguageServer::resolve_completion(add.clone());

        assert_eq!(add.kind, Some(CompletionItemKind::FUNCTION));
        assert_eq!(add.detail.as_deref(), Some("function"));
    }

    #[test]
    fn completion_inside_import_string_returns_module_items() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src/lib")).expect("lib dir should be created");
        let path = root.join("src/index.ms");
        let source = "let dep := import \"./l\";\n";
        fs::write(&path, "let dep := import \"./lib/dep\";\n").expect("entry should be written");
        fs::write(
            root.join("src/lib/dep.ms"),
            "--! dep docs\nexport let value := 1;\n",
        )
        .expect("dep should be written");
        fs::write(root.join("src/local.ms"), "export let local := 1;\n")
            .expect("local should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let response = server
            .completions(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 22),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .expect("completion response should exist");
        let CompletionResponse::List(list) = response else {
            panic!("completion list expected");
        };
        let labels = list
            .items
            .iter()
            .map(|item| item.label.as_str())
            .collect::<Vec<_>>();
        let dep = list
            .items
            .iter()
            .find(|item| item.label == "./lib/dep")
            .expect("dep module completion should exist");
        let edit = dep
            .text_edit
            .as_ref()
            .and_then(|edit| match edit {
                CompletionTextEdit::Edit(edit) => Some(edit),
                CompletionTextEdit::InsertAndReplace(_) => None,
            })
            .expect("completion should provide replacement edit");

        assert_eq!(labels, ["./lib/dep", "./local"]);
        assert_eq!(dep.kind, Some(CompletionItemKind::MODULE));
        assert_eq!(dep.documentation, None);
        let dep = MusiLanguageServer::resolve_completion(dep.clone());
        assert_eq!(
            dep.documentation,
            Some(Documentation::String("dep docs".to_owned()))
        );
        assert_eq!(edit.range.start, Position::new(0, 19));
        assert_eq!(edit.range.end, Position::new(0, 22));
        assert_eq!(edit.new_text, "./lib/dep");
    }

    #[test]
    fn completion_inside_import_string_returns_package_export_items() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        let path = root.join("src/index.ms");
        let source = "let math := import \"@std/m\";\n";
        fs::write(&path, "let math := import \"@std/math\";\n").expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let response = server
            .completions(CompletionParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 26),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: None,
            })
            .expect("completion response should exist");
        let CompletionResponse::List(list) = response else {
            panic!("completion list expected");
        };
        let math = list
            .items
            .iter()
            .find(|item| item.label == "@std/math")
            .expect("@std/math completion should exist");
        let edit = math
            .text_edit
            .as_ref()
            .and_then(|edit| match edit {
                CompletionTextEdit::Edit(edit) => Some(edit),
                CompletionTextEdit::InsertAndReplace(_) => None,
            })
            .expect("completion should provide replacement edit");

        assert_eq!(math.kind, Some(CompletionItemKind::MODULE));
        assert_eq!(edit.range.start, Position::new(0, 20));
        assert_eq!(edit.range.end, Position::new(0, 26));
        assert_eq!(edit.new_text, "@std/math");
    }

    #[test]
    fn signature_help_returns_active_callable_parameter() {
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
        let source = "\
let render (port : Int, secure : Bit) : Int := port;
render(8080, 1 = 1);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let help = server
            .signature_help_at(SignatureHelpParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 13),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                context: None,
            })
            .expect("signature help should exist");

        assert_eq!(help.active_signature, Some(0));
        assert_eq!(help.active_parameter, Some(1));
        assert_eq!(help.signatures[0].label, "render(Int, Bit) -> Int");
    }

    #[test]
    fn signature_help_uses_utf16_positions() {
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
        let source = "\
let render (port : Int, secure : Bit) : Int := port;
let icon := \"\u{1F600}\"; render(8080, 1 = 1);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let help = server
            .signature_help_at(SignatureHelpParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 34),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                context: None,
            })
            .expect("signature help should exist");

        assert_eq!(help.active_signature, Some(0));
        assert_eq!(help.active_parameter, Some(1));
        assert_eq!(help.signatures[0].label, "render(Int, Bit) -> Int");
    }

    #[test]
    fn signature_help_returns_dot_callable_signature() {
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
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
one.inc(2);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let help = server
            .signature_help_at(SignatureHelpParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(2, 9),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                context: None,
            })
            .expect("signature help should exist");

        assert_eq!(help.active_signature, Some(0));
        assert_eq!(help.active_parameter, Some(0));
        assert_eq!(help.signatures[0].label, "one.inc(Int) -> Int");
    }

    #[test]
    fn signature_help_returns_imported_member_signature() {
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
        fs::write(
            root.join("dep.ms"),
            "export let add (left : Int, right : Int) : Int := left + right;\n",
        )
        .expect("dep should be written");
        let path = root.join("index.ms");
        let source = "\
let dep := import \"./dep\";
dep.add(1, 2);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let help = server
            .signature_help_at(SignatureHelpParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 11),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                context: None,
            })
            .expect("signature help should exist");

        assert_eq!(help.active_parameter, Some(1));
        assert_eq!(
            help.signatures[0].label,
            "dep.add(left : Int, right : Int) -> Int"
        );
    }

    #[test]
    fn did_change_configuration_updates_hover_settings() {
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
        let source = "let message : String := \"Hello\";\nmessage;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        server.update_configuration(&DidChangeConfigurationParams {
            settings: serde_json::json!({
                "hover": {
                    "maximumLength": 10,
                },
            }),
        });
        let hover = server
            .hover_at(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 2),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("hover should resolve");

        let HoverContents::Markup(contents) = hover.contents else {
            panic!("markup hover expected");
        };
        assert_eq!(contents.value, "```musi\n(v…");
    }

    #[test]
    fn hover_uses_utf16_positions_for_request_and_range() {
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
        let source = "let value := 1;\nlet icon := \"\u{1F600}\"; value;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let hover = server
            .hover_at(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 22),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("hover should resolve");

        assert_eq!(
            hover.range,
            Some(Range::new(Position::new(1, 18), Position::new(1, 23)))
        );
    }

    #[test]
    fn symbol_navigation_uses_utf16_positions() {
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
        let source = "let value := 1;\nlet icon := \"\u{1F600}\"; value;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let definition = server
            .definition_at(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(1, 20),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("definition should resolve");
        let GotoDefinitionResponse::Scalar(definition) = definition else {
            panic!("definition should be scalar");
        };
        assert_eq!(
            definition.range,
            Range::new(Position::new(0, 4), Position::new(0, 9))
        );

        let references = server
            .references_at(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(1, 20),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .expect("references should resolve");
        assert_eq!(
            references
                .iter()
                .map(|location| location.range)
                .collect::<Vec<_>>(),
            [
                Range::new(Position::new(0, 4), Position::new(0, 9)),
                Range::new(Position::new(1, 18), Position::new(1, 23)),
            ]
        );

        let highlights = server
            .document_highlights(DocumentHighlightParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 20),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("document highlights should resolve");
        assert_eq!(
            highlights
                .iter()
                .map(|highlight| highlight.range)
                .collect::<Vec<_>>(),
            [
                Range::new(Position::new(0, 4), Position::new(0, 9)),
                Range::new(Position::new(1, 18), Position::new(1, 23)),
            ]
        );
    }

    #[test]
    fn rename_uses_utf16_positions_and_ranges() {
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
        let source = "let value := 1;\nlet icon := \"\u{1F600}\"; value;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let prepared = server
            .prepare_rename_at(TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position::new(1, 20),
            })
            .expect("rename should prepare");
        let PrepareRenameResponse::RangeWithPlaceholder { range, placeholder } = prepared else {
            panic!("rename should return range with placeholder");
        };
        assert_eq!(
            range,
            Range::new(Position::new(1, 18), Position::new(1, 23))
        );
        assert_eq!(placeholder, "value");

        let edit = server
            .rename_at(RenameParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(1, 20),
                },
                new_name: "renamed".to_owned(),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("rename should produce edits");
        let changes = edit.changes.expect("rename should have changes");
        let edits = changes.get(&uri).expect("rename should edit current file");
        assert_eq!(
            edits.iter().map(|edit| edit.range).collect::<Vec<_>>(),
            [
                Range::new(Position::new(0, 4), Position::new(0, 9)),
                Range::new(Position::new(1, 18), Position::new(1, 23)),
            ]
        );

        let linked = server
            .linked_editing_ranges(LinkedEditingRangeParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 20),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("linked ranges should resolve");
        assert_eq!(
            linked.ranges,
            [
                Range::new(Position::new(0, 4), Position::new(0, 9)),
                Range::new(Position::new(1, 18), Position::new(1, 23)),
            ]
        );
    }

    #[test]
    fn hover_names_imported_member_parameters() {
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
        fs::write(
            root.join("dep.ms"),
            "--- Add two numbers.\nexport let add (left : Int, right : Int) : Int := left + right;\n",
        )
        .expect("dep should be written");
        let path = root.join("index.ms");
        let source = "\
let dep := import \"./dep\";
dep.add(1, 2);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let hover = server
            .hover_at(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 4),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("hover should resolve");
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("markup hover expected");
        };

        assert!(
            contents
                .value
                .starts_with("```musi\n(function) add : (left : Int, right : Int) -> Int\n```"),
            "{}",
            contents.value
        );
        assert!(
            contents.value.contains("Add two numbers."),
            "{}",
            contents.value
        );
    }

    #[test]
    fn hover_shows_std_testing_describe_docs() {
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
        let source = "\
let testing := import \"@std/testing\";
testing.describe(\"encoding\");
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let hover = server
            .hover_at(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 9),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("hover should resolve");
        let HoverContents::Markup(contents) = hover.contents else {
            panic!("markup hover expected");
        };

        assert!(
            contents.value.contains("Start a named test suite."),
            "{}",
            contents.value
        );
    }

    #[test]
    fn inlay_hints_literal_parameter_setting_filters_non_literals() {
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
        let source = "\
let add (left : Int, right : Int) : Int := left + right;
let value := 1;
add(value, 2);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());
        server.update_configuration(&DidChangeConfigurationParams {
            settings: serde_json::json!({
                "inlayHints": {
                    "enabled": true,
                    "parameterNames": "literals",
                    "variableTypes": false,
                },
            }),
        });

        let hints = server
            .inlay_hints(&InlayHintParams {
                text_document: TextDocumentIdentifier { uri },
                range: full_document_range(source),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("inlay hints should run");
        let labels = hints
            .iter()
            .map(|hint| match &hint.label {
                InlayHintLabel::String(label) => label.as_str(),
                InlayHintLabel::LabelParts(_) => "",
            })
            .collect::<Vec<_>>();

        assert_eq!(labels, ["right:"]);
    }

    #[test]
    fn inlay_hints_defaults_match_editor_policy() {
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
        let source = "\
let add (left : Int, right : Int) : Int := left + right;
let value := 1;
let result := add(value, 2);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let hints = server
            .inlay_hints(&InlayHintParams {
                text_document: TextDocumentIdentifier { uri },
                range: full_document_range(source),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("inlay hints should run");
        let labels = hints
            .iter()
            .map(|hint| match &hint.label {
                InlayHintLabel::String(label) => label.as_str(),
                InlayHintLabel::LabelParts(_) => "",
            })
            .collect::<Vec<_>>();

        assert!(labels.iter().any(|label| label.starts_with(": Int")));
        assert!(labels.contains(&"right:"));
        assert!(!labels.contains(&"left:"));
    }

    #[test]
    fn inlay_hints_include_imported_member_parameter_names() {
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
        fs::write(
            root.join("dep.ms"),
            "export let add (left : Int, right : Int) : Int := left + right;\n",
        )
        .expect("dep should be written");
        let path = root.join("index.ms");
        let source = "\
let dep := import \"./dep\";
dep.add(1, 2);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());
        server.update_configuration(&DidChangeConfigurationParams {
            settings: serde_json::json!({
                "inlayHints": {
                    "enabled": true,
                    "parameterNames": "all",
                    "variableTypes": false,
                },
            }),
        });

        let hints = server
            .inlay_hints(&InlayHintParams {
                text_document: TextDocumentIdentifier { uri },
                range: full_document_range(source),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("inlay hints should run");
        let labels = hints
            .iter()
            .map(|hint| match &hint.label {
                InlayHintLabel::String(label) => label.as_str(),
                InlayHintLabel::LabelParts(_) => "",
            })
            .collect::<Vec<_>>();

        assert_eq!(labels, ["left:", "right:"]);
    }

    #[test]
    fn inlay_hints_use_utf16_positions() {
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
        let source = "\
let add (left : Int, right : Int) : Int := left + right;
let icon := \"\u{1F600}\"; add(1, 2);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());
        server.update_configuration(&DidChangeConfigurationParams {
            settings: serde_json::json!({
                "inlayHints": {
                    "enabled": true,
                    "parameterNames": "all",
                    "variableTypes": false,
                },
            }),
        });

        let hints = server
            .inlay_hints(&InlayHintParams {
                text_document: TextDocumentIdentifier { uri },
                range: Range::new(Position::new(1, 20), Position::new(1, 30)),
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("inlay hints should run");
        let positions = hints.iter().map(|hint| hint.position).collect::<Vec<_>>();

        assert_eq!(positions, [Position::new(1, 22), Position::new(1, 25)]);
    }

    #[test]
    fn workspace_symbols_use_initialize_workspace_roots_without_open_documents() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        fs::write(
            root.join("src/index.ms"),
            "let extra := import \"./extra\";\nlet entryValue := extra.extraValue;\n",
        )
        .expect("entry should be written");
        fs::write(root.join("src/extra.ms"), "export let extraValue := 2;\n")
            .expect("extra should be written");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        #[allow(deprecated)]
        server.configure(&InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::from_file_path(&root).expect("workspace URI should build"),
                name: "app".to_owned(),
            }]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        });

        let response = server.workspace_symbols(&WorkspaceSymbolParams {
            query: "Value".to_owned(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });
        let WorkspaceSymbolResponse::Nested(symbols) = response else {
            panic!("workspace symbols expected");
        };
        let names = symbols
            .iter()
            .map(|symbol| symbol.name.as_str())
            .collect::<Vec<_>>();

        assert!(names.contains(&"entryValue"));
        assert!(names.contains(&"extraValue"));
        let module_response = server.workspace_symbols(&WorkspaceSymbolParams {
            query: "src/extra".to_owned(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });
        let WorkspaceSymbolResponse::Nested(module_symbols) = module_response else {
            panic!("workspace module symbols expected");
        };
        let module_symbol = module_symbols
            .iter()
            .find(|symbol| symbol.name == "src/extra")
            .expect("module workspace symbol should exist");
        assert_eq!(module_symbol.kind, SymbolKind::MODULE);
        let entry = symbols
            .iter()
            .find(|symbol| symbol.name == "entryValue")
            .expect("entry symbol should exist");
        assert!(matches!(entry.location, OneOf::Right(_)));
        assert!(entry.data.is_some());
        let entry = MusiLanguageServer::resolve_workspace_symbol(entry.clone());
        assert!(matches!(entry.location, OneOf::Left(_)));
    }

    #[test]
    fn workspace_symbols_use_open_document_overlay_for_open_files() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        let path = root.join("src/index.ms");
        fs::write(&path, "let entryValue := 1;\n").expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("entry URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        #[allow(deprecated)]
        server.configure(&InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::from_file_path(&root).expect("workspace URI should build"),
                name: "app".to_owned(),
            }]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        });
        let _ = server
            .open_documents
            .insert(uri, "let unsavedValue := 1;\n".to_owned());

        let response = server.workspace_symbols(&WorkspaceSymbolParams {
            query: "Value".to_owned(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });
        let WorkspaceSymbolResponse::Nested(symbols) = response else {
            panic!("workspace symbols expected");
        };
        let names = symbols
            .iter()
            .map(|symbol| symbol.name.as_str())
            .collect::<Vec<_>>();

        assert!(names.contains(&"unsavedValue"), "{names:?}");
        assert!(!names.contains(&"entryValue"), "{names:?}");

        let module_response = server.workspace_symbols(&WorkspaceSymbolParams {
            query: "src/index".to_owned(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });
        let WorkspaceSymbolResponse::Nested(module_symbols) = module_response else {
            panic!("workspace module symbols expected");
        };
        assert!(
            module_symbols
                .iter()
                .any(|symbol| { symbol.name == "src/index" && symbol.kind == SymbolKind::MODULE })
        );
    }

    #[test]
    fn workspace_symbols_infer_project_root_from_open_document() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        let path = root.join("src/index.ms");
        fs::write(
            &path,
            "let extra := import \"./extra\";\nlet entryValue := extra.extraValue;\n",
        )
        .expect("entry should be written");
        fs::write(root.join("src/extra.ms"), "export let extraValue := 2;\n")
            .expect("extra should be written");
        let uri = Url::from_file_path(&path).expect("entry URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(
            uri,
            "let extra := import \"./extra\";\nlet unsavedValue := extra.extraValue;\n".to_owned(),
        );

        let response = server.workspace_symbols(&WorkspaceSymbolParams {
            query: "Value".to_owned(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });
        let WorkspaceSymbolResponse::Nested(symbols) = response else {
            panic!("workspace symbols expected");
        };
        let names = symbols
            .iter()
            .map(|symbol| symbol.name.as_str())
            .collect::<Vec<_>>();

        assert!(names.contains(&"unsavedValue"), "{names:?}");
        assert!(names.contains(&"extraValue"), "{names:?}");
        assert!(!names.contains(&"entryValue"), "{names:?}");
    }

    #[test]
    fn workspace_folder_changes_update_workspace_symbol_roots() {
        let old_root = temp_project();
        fs::write(
            old_root.join("musi.json"),
            r#"{
  "name": "old",
  "version": "0.1.0",
  "entry": "index.ms"
}
"#,
        )
        .expect("old manifest should be written");
        fs::write(old_root.join("index.ms"), "let oldValue := 1;\n")
            .expect("old entry should be written");
        let new_root = temp_project();
        fs::write(
            new_root.join("musi.json"),
            r#"{
  "name": "new",
  "version": "0.1.0",
  "entry": "index.ms"
}
"#,
        )
        .expect("new manifest should be written");
        fs::write(new_root.join("index.ms"), "let newValue := 1;\n")
            .expect("new entry should be written");
        let old_folder = WorkspaceFolder {
            uri: Url::from_file_path(&old_root).expect("old workspace URI should build"),
            name: "old".to_owned(),
        };
        let new_folder = WorkspaceFolder {
            uri: Url::from_file_path(&new_root).expect("new workspace URI should build"),
            name: "new".to_owned(),
        };
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        #[allow(deprecated)]
        server.configure(&InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![old_folder.clone()]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        });

        server.update_workspace_folders(DidChangeWorkspaceFoldersParams {
            event: WorkspaceFoldersChangeEvent {
                added: vec![new_folder],
                removed: vec![old_folder],
            },
        });

        let response = server.workspace_symbols(&WorkspaceSymbolParams {
            query: "Value".to_owned(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });
        let WorkspaceSymbolResponse::Nested(symbols) = response else {
            panic!("workspace symbols expected");
        };
        let names = symbols
            .iter()
            .map(|symbol| symbol.name.as_str())
            .collect::<Vec<_>>();

        assert!(names.contains(&"newValue"), "{names:?}");
        assert!(!names.contains(&"oldValue"), "{names:?}");
    }

    #[test]
    fn document_symbols_use_declaration_range_and_name_selection() {
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
        let source = "let before :=\n  1;\nlet after := before;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let symbols = server
            .document_symbols(DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("document symbols should resolve");
        let DocumentSymbolResponse::Nested(symbols) = symbols else {
            panic!("nested document symbols expected");
        };
        let before = symbols
            .iter()
            .find(|symbol| symbol.name == "before")
            .expect("before symbol should exist");

        assert_eq!(before.range.start, Position::new(0, 0));
        assert_eq!(before.range.end.line, 1);
        assert_eq!(before.selection_range.start, Position::new(0, 4));
        assert_eq!(before.selection_range.end, Position::new(0, 10));
    }

    #[test]
    fn document_symbols_nest_bindings_inside_declarations() {
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
        let source = "let outer (value : Int) : Int := value;\nlet after := outer(1);\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let symbols = server
            .document_symbols(DocumentSymbolParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("document symbols should resolve");
        let DocumentSymbolResponse::Nested(symbols) = symbols else {
            panic!("nested document symbols expected");
        };
        let outer = symbols
            .iter()
            .find(|symbol| symbol.name == "outer")
            .expect("outer symbol should exist");

        assert!(
            outer
                .children
                .as_ref()
                .is_some_and(|children| children.iter().any(|symbol| symbol.name == "value")),
            "{outer:?}"
        );
        assert!(
            !symbols.iter().any(|symbol| symbol.name == "value"),
            "{symbols:?}"
        );
    }

    #[test]
    fn declaration_reuses_symbol_definition_location() {
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
        let source = r"let value := 1;
let other := value;
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let declaration = server
            .definition_at(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 14),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("declaration should exist");

        let GotoDefinitionResponse::Scalar(location) = declaration else {
            panic!("declaration should return scalar location");
        };
        assert_eq!(location.range.start, Position::new(0, 4));
        assert_eq!(location.range.end, Position::new(0, 9));
    }

    #[test]
    fn definition_on_import_string_opens_target_module() {
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
        let dep_path = root.join("dep.ms");
        let source = "let dep := import \"./dep\";\n";
        fs::write(&path, source).expect("entry should be written");
        fs::write(&dep_path, "export let value := 1;\n").expect("dep should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let definition = server
            .definition_at(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 21),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("import definition should exist");

        let GotoDefinitionResponse::Scalar(location) = definition else {
            panic!("definition should return scalar location");
        };
        let location_path = location
            .uri
            .to_file_path()
            .expect("location URI should be a file path");
        assert_eq!(
            location_path,
            fs::canonicalize(dep_path).expect("dep path should canonicalize")
        );
        assert_eq!(
            location.range,
            Range::new(Position::new(0, 0), Position::new(0, 0))
        );
    }

    #[test]
    fn definition_on_import_record_member_jumps_to_exported_binding() {
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
        let index_path = root.join("index.ms");
        let core_path = root.join("core.ms");
        fs::write(
            &index_path,
            "let core := import \"./core\";\nlet value := core.Word;\n",
        )
        .expect("entry should be written");
        fs::write(&core_path, "export let Word := Type;\n").expect("core should be written");
        let uri = Url::from_file_path(&index_path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(
            uri.clone(),
            "let core := import \"./core\";\nlet value := core.Word;\n".to_owned(),
        );

        let definition = server
            .definition_at(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 19),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("member definition should resolve");

        let GotoDefinitionResponse::Scalar(location) = definition else {
            panic!("definition should return scalar location");
        };
        let location_path = location
            .uri
            .to_file_path()
            .expect("location URI should be a file path");
        assert_eq!(
            location_path,
            fs::canonicalize(core_path).expect("core path should canonicalize")
        );
        assert_eq!(
            location.range,
            Range::new(Position::new(0, 11), Position::new(0, 15))
        );
    }

    #[test]
    fn definition_on_variant_value_jumps_to_variant_declaration() {
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
        let index_path = root.join("index.ms");
        let source = "\
let Option := data { | A | B };
let first := .A;
";
        fs::write(&index_path, source).expect("entry should be written");
        let uri = Url::from_file_path(&index_path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let definition = server
            .definition_at(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 14),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("variant definition should resolve");

        let GotoDefinitionResponse::Scalar(location) = definition else {
            panic!("definition should return scalar location");
        };
        let location_path = location
            .uri
            .to_file_path()
            .expect("location URI should be a file path");
        assert!(paths_match(&location_path, &index_path));
        assert_eq!(
            location.range,
            Range::new(Position::new(0, 23), Position::new(0, 24))
        );
    }

    #[test]
    fn references_on_import_string_find_matching_imports() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        let index_path = root.join("src/index.ms");
        let other_path = root.join("src/other.ms");
        fs::write(&index_path, "let dep := import \"./dep\";\n").expect("entry should be written");
        fs::write(&other_path, "let dep := import \"./dep\";\n").expect("other should be written");
        fs::write(root.join("src/dep.ms"), "export let value := 1;\n")
            .expect("dep should be written");
        let uri = Url::from_file_path(&index_path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        #[allow(deprecated)]
        server.configure(&InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::from_file_path(&root).expect("workspace URI should build"),
                name: "app".to_owned(),
            }]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        });
        let _ = server
            .open_documents
            .insert(uri.clone(), "let dep := import \"./dep\";\n".to_owned());

        let locations = server
            .references_at(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 21),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .expect("import references should resolve");
        let paths = locations
            .iter()
            .map(|location| location.uri.to_file_path().expect("location path"))
            .collect::<Vec<_>>();

        assert_eq!(locations.len(), 2);
        assert!(paths.iter().any(|path| paths_match(path, &index_path)));
        assert!(paths.iter().any(|path| paths_match(path, &other_path)));
        assert!(locations.iter().all(|location| {
            location.range == Range::new(Position::new(0, 18), Position::new(0, 25))
        }));
    }

    #[test]
    fn references_on_import_string_infer_project_root_from_open_document() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        let index_path = root.join("src/index.ms");
        let other_path = root.join("src/other.ms");
        fs::write(
            &index_path,
            "let other := import \"./other\";\nlet dep := import \"./dep\";\n",
        )
        .expect("entry should be written");
        fs::write(&other_path, "let dep := import \"./dep\";\n").expect("other should be written");
        fs::write(root.join("src/dep.ms"), "export let value := 1;\n")
            .expect("dep should be written");
        let uri = Url::from_file_path(&index_path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(
            uri.clone(),
            "let other := import \"./other\";\nlet dep := import \"./dep\";\n".to_owned(),
        );

        let locations = server
            .references_at(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 21),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .expect("import references should resolve");
        let paths = locations
            .iter()
            .map(|location| location.uri.to_file_path().expect("location path"))
            .collect::<Vec<_>>();

        assert_eq!(locations.len(), 2);
        assert!(paths.iter().any(|path| paths_match(path, &index_path)));
        assert!(paths.iter().any(|path| paths_match(path, &other_path)));
    }

    #[test]
    fn references_on_member_reference_include_dot_callable_uses() {
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
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
let result := one.inc(2);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let locations = server
            .references_at(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(2, 19),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: ReferenceContext {
                    include_declaration: true,
                },
            })
            .expect("member references should resolve");

        assert_eq!(locations.len(), 2);
        assert_eq!(locations[0].range.start, Position::new(0, 4));
        assert_eq!(locations[1].range.start, Position::new(2, 18));
    }

    #[test]
    fn document_highlight_on_import_string_marks_matching_imports() {
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
        let source = "let dep := import \"./dep\";\nlet again := import \"./dep\";\n";
        fs::write(&path, source).expect("entry should be written");
        fs::write(root.join("dep.ms"), "export let value := 1;\n").expect("dep should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let highlights = server
            .document_highlights(DocumentHighlightParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 21),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("import highlights should resolve");

        assert_eq!(highlights.len(), 2);
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::TEXT));
        assert_eq!(
            highlights[0].range,
            Range::new(Position::new(0, 18), Position::new(0, 25))
        );
        assert_eq!(
            highlights[1].range,
            Range::new(Position::new(1, 20), Position::new(1, 27))
        );
    }

    #[test]
    fn linked_editing_on_import_string_returns_matching_import_ranges() {
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
        let source = "let dep := import \"./dep\";\nlet again := import \"./dep\";\n";
        fs::write(&path, source).expect("entry should be written");
        fs::write(root.join("dep.ms"), "export let value := 1;\n").expect("dep should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let ranges = server
            .linked_editing_ranges(LinkedEditingRangeParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 21),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("import linked editing should resolve");

        assert_eq!(
            ranges.ranges,
            [
                Range::new(Position::new(0, 18), Position::new(0, 25)),
                Range::new(Position::new(1, 20), Position::new(1, 27)),
            ]
        );
        assert_eq!(ranges.word_pattern, None);
    }

    #[test]
    fn type_definition_resolves_named_value_type() {
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
        let source = "\
let Box[T] := data {
  let value : T;
};
let boxedName : Box[String] := {
  value := \"Nora\"
};
boxedName.value;
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let response = server
            .type_definition_at(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(6, 2),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("type definition should resolve");
        let GotoDefinitionResponse::Scalar(location) = response else {
            panic!("scalar location expected");
        };

        assert_eq!(location.range.start, Position::new(0, 4));
        assert_eq!(location.range.end, Position::new(0, 7));
    }

    #[test]
    fn implementation_returns_none_for_shape_without_givens() {
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
        let source = "\
let Eq [T] := shape {
  let equals(left : T, right : T) : Bit;
};
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let response = server.implementation_at(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(0, 4),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });
        assert!(response.is_none());
    }

    #[test]
    fn implementation_returns_none_for_workspace_shape_without_givens() {
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
        fs::write(root.join("index.ms"), "import \"./shapes\";\n")
            .expect("entry should be written");
        let shape_path = root.join("shapes.ms");
        let shape_source = "\
export let Eq [T] := shape {
  let equals(left : T, right : T) : Bit;
};
";
        let impl_source = "\
let shapes := import \"./shapes\";
let Eq := shapes.Eq;
";
        let more_impl_source = "\
let shapes := import \"./shapes\";
let Eq := shapes.Eq;
";
        fs::write(&shape_path, shape_source).expect("shape module should be written");
        fs::write(root.join("impls.ms"), impl_source).expect("impl module should be written");
        fs::write(root.join("more_impls.ms"), more_impl_source)
            .expect("more impl module should be written");
        let uri = Url::from_file_path(&shape_path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server
            .open_documents
            .insert(uri.clone(), shape_source.to_owned());

        let response = server.implementation_at(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(0, 11),
            },
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });
        assert!(response.is_none());
    }

    #[test]
    fn prepare_rename_on_reference_returns_reference_range() {
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
        let source = "let before := 1;\nlet after := before;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let prepared = server
            .prepare_rename_at(TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(1, 13),
            })
            .expect("rename should prepare");
        let PrepareRenameResponse::RangeWithPlaceholder { range, placeholder } = prepared else {
            panic!("rename should return range with placeholder");
        };

        assert_eq!(placeholder, "before");
        assert_eq!(range.start, Position::new(1, 13));
        assert_eq!(range.end, Position::new(1, 19));
    }

    #[test]
    fn prepare_rename_on_member_reference_returns_member_range() {
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
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
let result := one.inc(2);
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let prepared = server
            .prepare_rename_at(TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri },
                position: Position::new(2, 19),
            })
            .expect("rename should prepare");
        let PrepareRenameResponse::RangeWithPlaceholder { range, placeholder } = prepared else {
            panic!("rename should return range with placeholder");
        };

        assert_eq!(placeholder, "inc");
        assert_eq!(range.start, Position::new(2, 18));
        assert_eq!(range.end, Position::new(2, 21));
    }

    #[test]
    fn document_highlight_returns_declaration_and_same_file_references() {
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
        let source = r"let value := 1;
let other := value + value;
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let highlights = server
            .document_highlights(DocumentHighlightParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 15),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("document highlight response should exist");

        assert_eq!(highlights.len(), 3);
        assert_eq!(highlights[0].kind, Some(DocumentHighlightKind::WRITE));
        assert_eq!(highlights[1].kind, Some(DocumentHighlightKind::READ));
        assert_eq!(highlights[2].kind, Some(DocumentHighlightKind::READ));
        assert!(
            highlights
                .iter()
                .any(|highlight| highlight.range.start == Position::new(0, 4))
        );
        assert!(
            highlights
                .iter()
                .filter(|highlight| highlight.range.start.line == 1)
                .count()
                == 2
        );
    }

    #[test]
    fn linked_editing_range_returns_same_file_symbol_ranges() {
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
        let source = r"let value := 1;
let other := value + value;
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let linked = server
            .linked_editing_ranges(LinkedEditingRangeParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 14),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("linked editing ranges should exist");

        assert_eq!(linked.ranges.len(), 3);
        assert!(linked.ranges.contains(&Range {
            start: Position::new(0, 4),
            end: Position::new(0, 9),
        }));
        assert!(linked.ranges.contains(&Range {
            start: Position::new(1, 13),
            end: Position::new(1, 18),
        }));
        assert!(linked.ranges.contains(&Range {
            start: Position::new(1, 21),
            end: Position::new(1, 26),
        }));
        assert_eq!(
            linked.word_pattern.as_deref(),
            Some("[A-Za-z_][A-Za-z0-9_]*")
        );
    }

    #[test]
    fn moniker_returns_project_unique_definition_identifier() {
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
        let source = "let value := 1;\nlet other := value;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let monikers = server
            .monikers_at(MonikerParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(1, 15),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("moniker should resolve");

        assert_eq!(monikers.len(), 1);
        assert_eq!(monikers[0].scheme, "musi");
        assert_eq!(monikers[0].unique, UniquenessLevel::Project);
        assert_eq!(monikers[0].kind, Some(MonikerKind::Local));
        assert_eq!(monikers[0].identifier, format!("{}#1:5", uri.as_str()));
    }

    #[test]
    fn moniker_marks_imported_binding_identifiers() {
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
        let source = "import \"./dep\";\nlet result := base;\n";
        fs::write(&path, source).expect("entry should be written");
        fs::write(root.join("dep.ms"), "export let base := 1;\n").expect("dep should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let monikers = server
            .monikers_at(MonikerParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(1, 14),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("import moniker should resolve");

        assert_eq!(monikers.len(), 1);
        assert_eq!(monikers[0].kind, Some(MonikerKind::Import));
        assert_eq!(monikers[0].identifier, format!("{}#1:8", uri.as_str()));
    }

    #[test]
    fn moniker_on_import_string_identifies_resolved_module() {
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
        let dep_path = root.join("dep.ms");
        let source = "let dep := import \"./dep\";\n";
        fs::write(&path, source).expect("entry should be written");
        fs::write(&dep_path, "export let value := 1;\n").expect("dep should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let dep_uri =
            Url::from_file_path(fs::canonicalize(dep_path).expect("dep path should canonicalize"))
                .expect("dep URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let monikers = server
            .monikers_at(MonikerParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 21),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("import string moniker should resolve");

        assert_eq!(monikers.len(), 1);
        assert_eq!(monikers[0].scheme, "musi");
        assert_eq!(monikers[0].unique, UniquenessLevel::Project);
        assert_eq!(monikers[0].kind, Some(MonikerKind::Import));
        assert_eq!(monikers[0].identifier, format!("{}#1:1", dep_uri.as_str()));
    }

    #[test]
    fn call_hierarchy_prepare_returns_symbol_under_cursor() {
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
        let source = "let value := 1;\nlet caller := value;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let items = server
            .prepare_call_hierarchy_at(CallHierarchyPrepareParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(0, 5),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("call hierarchy item should exist");

        assert_eq!(items.len(), 1);
        assert_eq!(items[0].name, "value");
        assert_eq!(items[0].uri, uri);
        assert_eq!(items[0].selection_range.start, Position::new(0, 4));
        assert!(items[0].data.is_some());
    }

    #[test]
    fn call_hierarchy_incoming_calls_group_references_by_caller_symbol() {
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
        let source = "let value := 1;\nlet caller := value;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());
        let value_hierarchy = server
            .prepare_call_hierarchy_at(CallHierarchyPrepareParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(0, 5),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("call hierarchy item should exist")
            .remove(0);

        let calls = server
            .call_hierarchy_incoming_calls(&CallHierarchyIncomingCallsParams {
                item: value_hierarchy,
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("incoming calls should run");

        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].from.name, "caller");
        assert_eq!(calls[0].from.uri, uri);
        assert_eq!(calls[0].from_ranges.len(), 1);
        assert_eq!(calls[0].from_ranges[0].start, Position::new(1, 14));
        assert_eq!(calls[0].from_ranges[0].end, Position::new(1, 19));
    }

    #[test]
    fn call_hierarchy_incoming_calls_use_multiline_caller_range() {
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
        let source = "let target () := 1;\nlet caller () : Int :=\n  target();\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());
        let target_hierarchy = server
            .prepare_call_hierarchy_at(CallHierarchyPrepareParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 5),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("call hierarchy item should exist")
            .remove(0);

        let calls = server
            .call_hierarchy_incoming_calls(&CallHierarchyIncomingCallsParams {
                item: target_hierarchy,
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("incoming calls should run");

        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].from.name, "caller");
        assert_eq!(calls[0].from_ranges[0].start, Position::new(2, 2));
        assert_eq!(calls[0].from_ranges[0].end, Position::new(2, 8));
    }

    #[test]
    fn call_hierarchy_outgoing_calls_return_direct_name_callees() {
        let root = temp_project();
        let path = root.join("index.ms");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "let target () := 1;\nlet caller () := target();\n";
        fs::write(&path, source).expect("entry should be written");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());
        let caller_hierarchy = server
            .prepare_call_hierarchy_at(CallHierarchyPrepareParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(1, 5),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("call hierarchy should prepare")
            .pop()
            .expect("caller item should exist");

        let calls = server
            .call_hierarchy_outgoing_calls(&CallHierarchyOutgoingCallsParams {
                item: caller_hierarchy,
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("outgoing calls should run");

        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].to.name, "target");
        assert_eq!(calls[0].from_ranges.len(), 1);
        assert_eq!(calls[0].from_ranges[0].start, Position::new(1, 17));
        assert_eq!(calls[0].from_ranges[0].end, Position::new(1, 23));

        let incoming = server
            .call_hierarchy_incoming_calls(&CallHierarchyIncomingCallsParams {
                item: calls[0].to.clone(),
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("outgoing callee item should keep navigation data");

        assert_eq!(incoming.len(), 1);
        assert_eq!(incoming[0].from.name, "caller");
    }

    #[test]
    fn call_hierarchy_outgoing_calls_return_member_callees() {
        let root = temp_project();
        let path = root.join("index.ms");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "\
let inc (self : Int, by : Int) : Int := self + by;
let one : Int := 1;
let caller () := one.inc(2);
";
        fs::write(&path, source).expect("entry should be written");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());
        let caller_hierarchy = server
            .prepare_call_hierarchy_at(CallHierarchyPrepareParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(2, 5),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("call hierarchy should prepare")
            .pop()
            .expect("caller item should exist");

        let calls = server
            .call_hierarchy_outgoing_calls(&CallHierarchyOutgoingCallsParams {
                item: caller_hierarchy,
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("outgoing calls should run");

        assert_eq!(calls.len(), 1);
        assert_eq!(calls[0].to.name, "inc");
        assert_eq!(calls[0].from_ranges.len(), 1);
        assert_eq!(calls[0].from_ranges[0].start, Position::new(2, 21));
        assert_eq!(calls[0].from_ranges[0].end, Position::new(2, 24));
    }

    #[test]
    fn code_action_returns_source_organize_imports_edit() {
        let root = temp_project();
        let path = root.join("index.ms");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "let testing := import \"@std/testing\";\nlet io := import \"@std/io\";\nlet value:=1;\n";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let actions = server
            .code_actions(CodeActionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                range: full_document_range(source),
                context: CodeActionContext {
                    diagnostics: Vec::new(),
                    only: Some(vec![CodeActionKind::SOURCE_ORGANIZE_IMPORTS]),
                    trigger_kind: None,
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("code action response should exist");

        assert_eq!(actions.len(), 1);
        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("organize imports should be returned as code action");
        };
        assert_eq!(action.kind, Some(CodeActionKind::SOURCE_ORGANIZE_IMPORTS));
        assert_eq!(action.is_preferred, Some(true));
        assert!(action.edit.is_none());
        assert!(action.data.is_some());
        let action = server.resolve_code_action(action.clone());
        let edit = action.edit.as_ref().expect("action should provide edit");
        let changes = edit.changes.as_ref().expect("edit should include changes");
        let edits = changes.get(&uri).expect("edit should target document URI");
        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].new_text,
            "let io := import \"@std/io\";\nlet testing := import \"@std/testing\";\nlet value:=1;\n"
        );
    }

    #[test]
    fn code_action_organize_imports_respects_fmt_ignore() {
        let root = temp_project();
        let path = root.join("index.ms");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "let z := import \"./z\";\nlet y := import \"./y\";\n-- musi-fmt-ignore\nlet   a:=import \"./a\";\n";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let actions = server
            .code_actions(CodeActionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                range: full_document_range(source),
                context: CodeActionContext {
                    diagnostics: Vec::new(),
                    only: Some(vec![CodeActionKind::SOURCE_ORGANIZE_IMPORTS]),
                    trigger_kind: None,
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("code action response should exist");
        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("organize imports should be returned as code action");
        };
        let action = server.resolve_code_action(action.clone());
        let edit = action.edit.as_ref().expect("action should provide edit");
        let changes = edit.changes.as_ref().expect("edit should include changes");
        let edits = changes.get(&uri).expect("edit should target document URI");

        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].new_text,
            "let y := import \"./y\";\nlet z := import \"./z\";\n-- musi-fmt-ignore\nlet   a:=import \"./a\";\n"
        );
    }

    #[test]
    fn code_action_organize_imports_moves_attached_comments() {
        let root = temp_project();
        let path = root.join("index.ms");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = "\
-- testing helpers
let testing := import \"@std/testing\";
-- io helpers
let io := import \"@std/io\";
";
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let actions = server
            .code_actions(CodeActionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                range: full_document_range(source),
                context: CodeActionContext {
                    diagnostics: Vec::new(),
                    only: Some(vec![CodeActionKind::SOURCE_ORGANIZE_IMPORTS]),
                    trigger_kind: None,
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("code action response should exist");
        let CodeActionOrCommand::CodeAction(action) = &actions[0] else {
            panic!("organize imports should be returned as code action");
        };
        let action = server.resolve_code_action(action.clone());
        let edit = action.edit.as_ref().expect("action should provide edit");
        let changes = edit.changes.as_ref().expect("edit should include changes");
        let edits = changes.get(&uri).expect("edit should target document URI");

        assert_eq!(edits.len(), 1);
        assert_eq!(
            edits[0].new_text,
            "\
-- io helpers
let io := import \"@std/io\";
-- testing helpers
let testing := import \"@std/testing\";
"
        );
    }

    #[test]
    fn document_link_resolves_static_import_targets() {
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
        let dep_path = root.join("dep.ms");
        let source = "let dep := import \"./dep\";\n";
        fs::write(&path, source).expect("entry should be written");
        fs::write(&dep_path, "export let value := 1;\n").expect("dep should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let links = server
            .document_links(DocumentLinkParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("document links should run");

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].range.start, Position::new(0, 18));
        assert_eq!(links[0].range.end, Position::new(0, 25));
        assert_eq!(links[0].target, None);
        assert_eq!(links[0].tooltip, None);
        assert!(links[0].data.is_some());

        let link = MusiLanguageServer::resolve_document_link(links[0].clone());

        assert_eq!(
            link.target.as_ref(),
            Some(
                &Url::from_file_path(
                    fs::canonicalize(dep_path).expect("dep path should canonicalize")
                )
                .expect("dep URI should build")
            )
        );
        assert!(
            link.tooltip
                .as_deref()
                .is_some_and(|tooltip| tooltip.starts_with("Open `"))
        );
    }

    #[test]
    fn document_link_resolves_package_import_targets() {
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
        let source = "let math := import \"@std/math\";\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let links = server
            .document_links(DocumentLinkParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("document links should run");

        assert_eq!(links.len(), 1);
        assert_eq!(links[0].range.start, Position::new(0, 19));
        assert_eq!(links[0].range.end, Position::new(0, 30));
        let link = MusiLanguageServer::resolve_document_link(links[0].clone());
        let target_path = link
            .target
            .expect("package import link should resolve target")
            .to_file_path()
            .expect("package import target should be file URI");

        assert_eq!(
            target_path.file_name().and_then(|name| name.to_str()),
            Some("math.ms")
        );
        assert_eq!(link.tooltip.as_deref(), Some("Open `@std/math`"));
    }

    #[test]
    fn import_string_features_use_utf16_positions() {
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
        fs::write(
            root.join("dep.ms"),
            "--! Dep docs.\nexport let value := 1;\n",
        )
        .expect("dep should be written");
        let path = root.join("index.ms");
        let source = "let icon := \"\u{1F600}\"; let dep := import \"./dep\";\nlet other := import \"./dep\";\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let links = server
            .document_links(DocumentLinkParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("document links should run");
        assert_eq!(
            links[0].range,
            Range::new(Position::new(0, 36), Position::new(0, 43))
        );

        let hover = server
            .hover_at(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(0, 37),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("import hover should resolve");
        assert_eq!(
            hover.range,
            Some(Range::new(Position::new(0, 36), Position::new(0, 43)))
        );

        let definition = server
            .definition_at(GotoDefinitionParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(0, 37),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("import definition should resolve");
        assert!(matches!(definition, GotoDefinitionResponse::Scalar(_)));

        let references = server
            .references_at(ReferenceParams {
                text_document_position: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(0, 37),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
                context: ReferenceContext {
                    include_declaration: false,
                },
            })
            .expect("import references should resolve");
        assert_eq!(
            references
                .iter()
                .map(|location| location.range)
                .collect::<Vec<_>>(),
            [
                Range::new(Position::new(0, 36), Position::new(0, 43)),
                Range::new(Position::new(1, 20), Position::new(1, 27)),
            ]
        );

        let highlights = server
            .document_highlights(DocumentHighlightParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri: uri.clone() },
                    position: Position::new(0, 37),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("import highlights should resolve");
        assert_eq!(
            highlights[0].range,
            Range::new(Position::new(0, 36), Position::new(0, 43))
        );

        let linked = server
            .linked_editing_ranges(LinkedEditingRangeParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 37),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("linked import ranges should resolve");
        assert_eq!(
            linked.ranges,
            [
                Range::new(Position::new(0, 36), Position::new(0, 43)),
                Range::new(Position::new(1, 20), Position::new(1, 27)),
            ]
        );
    }

    #[test]
    fn hover_on_import_string_returns_resolved_module_docs() {
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
        let source = "let dep := import \"./dep\";\n";
        fs::write(&path, source).expect("entry should be written");
        fs::write(
            root.join("dep.ms"),
            "--! dependency docs\nexport let value := 1;\n",
        )
        .expect("dep should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let hover = server
            .hover_at(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 21),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("import hover should resolve");

        let HoverContents::Markup(contents) = hover.contents else {
            panic!("markup hover expected");
        };
        assert!(contents.value.contains("(module) ./dep"));
        assert!(contents.value.contains("Resolves to `@app@0.1.0/dep.ms`."));
        assert!(contents.value.contains("dependency docs"));
        assert_eq!(
            hover.range,
            Some(Range::new(Position::new(0, 18), Position::new(0, 25)))
        );
    }

    #[test]
    fn hover_returns_module_docs_on_module_doc_comments() {
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
        let source = "--! module docs\n--! more module docs\nlet message : String := \"Hello\";\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let hover = server
            .hover_at(HoverParams {
                text_document_position_params: TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position: Position::new(0, 3),
                },
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("module doc hover should resolve");

        let HoverContents::Markup(contents) = hover.contents else {
            panic!("markup hover expected");
        };
        assert_eq!(hover.range.expect("hover range").start, Position::new(0, 0));
        assert!(contents.value.contains("module docs"));
        assert!(contents.value.contains("more module docs"));
    }

    #[test]
    fn code_lens_resolves_reference_counts_for_document_symbols() {
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
        let source = r"let value := 1;
let other := value + value;
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let lenses = server
            .code_lenses(CodeLensParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("code lenses should run");

        let value_lens = lenses
            .iter()
            .find(|lens| lens.range.start == Position::new(0, 4))
            .expect("value reference lens should exist");
        assert!(value_lens.command.is_none());
        let value_lens = server.resolve_code_lens(value_lens.clone());
        let command = value_lens.command.as_ref().expect("lens command");
        assert_eq!(command.title, "2 references");
        assert_eq!(command.command, SHOW_REFERENCES_COMMAND);
        let arguments = command.arguments.as_ref().expect("lens arguments");
        assert_eq!(arguments.len(), 3);
        assert_eq!(arguments[0].as_str(), Some(uri.as_str()));
        assert_eq!(
            arguments[1].get("line").and_then(serde_json::Value::as_u64),
            Some(0)
        );
        assert_eq!(
            arguments[1]
                .get("character")
                .and_then(serde_json::Value::as_u64),
            Some(4)
        );
        let locations: Vec<Location> =
            serde_json::from_value(arguments[2].clone()).expect("locations should deserialize");
        assert_eq!(locations.len(), 2);
    }

    #[test]
    fn code_lens_resolves_workspace_references_command() {
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
        let source = "export let value := 1;\n";
        fs::write(&path, source).expect("entry should be written");
        fs::write(
            root.join("use.ms"),
            "let app := import \"./index\";\nlet result := app.value;\n",
        )
        .expect("use module should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let lenses = server
            .code_lenses(CodeLensParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("code lenses should run");
        let value_lens = lenses
            .iter()
            .find(|lens| lens.range.start == Position::new(0, 11))
            .expect("value reference lens should exist");
        let value_lens = server.resolve_code_lens(value_lens.clone());
        let command = value_lens.command.as_ref().expect("lens command");
        assert_eq!(command.title, "1 reference");
        assert_eq!(command.command, SHOW_REFERENCES_COMMAND);
        let arguments = command.arguments.as_ref().expect("lens arguments");
        let locations: Vec<Location> =
            serde_json::from_value(arguments[2].clone()).expect("locations should deserialize");

        assert_eq!(locations.len(), 1);
    }

    #[test]
    fn execute_references_command_returns_reference_locations() {
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
        let source = r"let value := 1;
let other := value + value;
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let result = server
            .execute_command_request(&ExecuteCommandParams {
                command: "musi.references".to_owned(),
                arguments: vec![serde_json::json!({
                    "uri": uri.as_str(),
                    "line": 0,
                    "character": 4,
                })],
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("references command should return locations");
        let locations: Vec<Location> =
            serde_json::from_value(result).expect("locations should deserialize");

        assert_eq!(locations.len(), 2);
        assert!(locations.iter().all(|location| location.uri == uri));
        assert!(
            locations
                .iter()
                .all(|location| location.range.start.line == 1)
        );
    }

    #[test]
    fn execute_references_command_returns_import_reference_locations() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        let index_path = root.join("src/index.ms");
        let other_path = root.join("src/other.ms");
        fs::write(&index_path, "let dep := import \"./dep\";\n").expect("entry should be written");
        fs::write(&other_path, "let dep := import \"./dep\";\n").expect("other should be written");
        fs::write(root.join("src/dep.ms"), "export let value := 1;\n")
            .expect("dep should be written");
        let uri = Url::from_file_path(&index_path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        #[allow(deprecated)]
        server.configure(&InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::from_file_path(&root).expect("workspace URI should build"),
                name: "app".to_owned(),
            }]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        });
        let _ = server
            .open_documents
            .insert(uri.clone(), "let dep := import \"./dep\";\n".to_owned());

        let result = server
            .execute_command_request(&ExecuteCommandParams {
                command: "musi.references".to_owned(),
                arguments: vec![serde_json::json!({
                    "uri": uri.as_str(),
                    "line": 0,
                    "character": 21,
                })],
                work_done_progress_params: WorkDoneProgressParams::default(),
            })
            .expect("references command should return import locations");
        let locations: Vec<Location> =
            serde_json::from_value(result).expect("locations should deserialize");
        let paths = locations
            .iter()
            .map(|location| location.uri.to_file_path().expect("location path"))
            .collect::<Vec<_>>();

        assert_eq!(locations.len(), 2);
        assert!(paths.iter().any(|path| paths_match(path, &index_path)));
        assert!(paths.iter().any(|path| paths_match(path, &other_path)));
        assert!(locations.iter().all(|location| {
            location.range == Range::new(Position::new(0, 18), Position::new(0, 25))
        }));
    }

    #[test]
    fn folding_range_returns_multiline_node_and_comment_ranges() {
        let root = temp_project();
        let path = root.join("index.ms");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let source = concat!(
            "/",
            "\
-- docs
    more docs -/
let Pair := data {
  let left : Int;
  let right : Int;
};
",
        );
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let ranges = server
            .folding_ranges(FoldingRangeParams {
                text_document: TextDocumentIdentifier { uri },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("folding range response should exist");

        assert!(ranges.iter().any(|range| {
            range.kind == Some(FoldingRangeKind::Comment)
                && range.start_line == 0
                && range.end_line == 1
        }));
        assert!(
            ranges
                .iter()
                .any(|range| range.start_line == 2 && range.end_line == 5)
        );
    }

    #[test]
    fn selection_range_expands_identifier_selection_to_parent_ranges() {
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
        let source = r"let value := 1;
let other := value + 2;
";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let ranges = server
            .selection_ranges(SelectionRangeParams {
                text_document: TextDocumentIdentifier { uri },
                positions: vec![Position::new(1, 13)],
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("selection ranges should run");

        assert_eq!(ranges.len(), 1);
        assert_eq!(ranges[0].range.start, Position::new(1, 13));
        assert_eq!(ranges[0].range.end, Position::new(1, 18));
        assert!(
            ranges[0]
                .parent
                .as_ref()
                .is_some_and(|parent| parent.range.start.line == 1
                    && parent.range.end.character >= ranges[0].range.end.character)
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
    fn document_diagnostic_returns_full_report_for_open_document() {
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
        let source = "let value := 1;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let report = server.document_diagnostics(&DocumentDiagnosticParams {
            text_document: TextDocumentIdentifier { uri },
            identifier: Some("musi".to_owned()),
            previous_result_id: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });

        let DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(report)) = report
        else {
            panic!("document diagnostics should return a full report");
        };
        assert_eq!(report.full_document_diagnostic_report.result_id, None);
    }

    #[test]
    fn workspace_diagnostic_returns_open_document_reports() {
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
        let source = "let value := 1;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri, source.to_owned());

        let report = server.workspace_diagnostics(WorkspaceDiagnosticParams {
            identifier: Some("musi".to_owned()),
            previous_result_ids: Vec::new(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });

        let WorkspaceDiagnosticReportResult::Report(report) = report else {
            panic!("workspace diagnostics should return a report");
        };
        assert_eq!(report.items.len(), 1);
        let WorkspaceDocumentDiagnosticReport::Full(item) = &report.items[0] else {
            panic!("workspace diagnostics should use full reports");
        };
        let report_path = item
            .uri
            .to_file_path()
            .expect("report URI should be file path");
        assert!(paths_match(&report_path, &path));
        assert_eq!(item.version, None);
    }

    #[test]
    fn workspace_diagnostic_uses_initialize_workspace_roots_without_open_documents() {
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
        fs::write(&path, "let value : Int := \"bad\";\n").expect("entry should be written");
        let _uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        #[allow(deprecated)]
        server.configure(&InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::from_file_path(&root).expect("workspace URI should build"),
                name: "app".to_owned(),
            }]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        });

        let report = server.workspace_diagnostics(WorkspaceDiagnosticParams {
            identifier: Some("musi".to_owned()),
            previous_result_ids: Vec::new(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });

        let WorkspaceDiagnosticReportResult::Report(report) = report else {
            panic!("workspace diagnostics should return a report");
        };
        assert_eq!(report.items.len(), 1);
        let WorkspaceDocumentDiagnosticReport::Full(item) = &report.items[0] else {
            panic!("workspace diagnostics should use full reports");
        };
        let report_path = item
            .uri
            .to_file_path()
            .expect("report URI should be file path");
        assert!(paths_match(&report_path, &path));
        assert!(!item.full_document_diagnostic_report.items.is_empty());
    }

    #[test]
    fn workspace_diagnostic_uses_open_document_overlay_for_open_files() {
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
        fs::write(&path, "let value : Int := 1;\n").expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        #[allow(deprecated)]
        server.configure(&InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::from_file_path(&root).expect("workspace URI should build"),
                name: "app".to_owned(),
            }]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        });
        let _ = server
            .open_documents
            .insert(uri.clone(), "let value : Int := \"bad\";\n".to_owned());

        let report = server.workspace_diagnostics(WorkspaceDiagnosticParams {
            identifier: Some("musi".to_owned()),
            previous_result_ids: Vec::new(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });

        let WorkspaceDiagnosticReportResult::Report(report) = report else {
            panic!("workspace diagnostics should return a report");
        };
        assert_eq!(report.items.len(), 1);
        let WorkspaceDocumentDiagnosticReport::Full(item) = &report.items[0] else {
            panic!("workspace diagnostics should use full reports");
        };
        assert_eq!(item.uri, uri);
        assert!(!item.full_document_diagnostic_report.items.is_empty());
    }

    #[test]
    fn workspace_diagnostic_infers_project_root_from_open_document() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        let index_path = root.join("src/index.ms");
        let other_path = root.join("src/other.ms");
        fs::write(
            &index_path,
            "let other := import \"./other\";\nlet value : Int := 1;\n",
        )
        .expect("entry should be written");
        fs::write(&other_path, "export let otherValue : Int := \"bad\";\n")
            .expect("other should be written");
        let uri = Url::from_file_path(&index_path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(
            uri,
            "let other := import \"./other\";\nlet value : Int := 1;\n".to_owned(),
        );

        let report = server.workspace_diagnostics(WorkspaceDiagnosticParams {
            identifier: Some("musi".to_owned()),
            previous_result_ids: Vec::new(),
            work_done_progress_params: WorkDoneProgressParams::default(),
            partial_result_params: PartialResultParams::default(),
        });

        let WorkspaceDiagnosticReportResult::Report(report) = report else {
            panic!("workspace diagnostics should return a report");
        };
        let diagnostic_paths = report
            .items
            .iter()
            .filter_map(|item| {
                let WorkspaceDocumentDiagnosticReport::Full(item) = item else {
                    return None;
                };
                (!item.full_document_diagnostic_report.items.is_empty())
                    .then(|| item.uri.to_file_path().expect("diagnostic URI path"))
            })
            .collect::<Vec<_>>();

        assert!(
            diagnostic_paths
                .iter()
                .any(|path| paths_match(path, &other_path)),
            "{diagnostic_paths:?}"
        );
    }

    #[test]
    fn will_rename_files_updates_static_import_specifiers() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        let index_path = root.join("src/index.ms");
        let dep_path = root.join("src/dep.ms");
        let next_path = root.join("src/renamed.ms");
        fs::write(&index_path, "let dep := import \"./dep\";\n").expect("index should be written");
        fs::write(&dep_path, "export let value := 1;\n").expect("dep should be written");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        #[allow(deprecated)]
        server.configure(&InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::from_file_path(&root).expect("workspace URI should build"),
                name: "app".to_owned(),
            }]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        });

        let edit = server
            .will_rename_files_edit(&RenameFilesParams {
                files: vec![FileRename {
                    old_uri: Url::from_file_path(&dep_path)
                        .expect("old URI should build")
                        .to_string(),
                    new_uri: Url::from_file_path(&next_path)
                        .expect("new URI should build")
                        .to_string(),
                }],
            })
            .expect("rename should produce import edit");

        let changes = edit.changes.expect("edit should use plain changes");
        assert_eq!(changes.len(), 1);
        let (edit_uri, edits) = changes.iter().next().expect("index edit should exist");
        let edit_path = edit_uri
            .to_file_path()
            .expect("edit URI should be a file path");
        assert!(paths_match(&edit_path, &index_path));
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].range.start, Position::new(0, 18));
        assert_eq!(edits[0].range.end, Position::new(0, 25));
        assert_eq!(edits[0].new_text, "\"./renamed\"");
    }

    #[test]
    fn will_rename_files_updates_imports_for_renamed_folders() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src/lib")).expect("lib dir should be created");
        let index_path = root.join("src/index.ms");
        let old_dir = root.join("src/lib");
        let new_dir = root.join("src/moved");
        fs::write(&index_path, "let dep := import \"./lib/dep\";\n")
            .expect("index should be written");
        fs::write(old_dir.join("dep.ms"), "export let value := 1;\n")
            .expect("dep should be written");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        #[allow(deprecated)]
        server.configure(&InitializeParams {
            process_id: None,
            root_path: None,
            root_uri: None,
            initialization_options: None,
            capabilities: ClientCapabilities::default(),
            trace: None,
            workspace_folders: Some(vec![WorkspaceFolder {
                uri: Url::from_file_path(&root).expect("workspace URI should build"),
                name: "app".to_owned(),
            }]),
            client_info: None,
            locale: None,
            work_done_progress_params: WorkDoneProgressParams::default(),
        });

        let edit = server
            .will_rename_files_edit(&RenameFilesParams {
                files: vec![FileRename {
                    old_uri: Url::from_file_path(&old_dir)
                        .expect("old URI should build")
                        .to_string(),
                    new_uri: Url::from_file_path(&new_dir)
                        .expect("new URI should build")
                        .to_string(),
                }],
            })
            .expect("folder rename should produce import edit");

        let changes = edit.changes.expect("edit should use plain changes");
        assert_eq!(changes.len(), 1);
        let (edit_uri, edits) = changes.iter().next().expect("index edit should exist");
        let edit_path = edit_uri
            .to_file_path()
            .expect("edit URI should be a file path");
        assert!(paths_match(&edit_path, &index_path));
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "\"./moved/dep\"");
    }

    #[test]
    fn will_rename_files_infers_project_root_from_open_document() {
        let root = temp_project();
        fs::write(
            root.join("musi.json"),
            r#"{
  "name": "app",
  "version": "0.1.0",
  "entry": "src/index.ms"
}
"#,
        )
        .expect("manifest should be written");
        fs::create_dir_all(root.join("src")).expect("src dir should be created");
        let index_path = root.join("src/index.ms");
        let dep_path = root.join("src/dep.ms");
        let next_path = root.join("src/renamed.ms");
        fs::write(&index_path, "let dep := import \"./dep\";\n").expect("index should be written");
        fs::write(&dep_path, "export let value := 1;\n").expect("dep should be written");
        let uri = Url::from_file_path(&index_path).expect("index URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server
            .open_documents
            .insert(uri, "let dep := import \"./dep\";\n".to_owned());

        let edit = server
            .will_rename_files_edit(&RenameFilesParams {
                files: vec![FileRename {
                    old_uri: Url::from_file_path(&dep_path)
                        .expect("old URI should build")
                        .to_string(),
                    new_uri: Url::from_file_path(&next_path)
                        .expect("new URI should build")
                        .to_string(),
                }],
            })
            .expect("rename should produce import edit");

        let changes = edit.changes.expect("edit should use plain changes");
        let (edit_uri, edits) = changes.iter().next().expect("index edit should exist");
        let edit_path = edit_uri
            .to_file_path()
            .expect("edit URI should be a file path");
        assert!(paths_match(&edit_path, &index_path));
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, "\"./renamed\"");
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
            encode_semantic_tokens("let\n  binding", &tokens, None),
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
    fn semantic_token_encoding_uses_utf16_positions() {
        let tokens = vec![ToolSemanticToken::new(
            ToolRange::new(1, 18, 1, 21),
            ToolSemanticTokenKind::Variable,
            Vec::new(),
        )];

        assert_eq!(
            encode_semantic_tokens("let icon := \"\u{1F600}\"; bef", &tokens, None),
            vec![SemanticToken {
                delta_line: 0,
                delta_start: 18,
                length: 3,
                token_type: 4,
                token_modifiers_bitset: 0,
            }]
        );
    }

    #[test]
    fn semantic_token_delta_returns_edits_for_cached_result_id() {
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
        let source = "let value := 1;\n";
        fs::write(&path, source).expect("entry should be written");
        let uri = Url::from_file_path(&path).expect("file URI should build");
        let mut server = MusiLanguageServer::new(ClientSocket::new_closed());
        let _ = server.open_documents.insert(uri.clone(), source.to_owned());

        let full = server
            .semantic_tokens_full_response(&SemanticTokensParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("full semantic tokens should run");
        let previous_result_id = full
            .result_id
            .expect("full tokens should include result id");
        let _ = server.open_documents.insert(
            uri.clone(),
            "let value := 1;\nlet other := value;\n".to_owned(),
        );

        let response = server
            .semantic_token_delta(&SemanticTokensDeltaParams {
                text_document: TextDocumentIdentifier { uri },
                previous_result_id,
                work_done_progress_params: WorkDoneProgressParams::default(),
                partial_result_params: PartialResultParams::default(),
            })
            .expect("semantic token delta should run");

        let SemanticTokensFullDeltaResult::TokensDelta(delta) = response else {
            panic!("semantic token delta expected");
        };
        assert!(delta.result_id.is_some());
        assert_eq!(delta.edits.len(), 1);
        assert_eq!(delta.edits[0].start % 5, 0);
        assert_eq!(delta.edits[0].delete_count % 5, 0);
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
        assert_eq!(
            converted
                .data
                .as_ref()
                .and_then(|data| data.get("phase"))
                .and_then(serde_json::Value::as_str),
            None
        );
    }

    #[test]
    fn lsp_diagnostic_preserves_hint_and_notes_in_data() {
        let diagnostic = CliDiagnostic::new("sema", "error", "return value expected `Int`")
            .with_hint(Some("return integer value".to_owned()))
            .with_notes(vec!["function body returned `String`".to_owned()]);

        let converted = to_lsp_diagnostic(diagnostic);
        let data = converted.data.expect("diagnostic data should exist");

        assert_eq!(
            data.get("phase").and_then(serde_json::Value::as_str),
            Some("sema")
        );
        assert_eq!(
            data.get("hint").and_then(serde_json::Value::as_str),
            Some("return integer value")
        );
        assert_eq!(
            data.get("notes")
                .and_then(serde_json::Value::as_array)
                .and_then(|notes| notes.first())
                .and_then(serde_json::Value::as_str),
            Some("function body returned `String`")
        );
    }

    #[test]
    fn inlay_hint_conversion_uses_lsp_kind_and_padding() {
        let mut tool_hint = ToolInlayHint::new(
            ToolPosition::new(2, 5),
            "value:",
            ToolInlayHintKind::Parameter,
        );
        tool_hint.tooltip = Some("parameter `value`".to_owned());
        let hint = to_lsp_inlay_hint("one\nvalue", tool_hint);

        assert_eq!(hint.position, Position::new(1, 4));
        assert!(matches!(hint.kind, Some(InlayHintKind::PARAMETER)));
        assert_eq!(hint.padding_right, Some(true));
        assert!(hint.tooltip.is_none());
        assert!(hint.data.is_some());

        let hint = resolve_lsp_inlay_hint(hint);

        assert!(matches!(
            hint.tooltip,
            Some(InlayHintTooltip::String(ref tooltip)) if tooltip == "parameter `value`"
        ));
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
