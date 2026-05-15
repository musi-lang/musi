use async_lsp::lsp_types::{
    CallHierarchyServerCapability, CodeActionKind, CodeActionOptions, CodeActionProviderCapability,
    CodeLensOptions, CompletionOptions, DeclarationCapability, DiagnosticOptions,
    DiagnosticServerCapabilities, DocumentLinkOptions, DocumentOnTypeFormattingOptions,
    ExecuteCommandOptions, FileOperationFilter, FileOperationPattern, FileOperationPatternKind,
    FileOperationRegistrationOptions, FoldingRangeProviderCapability, HoverProviderCapability,
    ImplementationProviderCapability, InitializeResult, InlayHintOptions,
    InlayHintServerCapabilities, LinkedEditingRangeServerCapabilities, OneOf, RenameOptions,
    SelectionRangeProviderCapability, SemanticTokensFullOptions, SemanticTokensOptions,
    SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo, SignatureHelpOptions,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions,
    TextDocumentSyncSaveOptions, TypeDefinitionProviderCapability, WorkDoneProgressOptions,
    WorkspaceFileOperationsServerCapabilities, WorkspaceFoldersServerCapabilities,
    WorkspaceServerCapabilities, WorkspaceSymbolOptions,
};

use super::convert::semantic_tokens_legend;

#[allow(clippy::too_many_lines)]
pub(super) fn initialize_result(references_command: &str) -> InitializeResult {
    InitializeResult {
        capabilities: ServerCapabilities {
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    will_save: None,
                    will_save_wait_until: None,
                    save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                },
            )),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
            signature_help_provider: Some(SignatureHelpOptions {
                trigger_characters: Some(vec!["(".to_owned(), ",".to_owned()]),
                retrigger_characters: Some(vec![",".to_owned()]),
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            }),
            declaration_provider: Some(DeclarationCapability::Simple(true)),
            definition_provider: Some(OneOf::Left(true)),
            type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
            implementation_provider: Some(ImplementationProviderCapability::Simple(true)),
            references_provider: Some(OneOf::Left(true)),
            moniker_provider: Some(OneOf::Left(true)),
            call_hierarchy_provider: Some(CallHierarchyServerCapability::Simple(true)),
            linked_editing_range_provider: Some(LinkedEditingRangeServerCapabilities::Simple(true)),
            document_highlight_provider: Some(OneOf::Left(true)),
            document_symbol_provider: Some(OneOf::Left(true)),
            document_link_provider: Some(DocumentLinkOptions {
                resolve_provider: Some(true),
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            }),
            code_lens_provider: Some(CodeLensOptions {
                resolve_provider: Some(true),
            }),
            execute_command_provider: Some(ExecuteCommandOptions {
                commands: vec![references_command.to_owned()],
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            }),
            workspace: Some(WorkspaceServerCapabilities {
                workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                    supported: Some(true),
                    change_notifications: Some(OneOf::Left(true)),
                }),
                file_operations: Some(WorkspaceFileOperationsServerCapabilities {
                    will_rename: Some(FileOperationRegistrationOptions {
                        filters: vec![
                            FileOperationFilter {
                                scheme: Some("file".to_owned()),
                                pattern: FileOperationPattern {
                                    glob: "**/*.ms".to_owned(),
                                    matches: Some(FileOperationPatternKind::File),
                                    options: None,
                                },
                            },
                            FileOperationFilter {
                                scheme: Some("file".to_owned()),
                                pattern: FileOperationPattern {
                                    glob: "**".to_owned(),
                                    matches: Some(FileOperationPatternKind::Folder),
                                    options: None,
                                },
                            },
                        ],
                    }),
                    ..WorkspaceFileOperationsServerCapabilities::default()
                }),
            }),
            diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
                identifier: Some("musi".to_owned()),
                inter_file_dependencies: true,
                workspace_diagnostics: false,
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
            selection_range_provider: Some(SelectionRangeProviderCapability::Simple(true)),
            workspace_symbol_provider: Some(OneOf::Right(WorkspaceSymbolOptions {
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
                resolve_provider: Some(true),
            })),
            code_action_provider: Some(CodeActionProviderCapability::Options(CodeActionOptions {
                code_action_kinds: Some(vec![CodeActionKind::SOURCE_ORGANIZE_IMPORTS]),
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
                resolve_provider: Some(true),
            })),
            rename_provider: Some(OneOf::Right(RenameOptions {
                prepare_provider: Some(true),
                work_done_progress_options: WorkDoneProgressOptions {
                    work_done_progress: None,
                },
            })),
            document_formatting_provider: Some(OneOf::Left(true)),
            document_range_formatting_provider: Some(OneOf::Left(true)),
            document_on_type_formatting_provider: Some(DocumentOnTypeFormattingOptions {
                first_trigger_character: ";".to_owned(),
                more_trigger_character: Some(vec![")".to_owned(), "]".to_owned(), "}".to_owned()]),
            }),
            completion_provider: Some(CompletionOptions {
                resolve_provider: Some(true),
                trigger_characters: Some(vec![
                    ".".to_owned(),
                    "\"".to_owned(),
                    "/".to_owned(),
                    "@".to_owned(),
                ]),
                ..CompletionOptions::default()
            }),
            inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
                InlayHintOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    resolve_provider: Some(true),
                },
            ))),
            semantic_tokens_provider: Some(
                SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                    work_done_progress_options: WorkDoneProgressOptions {
                        work_done_progress: None,
                    },
                    legend: semantic_tokens_legend(),
                    range: Some(true),
                    full: Some(SemanticTokensFullOptions::Delta { delta: Some(true) }),
                }),
            ),
            ..ServerCapabilities::default()
        },
        server_info: Some(ServerInfo {
            name: "musi_lsp".to_owned(),
            version: None,
        }),
    }
}
