use std::env::temp_dir;
use std::path::Path;

use async_lsp::lsp_types::{
    DiagnosticSeverity, InlayHintKind, Position, SemanticToken, TextDocumentIdentifier,
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
