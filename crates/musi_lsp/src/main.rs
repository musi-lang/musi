mod bindings;
mod completion;
mod diagnostics;
mod doc_store;
mod semantic_tokens;

use std::collections::HashMap;
use std::io;
use std::sync::Arc;

use bindings::{BindingCollector, BindingInfo, find_binding_at};
use doc_store::DocumentStore;
use musi_ast::{AstArena, Prog};
use musi_core::{Interner, SourceFile, Symbol};
use tokio::io::{stdin, stdout};
use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::{
    CompletionOptions, CompletionParams, CompletionResponse, DidChangeTextDocumentParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability,
    InitializeParams, InitializeResult, InitializedParams, Location, MarkedString, MessageType,
    OneOf, Position, Range, ReferenceParams, RenameParams, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensOptions, SemanticTokensParams, SemanticTokensResult,
    SemanticTokensServerCapabilities, ServerCapabilities, ServerInfo, TextDocumentPositionParams,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Uri, WorkspaceEdit,
};
use tower_lsp_server::{Client, LanguageServer, LspService, Server};

use crate::diagnostics::offset_to_position;

#[derive(Debug)]
struct Backend {
    client: Client,
    documents: Arc<RwLock<DocumentStore>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(DocumentStore::new())),
        }
    }

    async fn diagnose(&self, uri: &Uri, text: &str) {
        let uri_path = uri_to_path(uri);
        let diagnostics = diagnostics::run_diagnostics(&uri_path, text);
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

fn uri_to_path(uri: &Uri) -> String {
    format!("{uri:?}")
}

struct ParsedDoc {
    arena: AstArena,
    prog: Prog,
    bindings: HashMap<Symbol, BindingInfo>,
    interner: Interner,
}

fn parse_document(uri_path: &str, text: &str) -> Option<ParsedDoc> {
    let mut interner = Interner::new();
    let source = SourceFile::new(uri_path.into(), text.into(), 0);
    let (tokens, lex_errors) = musi_lex::tokenize(&source, &mut interner, true);

    if lex_errors.errors > 0 {
        return None;
    }

    let parse_result = musi_parse::parse(&tokens, &interner);
    if parse_result.diagnostics.errors > 0 {
        return None;
    }

    let collector = BindingCollector::new(&parse_result.arena);
    let bindings = collector.collect(&parse_result.prog);

    Some(ParsedDoc {
        arena: parse_result.arena,
        prog: parse_result.prog,
        bindings,
        interner,
    })
}

fn position_to_offset(pos: Position, text: &str) -> u32 {
    let mut byte_offset = 0u32;
    let mut current_line = 0u32;
    let mut current_col = 0u32;

    for ch in text.chars() {
        if current_line == pos.line && current_col == pos.character {
            break;
        }
        let char_len: u32 = ch.len_utf8().try_into().expect("char len overflow");
        byte_offset += char_len;
        if ch == '\n' {
            current_line += 1;
            current_col = 0;
        } else {
            current_col += 1;
        }
    }
    byte_offset
}

fn span_to_range(span: musi_core::Span, text: &str) -> Range {
    let start = offset_to_position(span.lo.try_into().expect("span.lo overflow"), text);
    let end = offset_to_position(span.hi.try_into().expect("span.hi overflow"), text);
    Range { start, end }
}

impl LanguageServer for Backend {
    async fn initialize(&self, _params: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "musi_lsp".to_owned(),
                version: Some(env!("CARGO_PKG_VERSION").to_owned()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            legend: semantic_tokens::legend(),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                            range: None,
                            ..Default::default()
                        },
                    ),
                ),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                rename_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions::default()),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Musi LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        self.documents.write().await.open(uri.clone(), text.clone());
        self.diagnose(&uri, &text).await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        self.documents.write().await.close(&uri);
        self.client.publish_diagnostics(uri, vec![], None).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().next() {
            self.documents
                .write()
                .await
                .update(&uri, change.text.clone());
            self.diagnose(&uri, &change.text).await;
        }
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let uri_path = uri_to_path(&uri);
        let text = {
            let store = self.documents.read().await;
            store.get(&uri).cloned()
        };

        let Some(text) = text else {
            return Ok(None);
        };

        let parsed = parse_document(&uri_path, &text);
        let tokens = parsed
            .map(|p| semantic_tokens::compute_tokens(&text, &p.arena, &p.prog))
            .unwrap_or_default();

        Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        })))
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let loc = self
            .find_definition(&params.text_document_position_params)
            .await;
        Ok(loc.map(GotoDefinitionResponse::Scalar))
    }

    async fn references(&self, params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        self.find_references(&params.text_document_position).await
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        self.get_hover(&params.text_document_position_params).await
    }

    async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
        self.do_rename(&params.text_document_position, &params.new_name)
            .await
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        self.get_completions(&params.text_document_position).await
    }
}

impl Backend {
    async fn find_definition(&self, params: &TextDocumentPositionParams) -> Option<Location> {
        let (uri, text) = self.get_doc(&params.text_document.uri).await?;
        let uri_path = uri_to_path(&uri);
        let parsed = parse_document(&uri_path, &text)?;
        let offset = position_to_offset(params.position, &text);
        let (_sym, info) = find_binding_at(&parsed.bindings, offset)?;
        Some(Location {
            uri,
            range: span_to_range(info.def_span, &text),
        })
    }

    async fn find_references(
        &self,
        params: &TextDocumentPositionParams,
    ) -> Result<Option<Vec<Location>>> {
        let Some((uri, text)) = self.get_doc(&params.text_document.uri).await else {
            return Ok(None);
        };
        let uri_path = uri_to_path(&uri);
        let Some(parsed) = parse_document(&uri_path, &text) else {
            return Ok(None);
        };
        let offset = position_to_offset(params.position, &text);
        let Some((_sym, info)) = find_binding_at(&parsed.bindings, offset) else {
            return Ok(None);
        };

        let mut locs = vec![Location {
            uri: uri.clone(),
            range: span_to_range(info.def_span, &text),
        }];
        for usage in &info.usages {
            locs.push(Location {
                uri: uri.clone(),
                range: span_to_range(*usage, &text),
            });
        }
        Ok(Some(locs))
    }

    async fn get_hover(&self, params: &TextDocumentPositionParams) -> Result<Option<Hover>> {
        let Some((uri, text)) = self.get_doc(&params.text_document.uri).await else {
            return Ok(None);
        };
        let uri_path = uri_to_path(&uri);
        let Some(parsed) = parse_document(&uri_path, &text) else {
            return Ok(None);
        };
        let offset = position_to_offset(params.position, &text);
        let Some((_sym, info)) = find_binding_at(&parsed.bindings, offset) else {
            return Ok(None);
        };

        let kind_str = match info.kind {
            bindings::BindingKind::ValBinding => "val (readonly)",
            bindings::BindingKind::VarBinding => "var (mutable)",
            bindings::BindingKind::Function => "function",
            bindings::BindingKind::TypeDef => "type",
            bindings::BindingKind::Parameter { mutable } => {
                if mutable {
                    "parameter (mutable)"
                } else {
                    "parameter"
                }
            }
        };

        Ok(Some(Hover {
            contents: HoverContents::Scalar(MarkedString::String(kind_str.to_owned())),
            range: None,
        }))
    }

    async fn do_rename(
        &self,
        params: &TextDocumentPositionParams,
        new_name: &str,
    ) -> Result<Option<WorkspaceEdit>> {
        let Some((uri, text)) = self.get_doc(&params.text_document.uri).await else {
            return Ok(None);
        };
        let uri_path = uri_to_path(&uri);
        let Some(parsed) = parse_document(&uri_path, &text) else {
            return Ok(None);
        };
        let offset = position_to_offset(params.position, &text);
        let Some((_sym, info)) = find_binding_at(&parsed.bindings, offset) else {
            return Ok(None);
        };

        let mut edits = vec![TextEdit {
            range: span_to_range(info.def_span, &text),
            new_text: new_name.to_owned(),
        }];
        for usage in &info.usages {
            edits.push(TextEdit {
                range: span_to_range(*usage, &text),
                new_text: new_name.to_owned(),
            });
        }

        let mut changes = HashMap::new();
        let _ = changes.insert(uri, edits);
        Ok(Some(WorkspaceEdit {
            changes: Some(changes),
            ..Default::default()
        }))
    }

    async fn get_completions(
        &self,
        params: &TextDocumentPositionParams,
    ) -> Result<Option<CompletionResponse>> {
        let Some((uri, text)) = self.get_doc(&params.text_document.uri).await else {
            return Ok(None);
        };
        let uri_path = uri_to_path(&uri);
        let Some(parsed) = parse_document(&uri_path, &text) else {
            return Ok(Some(CompletionResponse::Array(
                completion::build_completions(&HashMap::new(), &Interner::new()),
            )));
        };

        let items = completion::build_completions(&parsed.bindings, &parsed.interner);
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn get_doc(&self, uri: &Uri) -> Option<(Uri, String)> {
        let store = self.documents.read().await;
        store.get(uri).cloned().map(|text| (uri.clone(), text))
    }
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_writer(io::stderr)
        .with_ansi(false)
        .init();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin(), stdout(), socket).serve(service).await;
}
