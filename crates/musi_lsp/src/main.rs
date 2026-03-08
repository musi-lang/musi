mod analysis;
mod backend;
mod code_actions;
mod completion;
mod document_links;
mod document_symbols;
mod goto_def;
mod hover;
mod inlay_hints;
mod references;
mod semantic_tokens;
mod signature_help;

use backend::MusiBackend;
use tower_lsp_server::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(MusiBackend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
