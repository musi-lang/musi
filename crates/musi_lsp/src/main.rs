mod backend;
mod diagnostics;
mod semantic_tokens;

use backend::MusiBackend;
use tower_lsp_server::{LspService, Server};

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();
    let (service, socket) = LspService::new(|client| MusiBackend::new(client));
    Server::new(stdin, stdout, socket).serve(service).await;
}
