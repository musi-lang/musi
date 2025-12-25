use async_lsp::{MainLoop, router::Router, server::Lifecycle};
use tokio::io;
use tokio_util::compat::{TokioAsyncReadCompatExt, TokioAsyncWriteCompatExt};

mod diagnostics;
mod handlers;
mod server_state;

use handlers::MusiLanguageServer;
use server_state::ServerState;

#[tokio::main]
async fn main() {
    let (stdin, stdout) = (io::stdin(), io::stdout());
    let (stdin2, stdout2) = (stdin.compat(), stdout.compat_write());

    let (loop_node, _) = MainLoop::new_server(|client| {
        let state = ServerState::new(client);
        let server = MusiLanguageServer::new(state);
        Lifecycle::new(Router::from_language_server(server))
    });

    if let Err(e) = loop_node.run_buffered(stdin2, stdout2).await {
        eprintln!("LSP server error: {e}");
    }
}
