mod diagnostics;
mod dispatch;
mod folding;
mod handlers;
mod server;
mod state;
mod symbols;
mod tokens;
mod types;

use anyhow::Result;
use lsp_server::Connection;
use server::LspServer;

fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let (conn, io_threads) = Connection::stdio();
    let mut server = LspServer::new(conn);
    server.run()?;
    io_threads.join()?;
    Ok(())
}
