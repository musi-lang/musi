mod server;

use async_lsp::router::Router;
use async_lsp::server::Lifecycle;
use async_lsp::stdio::{PipeStdin, PipeStdout};
use async_lsp::{Error as LspError, MainLoop, Result as LspResult};
use tokio::runtime::Builder;

pub use server::MusiLanguageServer;

/// Starts the Musi language server over standard input and output.
///
/// # Errors
///
/// Returns an LSP error when the Tokio runtime, standard streams, or buffered server loop fails.
pub fn run_stdio_server() -> LspResult<()> {
    Builder::new_current_thread()
        .enable_all()
        .build()
        .map_err(LspError::from)?
        .block_on(async {
            let stdin = PipeStdin::lock_tokio()?;
            let stdout = PipeStdout::lock_tokio()?;
            let (main_loop, _) = MainLoop::new_server(|client| {
                Lifecycle::new(Router::from_language_server(MusiLanguageServer::new(
                    client,
                )))
            });
            main_loop.run_buffered(stdin, stdout).await
        })
}
