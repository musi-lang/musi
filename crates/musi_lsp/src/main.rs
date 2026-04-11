use async_lsp::router::Router;
use async_lsp::server::Lifecycle;
use async_lsp::stdio::{PipeStdin, PipeStdout};
use async_lsp::{MainLoop, Result as LspResult};
use musi_lsp::MusiLanguageServer;

#[tokio::main(flavor = "current_thread")]
async fn main() -> LspResult<()> {
    let stdin = PipeStdin::lock_tokio()?;
    let stdout = PipeStdout::lock_tokio()?;
    let (main_loop, _) = MainLoop::new_server(|client| {
        Lifecycle::new(Router::from_language_server(MusiLanguageServer::new(
            client,
        )))
    });
    main_loop.run_buffered(stdin, stdout).await
}
