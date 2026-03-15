mod analysis;
mod backend;
mod code_actions;
mod code_lens;
mod completion;
mod document_links;
mod document_symbols;
mod goto_def;
mod hover;
mod inlay_hints;
mod references;
mod semantic_tokens;
mod signature_help;

use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::MainLoop;
use tower::ServiceBuilder;

use backend::MusiBackend;

#[tokio::main]
async fn main() {
    let (mainloop, _) = MainLoop::new_server(|client| {
        let state = MusiBackend::new(client);
        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .service(Router::from_language_server(state))
    });
    let stdin = async_lsp::stdio::PipeStdin::lock_tokio()
        .expect("failed to lock stdin");
    let stdout = async_lsp::stdio::PipeStdout::lock_tokio()
        .expect("failed to lock stdout");
    mainloop.run_buffered(stdin, stdout).await
        .expect("main loop failed");
}
