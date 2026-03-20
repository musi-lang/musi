use async_lsp::MainLoop;
use async_lsp::concurrency::ConcurrencyLayer;
use async_lsp::panic::CatchUnwindLayer;
use async_lsp::router::Router;
use async_lsp::server::LifecycleLayer;
use tower::ServiceBuilder;

use msc_lsp::backend::MusiBackend;
use msc_lsp::handlers::test_discovery::DiscoverTests;

#[tokio::main]
async fn main() {
    let (mainloop, _) = MainLoop::new_server(|client| {
        let state = MusiBackend::new(client);
        let mut router = Router::from_language_server(state);
        router.request::<DiscoverTests, _>(|state, params| {
            let result = state.handle_discover_tests(params);
            async move { Ok(result) }
        });
        ServiceBuilder::new()
            .layer(LifecycleLayer::default())
            .layer(CatchUnwindLayer::default())
            .layer(ConcurrencyLayer::default())
            .service(router)
    });
    let stdin = async_lsp::stdio::PipeStdin::lock_tokio().expect("failed to lock stdin");
    let stdout = async_lsp::stdio::PipeStdout::lock_tokio().expect("failed to lock stdout");
    mainloop
        .run_buffered(stdin, stdout)
        .await
        .expect("main loop failed");
}
