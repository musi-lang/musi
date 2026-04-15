use async_lsp::Result as LspResult;
use musi_lsp::run_stdio_server;

fn main() -> LspResult<()> {
    run_stdio_server()
}
