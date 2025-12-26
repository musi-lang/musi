# musi_lsp Rewrite: lsp-server Migration

Replace `async-lsp` with `lsp-server` using a clean, high-level wrapper architecture.

## Architecture

```text
crates/musi_lsp/src/
├── main.rs           # Entry point
├── server.rs         # LspServer: connection + run loop
├── dispatch.rs       # Route method → handler
├── handlers.rs       # Handler implementations
├── state.rs          # GlobalState: interner, documents
└── diagnostics.rs    # Musi → LSP conversion
```

## Dependencies

```toml
[dependencies]
lsp-server = "0.7.9"
lsp-types = "0.97"
crossbeam-channel = "0.5.15"
serde_json = "1.0.147"
serde = { version = "1.0.228", features = ["derive"] }
anyhow = { workspace = true }
tracing = { workspace = true }
tracing-subscriber = { workspace = true, features = ["env-filter"] }
```

## Files

### main.rs

```rust
fn main() -> Result<()> {
    let (conn, io) = Connection::stdio();
    LspServer::new(conn).run()?;
    io.join()?;
    Ok(())
}
```

### server.rs

- `LspServer` struct with `Connection`
- `run()`: initialize handshake + message loop
- Delegates to `dispatch.rs`

### dispatch.rs

- `dispatch_request(state, req) -> Response`
- `dispatch_notification(state, notif)`
- Generic routing, no business logic

### handlers.rs

- `initialize(state, params) -> InitializeResult`
- `did_open(state, params)`
- `did_change(state, params)`
- Pure functions, no LSP plumbing

### state.rs

```rust
pub struct GlobalState {
    pub interner: Interner,
    pub documents: HashMap<Uri, SourceFile>,
    pub sender: Sender<Message>,
}
```

### diagnostics.rs

- `convert_diagnostics(source, musi_diags) -> Vec<Diagnostic>`
- Keep existing logic

## Principles

| Principle | Application |
|-----------|-------------|
| SRP | Each file/function has one responsibility |
| DRY | Generic dispatch, shared state |
| KISS | Simple sync loop, no async |
