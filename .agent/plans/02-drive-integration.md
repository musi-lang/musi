# Plan 2: musi_cli Integration

## Goal

Integrate `musi_parse` and `musi_sema` into the CLI driver.

## Changes

### [MODIFY] `musi_cli/Cargo.toml`

Add dependencies:

```toml
musi_parse = { path = "../musi_parse" }
musi_sema = { path = "../musi_sema" }
```

### [MODIFY] `musi_cli/src/cmds.rs`

1. Add imports for `musi_parse` and `musi_sema`
2. Update `check_file()` to:
   - Parse tokens → AST
   - Run semantic analysis
   - Emit diagnostics from all phases
3. Add `EmitKind::Ast` support

### [MODIFY] `musi_cli/src/main.rs`

No changes needed (already has `--emit ast` option).

## Verification

```sh
cargo build -p musi_cli
cargo run -p musi_cli -- check examples/hello.ms
cargo run -p musi_cli -- --emit ast examples/hello.ms
```
