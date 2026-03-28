# Musi

A programming language with a type system, functional features, and a stack-based bytecode VM.

> [!WARNING]
> Musi is `v0.1.0-rc1`. The language, tooling, and standard library will have breaking changes. Do not use it for anything you can't afford to rewrite.

## What Musi Is

Musi source files use the `.ms` extension. The toolchain has two binaries:

| Binary | What it does                                                              |
| ------ | ------------------------------------------------------------------------- |
| `musi` | Universal driver - run, check, build, test, format, lint, manage projects |
| `msc`  | Standalone compiler - type-check and compile without the VM               |

You will mostly use `musi`. The `msc` binary is for compiler-only workflows (CI type-checking, producing `.seam` bytecode without running it).

## Prerequisites

### 1. Rust 1.87 or newer

```bash
# install
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# or update
rustup update stable

# verify
rustc --version
```

### 2. libffi

Musi uses libffi for its foreign function interface.

**macOS:**

```bash
brew install libffi
```

**Ubuntu / Debian:**

```bash
sudo apt install libffi-dev
```

**Fedora / RHEL:**

```bash
sudo dnf install libffi-devel
```

### 3. Git

You probably already have this. If not: <https://git-scm.com/downloads>

## Install from Source

There are no pre-built binaries yet.

```bash
git clone https://github.com/musi-lang/musi.git
cd musi
cargo build --release
```

Binaries land in `./target/release/`:

- `musi` (toolchain driver)
- `msc` (standalone compiler)
- `msc_lsp` (LSP server)

Add to your PATH:

```bash
export PATH="/path/to/musi/target/release:$PATH"
```

## Quick Start

```bash
musi new hello
cd hello
```

This creates:

```text
hello/
  musi.json     project manifest
  index.ms      source file
  .gitignore
```

Look at the generated `index.ms`:

```musi
import "@std/rt" as rt;

rt.writeln("hello, world!");
```

Run it:

```bash
musi run
```

Other commands:

```bash
musi check              # type-check without running
musi build              # compile to .seam bytecode
musi exec index.seam    # run compiled bytecode directly
musi test               # discover and run *.test.ms files
musi fmt                # format source files
musi lint               # lint source files
```

## Import Namespaces

- `@std/...` is the external standard library package namespace. In local development this is usually mapped to the sibling `musi-lang/std` checkout through `musi.json`.
- `musi:...` is the public compiler-owned intrinsic namespace, analogous to `node:` or `bun:`.
- Prelude names such as builtin types and core classes are injected by the compiler. They are not loaded from `@std`.

Test files export `test`, not `suite`. `musi test` invokes that exported entrypoint and expects it to return the root suite node.

## A Taste of the Language

TODO

## Project Structure

| Crate          | Role                                          |
| -------------- | --------------------------------------------- |
| `msc`          | Compiler library + CLI                        |
| `musi`         | Toolchain driver CLI                          |
| `msc_lsp`      | LSP server                                    |
| `msc_shared`   | Spans, source database, interner, diagnostics |
| `msc_lex`      | Lexer                                         |
| `msc_ast`      | AST node types                                |
| `msc_parse`    | Parser                                        |
| `msc_sema`     | Semantic analysis / type-checker              |
| `msc_resolve`  | Module resolution                             |
| `msc_emit`     | Bytecode emitter                              |
| `msc_bc`       | Bytecode format definitions                   |
| `msc_vm`       | Bytecode interpreter / VM                     |
| `msc_builtins` | Standard library runtime + FFI                |
| `msc_manifest` | `musi.json` parser                            |

## Editor Support

The `msc_lsp` binary is built alongside the compiler. Point your editor's LSP client at it. A VS Code extension is in development at `tools/vscode/`.

## Testing

```bash
cargo test -p msc_parse        # test a specific crate
cargo clippy -p msc_parse      # lint a specific crate
musi test                      # run Musi stdlib tests
```

Avoid `cargo test --workspace` on machines with less than 16 GB of free RAM.

See [CONTRIBUTING.md](CONTRIBUTING.md) for the full development guide.

## Contributing

Contributions welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for workflow, coding guidelines, and the PR checklist.

## Code of Conduct

All contributors are expected to follow the [Code of Conduct](CODE_OF_CONDUCT.md).

## Star History

<a href="https://www.star-history.com/?repos=musi-lang%2Fmusi&type=date&logscale=&legend=top-left">
 <picture>
   <source media="(prefers-color-scheme: dark)" srcset="https://api.star-history.com/image?repos=musi-lang/musi&type=date&theme=dark&logscale&legend=top-left" />
   <source media="(prefers-color-scheme: light)" srcset="https://api.star-history.com/image?repos=musi-lang/musi&type=date&logscale&legend=top-left" />
   <img alt="Star History Chart" src="https://api.star-history.com/image?repos=musi-lang/musi&type=date&logscale&legend=top-left" />
 </picture>
</a>

## License

[MIT OR Apache-2.0](LICENSE)
