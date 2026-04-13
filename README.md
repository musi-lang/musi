# Musi

Musi is an expression-first programming language with typed effects, a SEAM bytecode pipeline, and a standard library built on `musi:` and `@std`.

> [!WARNING]
> Musi is `v0.1.0-alpha.1`. Language, tooling, and stdlib shape will still change.

## What Musi ships

Musi source files use the `.ms` extension. The repo currently ships two user-facing binaries:

| Binary  | What it does                                  |
| ------- | --------------------------------------------- |
| `music` | direct `.ms` and `.seam` work                 |
| `musi`  | package-aware manifest and workspace workflow |

Core surface:

- `musi:...` is compiler-owned foundation and runtime capability space.
- `@std/<family>` is the first-party standard library surface.
- `@std` re-exports stdlib families from its root module.
- `*.test.ms` files export `test`; `musi test` runs them.

## Install

### Prerequisites

**Rust 1.87 or newer**

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup update stable
rustc --version
```

**libffi**

macOS:

```bash
brew install libffi
```

Ubuntu / Debian:

```bash
sudo apt install libffi-dev
```

Fedora / RHEL:

```bash
sudo dnf install libffi-devel
```

### Install script

macOS / Linux:

```bash
curl -fsSL https://raw.githubusercontent.com/musi-lang/musi/main/install.sh | sh
```

Windows PowerShell:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -Command "irm https://raw.githubusercontent.com/musi-lang/musi/main/install.ps1 | iex"
```

The scripts download the repo archive, then run:

- `cargo install --locked --force --path crates/music`
- `cargo install --locked --force --path crates/musi`

Installed binaries land in Cargo's bin directory:

- macOS / Linux: `~/.cargo/bin`
- Windows: `%USERPROFILE%\.cargo\bin`

Make sure that directory is on `PATH`.

### Install from local clone

```bash
git clone https://github.com/musi-lang/musi.git
cd musi
cargo install --locked --force --path crates/music
cargo install --locked --force --path crates/musi
```

## Quick start

Create a package:

```bash
musi new hello
cd hello
musi run
```

Create a direct scratch file:

```musi
let base := 21;

let twice (x : Int) : Int := x + x;

let answer := twice(base);
answer;
```

Check it:

```bash
music check index.ms
```

## Command lanes

Package lane:

```bash
musi check
musi build
musi run
musi test
```

Direct lane:

```bash
music check index.ms
music build index.ms
music run index.seam
```

Use `musi` inside package roots. Use `music` when you want one file or one artifact.

## Imports and stdlib

Prefer focused stdlib imports:

```musi
let Option := import "@std/option";
let Testing := import "@std/testing";
```

Root import also works:

```musi
let Std := import "@std";
let Option := Std.option;
let Testing := Std.testing;
```

Small current example:

```musi
let Option := import "@std/option";

let configured := Option.some[Int](8080);
let port := Option.unwrapOr[Int](configured, 3000);

port;
```

## Read next

Repo-canonical language docs live under `docs/what/language/`.

Good entry points:

- `docs/what/language/index.md`
- `docs/what/language/start/getting-started.md`
- `docs/what/language/advanced/running-and-tooling.md`
- `grammar/MusiParser.g4`
- `grammar/MusiLexer.g4`
- `grammar/Musi.abnf`

Website source and extension source live here too:

- `www/`
- `vscode-ext/`

## Testing and validation

Common commands:

```bash
make lint
make check
cargo test -p music_syntax
cargo test -p music_sema
rtk bun run --cwd www build
```

Prefer targeted crate tests over `cargo test --workspace` on lower-memory machines.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for workflow, validation, and website/doc guidance.

## Code of Conduct

All contributors must follow [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

## License

[MIT OR Apache-2.0](LICENSE)
