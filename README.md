# Musi

Musi is an expression-first programming language with typed effects, a SEAM bytecode pipeline, and package tooling built around `.ms` source files.

> [!WARNING]
> Musi is `v0.1.0-alpha.1`. Language, tooling, and stdlib shape will still change.

## Overview

The repo ships two user-facing binaries:

| Binary  | Lane    | What it does                                   |
| ------- | ------- | ---------------------------------------------- |
| `musi`  | package | manifest, workspace, run, build, and test flow |
| `music` | direct  | single-file `.ms` and `.seam` artifact work    |

Core surface:

- `musi:...` is compiler-owned foundation and runtime capability space.
- `@std/<family>` is the first-party standard library surface.
- `@std` re-exports stdlib families from its root module.
- `*.test.ms` files export `test`; `musi test` finds them recursively, including under `__tests__/`.

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
musi init hello
cd hello
musi run
musi test
```

`musi init` creates a small project:

```text
hello/
  musi.json
  index.ms
  __tests__/add.test.ms
  .gitignore
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

## Commands

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
music info index
music disasm index
music run index.seam
```

Use `musi` inside package roots. Use `music` when you want one source graph or one artifact.

## Imports and stdlib

Prefer focused stdlib imports:

```musi
let option := import "@std/option";
let testing := import "@std/testing";
```

Root import also works:

```musi
let std := import "@std";
let option := std.option;
let testing := std.testing;
let os := std.os;
```

The root module is a barrel: focused aliases stay available without adding extra grouping files.

Foundation host modules stay separate from stdlib:

```musi
let Core := import "musi:core";
let Io := import "musi:io";
let Fs := import "musi:fs";
```

Reach for `@std` first in ordinary application code. Reach for `musi:*` only when you are working at language, runtime, or integration boundaries.

## Project layout

Key repo areas:

- `crates/` — Rust compiler, runtime, tooling, package, and CLI crates
- `packages/` — first-party Musi packages, including `@std`
- `diagnostics/` — diagnostic fixtures and renderer references
- `docs/reference/` — compiler, runtime, diagnostics, and language coverage references
- `docs/reference/performance.md` — VM/runtime benchmark tracking across CLR, JVM, and SEAM
- `docs/where/` — workspace and ownership maps
- `grammar/` — grammar sources

Good entry points:

- `docs/where/workspace-map.md`
- `docs/reference/public-api.md`
- `docs/reference/language-feature-coverage.md`
- `docs/reference/performance.md`
- `docs/reference/diagnostics.md`
- `grammar/MusiParser.g4`
- `grammar/MusiLexer.g4`
- `grammar/Musi.abnf`

## Testing and validation

Common commands:

```bash
make lint
make check
cargo test -p music_syntax
cargo test -p music_sema
```

Prefer targeted crate tests over `cargo test --workspace` on lower-memory machines.

## Contributing

See [CONTRIBUTING.md](CONTRIBUTING.md) for workflow and validation guidance.

All contributors must follow [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).

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
