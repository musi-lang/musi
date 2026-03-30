# Musi

A programming language with a type system, effect handling, and a stack-based bytecode VM.

> [!WARNING]
> Musi is `v0.1.0-alpha.1`. The language, tooling, and standard library will have breaking changes. Do not use it for anything you can't afford to rewrite.

## What Musi Is

Musi source files use the `.ms` extension. The current toolchain ships one binary:

| Binary | What it does                              |
| ------ | ----------------------------------------- |
| `musi` | Check, build, run, and test Musi projects |

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

- `musi`

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

Run it:

```bash
musi run
```

Current commands:

```bash
musi check              # type-check without running
musi build              # compile to .seam bytecode
musi run index.seam     # run compiled bytecode directly
musi test               # discover and run *.test.ms files
```

## Import Namespaces

- `@std/...` is the external standard library package namespace. In local development this is usually mapped to the sibling `musi-lang/std` checkout through `musi.json`.
- `musi:...` is the public compiler-owned intrinsic namespace, analogous to `node:` or `bun:`.
- Prelude names such as builtin types and core classes are injected by the compiler. They are not loaded from `@std`.

Test files export `test`, not `suite`. `musi test` invokes that exported entrypoint and collects test events through `musi:test`.

## A Taste of the Language

```musi
let Maybe[T] := data { Some : T | None };

let unwrap_or[T] (value : Maybe[T], fallback : T) : T :=
  case value of (
  | .Some(x) => x
  | .None => fallback
  );

let main : Int := (
  let xs := [1, 2, 3];
  let doubled := xs |> map((x) => x * 2);
  unwrap_or(.Some(41), 0) + 1
);
main();
```

The canonical grammar lives in `grammar.abnf`. Historical pre-reduction docs live under `docs/legacy/`.

## Project Structure

The clean-room crate structure lives under `crates_new/` and is the authoritative ownership map.

| Crate            | Role                                        |
| ---------------- | ------------------------------------------- |
| `music_basic`    | spans, sources, diagnostics, literals       |
| `music_storage`  | arenas and typed indices                    |
| `music_names`    | symbols and name-resolution graph           |
| `music_known`    | compiler-known surface names                |
| `music_lex`      | lexer                                       |
| `music_ast`      | full-fidelity green/red syntax tree         |
| `music_parse`    | parser (tokens to syntax tree)              |
| `music_resolve`  | import/env resolution + AST to HIR lowering |
| `music_hir`      | typed high-level IR model                   |
| `music_check`    | semantic analysis (types/effects/classes)   |
| `music_il`       | SEAM bytecode contract                      |
| `music_assembly` | SEAM text/binary transport + validation     |

## Editor Support

VS Code syntax support lives under `tools/vscode/`.

## Testing

```bash
cargo test -p music_parse      # test a specific crate
cargo clippy -p music_parse    # lint a specific crate
musi test                      # run Musi tests
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
