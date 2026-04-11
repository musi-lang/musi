# Musi

A programming language with typed effects, a SEAM bytecode pipeline, and a runtime built on `musi:` and `@std`.

> [!WARNING]
> Musi is `v0.1.0-alpha.1`. The language, tooling, and standard library will have breaking changes. Do not use it for anything you can't afford to rewrite.

## What Musi Is

Musi source files use the `.ms` extension. The repo now ships two user-facing binaries:

| Binary  | What it does                                  |
| ------- | --------------------------------------------- |
| `music` | Direct `.ms` and `.seam` work                 |
| `musi`  | Package-aware manifest and workspace workflow |

- `musi:...` is the low-level capability namespace.
- `@std/<family>` is the standard library built on top of `musi:`.
- `@std` re-exports the standard-library families.
- `*.test.ms` files export `test`; `musi test` runs them through `musi:test`.

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

If you do not already have Git: <https://git-scm.com/downloads>

## Install from Source

There are no pre-built binaries yet.

```bash
git clone https://github.com/musi-lang/musi.git
cd musi
cargo build --release
```

Binaries land in `./target/release/`:

- `music`
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
musi check              # type-check the owning package
musi build              # compile the package entry to .seam bytecode
musi run                # run the package entry
musi test               # discover and run *.test.ms files
music check index.ms    # check one direct source graph
music build index.ms    # emit one direct .seam artifact
music run index.seam    # run compiled bytecode directly
```

## Imports and Packages

- `@std` is the first-party standard library root namespace under `packages/std`.
- `@std/<family>` is the canonical import shape for standard library code such as `@std/bytes`, `@std/math`, `@std/assert`, `@std/option`, `@std/result`, and `@std/testing`.
- `@std` re-exports those family modules directly from its root module.
- `musi:...` is the compiler-owned intrinsic namespace for low-level host/runtime capabilities.
- Prelude names such as builtin types and core classes are injected by the compiler. They are not loaded from `@std`.

Rules:

- Standard library code should prefer family imports such as `@std/bytes` and `@std/math`.
- Root imports through `@std` are supported.
- `@std` is the only first-party package family.
- Low-level runtime capabilities live in `musi:`.
- Test files export `test`, not `suite`.

Import style:

```musi
let Bytes := import "@std/bytes";
let Math := import "@std/math";
```

Root import is supported:

```musi
let Std := import "@std";
let Bytes := Std.Bytes;
let Math := Std.Math;
```

## Example

```musi
let Bytes := import "@std/bytes";
let Math := import "@std/math";

export let normalized_port () : Int := (
  let configured := Option.none[Int]();
  let fallback := 8080;
  Math.clamp(Option.unwrap_or[Int](configured, fallback), 1024, 65535)
);

export let payload () : Array[Int] :=
  Bytes.concat([1, 2, 3], [4, 5]);

export let main () : Int := normalized_port();
```

Test:

```musi
let Testing := import "@std/testing";
let Std := import "@std";

export let test () := (
  let (Math, Option) := (Std.Math, Std.Option);
  Testing.describe("app");
  Testing.it("normalizes default port", Testing.to_be(Math.clamp(Option.unwrap_or[Int](Option.none[Int](), 8080), 1024, 65535), 8080));
  Testing.end_describe()
);
```

## Runtime

- `musi_vm` executes validated SEAM programs.
- `musi_rt` is the source-aware runtime layer used by the repo tooling.
- `musi_native` is the first-party native host adapter behind low-level runtime capabilities.
- `music` is the direct source/artifact CLI.
- `musi` is the package-aware CLI.

## Project Structure

Rust crates live under `crates/`.

First-party Musi packages live under `packages/`.

Canonical reading order:

1. `docs/what/language/syntax.md`
2. `docs/what/runtime/seam-vm.md`
3. `docs/how/runtime/runtime-api.md`
4. `docs/where/workspace-map.md`
5. `docs/where/stack-map.md`

Grammar and language reference:

- `grammar/Musi.g4`
- `grammar/Musi.abnf`
- `docs/what/language/syntax.md`

## Editor Support

VS Code syntax support lives under `vscode-ext/`.

## Testing

```bash
cargo test -p music_syntax     # test a specific crate
cargo clippy -p music_syntax   # lint a specific crate
musi test                      # run Musi tests
```

Test layout:

- co-locate tests as `*.test.ms`
- export `test`
- import `@std/testing`
- emit test events through `musi:test`

Avoid `cargo test --workspace` on machines with less than 16 GB of free RAM.

See [CONTRIBUTING.md](CONTRIBUTING.md) for contributor workflow and project rules.

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
