# Musi

A programming language with a type system, functional features, and a stack-based bytecode VM.

> [!WARNING]
> Musi is v0.1.0-rc1. The language, tooling, and standard library will have breaking changes. Do not use it for anything you can't afford to rewrite.

## What Musi Is

Musi source files use the `.ms` extension. The compiler (`music`) type-checks and compiles them to `.msbc` bytecode. The interpreter (`musi`) executes the bytecode.

There are two binaries:

| Binary | What it does |
|--------|--------------|
| `music` | Compiler — type-check, build, run, format, lint, test |
| `musi` | Standalone bytecode interpreter — runs `.msbc` files |

You will mostly use `music`. The `musi` binary is for running pre-compiled bytecode without the compiler.

## Prerequisites

You need three things installed before you start.

### 1. Rust 1.88 or newer

If you don't have Rust installed:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

If you already have Rust, make sure it's up to date:

```bash
rustup update stable
```

Check your version:

```bash
rustc --version   # must show 1.89.0 or higher
```

### 2. libffi

Musi uses libffi for its foreign function interface. You need the system library installed.

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

There are no pre-built binaries yet. You build from source.

**Step 1.** Clone the repository:

```bash
git clone https://github.com/musi-lang/musi.git
cd musi
```

**Step 2.** Build in release mode:

```bash
cargo build --release
```

This takes a few minutes the first time. When it finishes, the binaries are at:

- `./target/release/music` (compiler)
- `./target/release/musi` (bytecode interpreter)
- `./target/release/music_lsp` (LSP server)

**Step 3.** (Optional) Add to your PATH so you can run `music` from anywhere:

```bash
export PATH="/path/to/musi/target/release:$PATH"
```

Add that line to your `~/.bashrc`, `~/.zshrc`, or equivalent.

Replace `/path/to/musi` with the actual path where you cloned the repo.

## Quick Start

These steps assume `music` is in your PATH. If not, replace `music` with `./target/release/music`.

**Step 1.** Create a new project:

```bash
music new hello
cd hello
```

This creates three files:

```text
hello/
  mspackage.toml   project manifest (name, version, entry point)
  index.ms          source file
  .gitignore
```

**Step 2.** Look at the generated `index.ms`:

```musi
import "@std/rt" as rt;

rt.writeln("hello, world!");
```

**Step 3.** Run it:

```bash
music run
```

This compiles `index.ms` and immediately executes it. You should see `hello, world!` printed.

**Step 4.** Other things you can do:

```bash
music check              # type-check without running
music build              # compile to bytecode (produces index.msbc)
musi index.msbc          # run compiled bytecode directly
music fmt                # format source files
music lint               # lint source files
```

## CLI Reference

| Command | What it does |
|---------|-------------|
| `music run [file]` | Compile and execute. Defaults to `package.main` from `mspackage.toml`. |
| `music check [file]` | Lex, parse, and type-check without running. |
| `music build [file] [-o output]` | Compile `.ms` to `.msbc` bytecode. |
| `music test [filter]` | Discover and run `*.test.ms` files. |
| `music fmt [files...] [--check]` | Format source files. `--check` reports without modifying. |
| `music lint [files...]` | Lint source files. |
| `music new <name> [--template lib]` | Create a new project directory. Default template is `bin`. |
| `music init [--template lib]` | Initialize a project in the current directory. |
| `music add <specifier> [--name x] [--dev]` | Add a dependency to `mspackage.toml`. |
| `music task [name] [--list]` | Run a task defined in `mspackage.toml`. |

## Language Syntax at a Glance

These are real examples from the codebase. This is not a complete reference.

### Bindings and Functions

```musi
let x := 42;
let name : String := "musi";

let add := (a, b) => a + b;

export let greet : (String) -> String := (name) => f"hello, {name}!";
```

### Records and Choice Types

```musi
record Stack ['a] {
    items : [&'a],
    len : Int,
};

choice Option ['a] {
    Some of 'a,
    None,
};

choice Result ['a, 'e] {
    Ok of 'a,
    Err of 'e,
};
```

### Pattern Matching

```musi
match result (
    .Ok(value) => value
  | .Err(e) => default
);
```

### Imports and Modules

```musi
import "@std/rt" as rt;
import "@std/math" as math;
```

### Type Classes

```musi
class Eq ['T] {
    let (=)(a, b) : Bool;
};

instance Eq of Int {
    let (=)(a, b) : Bool := a;
};
```

### Effects

```musi
effect State ['S] {
    get : () -> 'S;
    put : 'S -> ();
};
```

## Editor Support

### LSP Server

The `music_lsp` binary is built alongside the compiler. It supports:

- Code completion
- Hover information
- Go-to definition and type definition
- Find references
- Inlay hints
- Semantic highlighting
- Code actions and code lens
- Document symbols and folding ranges
- Signature help

Point your editor's LSP client at the `music_lsp` binary.

### VS Code

A VS Code extension is in development at `tools/vscode/`. It is not published to the marketplace yet.

## Project Structure

The compiler is a Cargo workspace with these crates:

| Crate | Role |
|-------|------|
| `music` | Compiler CLI (all subcommands) |
| `musi` | Standalone bytecode interpreter CLI |
| `music_lsp` | LSP server |
| `music_shared` | Spans, source database, string interner, diagnostics |
| `music_lex` | Lexer / tokenizer |
| `music_ast` | AST node types |
| `music_parse` | Parser |
| `music_sema` | Semantic analysis / type-checker |
| `music_resolve` | Module resolution |
| `music_emit` | Bytecode emitter (AST + sema to `.msbc`) |
| `musi_bc` | Bytecode format definitions |
| `musi_vm` | Bytecode interpreter / VM |
| `musi_builtins` | Standard library runtime + FFI |
| `musi_manifest` | `mspackage.toml` parser |

## Standard Library

The stdlib lives in `stdlib/` and includes: `array`, `assert`, `cmp`, `collections`, `convert`, `csv`, `encoding`, `fmt`, `func`, `hash`, `ini`, `io`, `iter`, `json`, `log`, `math`, `ops`, `option`, `path`, `random`, `result`, `rune`, `semver`, `sort`, `text`, `testing`, `uuid`.

Collections include: stack, queue, deque, heap, ring buffer, bitset, sorted array.

## Testing

The full workspace can use a lot of memory. Test individual crates:

```bash
cargo test -p music_parse       # test a specific crate
cargo clippy -p music_parse     # lint a specific crate
```

Avoid `cargo test --workspace` on machines with less than 16 GB of free RAM.

Run the Musi standard library tests:

```bash
cargo run --bin music -- test
```

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

MIT OR Apache-2.0 — see [LICENSE](LICENSE) for details.
