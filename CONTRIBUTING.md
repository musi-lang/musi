# Contributing to Musi

This guide covers repo workflow and project rules.

## Before Starting

- Read `grammar/Musi.g4` before grammar changes.
- Use `grammar/Musi.abnf` as the strict RFC 5234 ABNF reference.
- Read `AGENTS.md` and `docs/where/workspace-map.md` before large changes.
- Check open issues to avoid duplicating work.

## Standard Workflow

1. Fork the repository.
2. Clone your fork: `git clone https://github.com/<your-user>/musi.git`
3. Create a topic branch: `git checkout -b feat/<short-description>`
4. Make focused changes.
5. Build and test the relevant crates (see [Testing](#testing)).
6. Commit with a clear message explaining *why* the change exists.
7. Push and open a pull request describing the behavior change and how you tested it.

## Coding Guidelines

### Core Principles

- **KISS** -- prefer the simplest solution that works today.
- **YAGNI** -- do not build future features until they are needed.
- **No over-engineering** -- three similar lines of code is better than a premature abstraction.

### Naming

- Types and enum variants use `PascalCase` (`Expr`, `TokenKind`, `VmError`).
- Functions, methods, and local variables use `snake_case`.
- Prefix intentionally unused bindings with `_`.

### Rust Style

- Follow Rust edition 2024 conventions.
- Use `thiserror` for error types; avoid `anyhow`.
- Prefer `match` exhaustiveness over `_` fallbacks where practical.
- Keep functions short and focused; avoid nesting beyond three levels.
- Remove dead or commented-out code before submitting.

### Comments

Comments are noise. Only add them where the logic is genuinely non-obvious. Section dividers use the form `// -- Name ---`.

### Grammar Changes

Every grammar change must satisfy both project constraints:

1. **Strict LL(1)** -- compute FIRST sets explicitly and verify disjointness.
2. **Mathematical purity** -- syntax must reflect type-theoretic meaning.

## Testing

The workspace is large. Test crates individually:

```bash
# Build test binaries for the crates you changed
cargo build --tests -p music_syntax
cargo build --tests -p music_sema
cargo build --tests -p music_emit
cargo build --tests -p musi_vm

# Run the built binary (hash will differ)
./target/debug/deps/music_syntax-<hash>
```

Run `cargo clippy -p <crate>` on every crate you touch.

Avoid `cargo test --workspace` -- it may OOM on machines with less than 16 GB free RAM.

## Pull Request Checklist

- [ ] Relevant crates build cleanly with no warnings.
- [ ] `cargo clippy -p <crate>` passes for each changed crate.
- [ ] Tests pass for changed crates.
- [ ] Grammar changes update `grammar/Musi.g4` and relevant docs (`docs/what/language/syntax.md`, `grammar/Musi.abnf` if the spec reference changes).
- [ ] Commit messages explain intent (the *why*, not the *what*).
- [ ] PR description covers motivation, approach, and how it was tested.

## Reporting Issues

When filing a bug, include:

- Steps to reproduce.
- Expected versus actual behavior.
- Relevant source snippet or error output.
- Musi commit hash and platform (OS + architecture).

Feature requests should describe the use case and why it fits the language design.

## Development Setup

### Prerequisites

- Rust 1.88 or newer (edition 2024)
- Cargo (comes with Rust)
- Git

### Typical Commands

```bash
# Build the toolchain
cargo build

# Build release binaries
cargo build --release

# Run one direct source file
./target/release/music run path/to/main.ms

# Type-check one direct source file
./target/release/music check myfile.ms

# Check one package root
./target/release/musi check /path/to/package

# Run package tests
./target/release/musi test /path/to/package

# Lint
cargo clippy --workspace
```

## Using AI Assistants

You may use AI tools. You remain responsible for the result:

- Understand surrounding code before accepting AI suggestions.
- Review and test generated code carefully.
- Never merge output you do not fully understand.

## Questions and Support

Open an issue if you need clarification on direction, architecture, or roadmap priorities.

## Code of Conduct

All contributors must follow the [Code of Conduct](CODE_OF_CONDUCT.md).
