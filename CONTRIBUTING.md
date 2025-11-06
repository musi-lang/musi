# Contributing to Musi

Thanks for helping Musi grow. This guide keeps expectations clear and simple so you can move fast without guessing.

## Before You Start

- Read the Language Guide in `docs/` to understand current syntax and roadmap.
- Set up the toolchain listed in [Development Setup](#development-setup).
- Skim open issues to avoid duplicating work.
- Keep British spelling in docs and commit messages.

## Standard Workflow

1. Fork the repository.
2. Clone your fork: `git clone https://github.com/<your-user>/musi.git`.
3. Create a topic branch: `git checkout -b feat/<short-description>`.
4. Make focused changes.
5. Run `opam exec -- dune test` (and any extra commands relevant to your change).
6. Commit with a clear message that explains *why* the change exists.
7. Push and open a pull request describing behaviour changes and tests.

## Coding Guidelines

### Core Principles

- **KISS** — prefer the simplest solution that works today.
- **DRY** — extract shared behaviour quickly to keep one source of truth.
- **YAGNI** — do not build future features until the roadmap calls for them.

### Naming

- Modules and variant constructors use `PascalCase` (`Ast`, `Token`, `Some`).
- Functions and values use `snake_case`.
- Use `t` for primary types inside modules.
- Prefix intentionally unused bindings with `_`.

### OCaml Style

- Add type annotations when inference is unclear, especially in `.mli` files.
- Pattern match exhaustively instead of relying on `_` fallbacks.
- Keep data immutable unless a mutable field is required; mark it with `mutable`.
- Prefer small functions (under 50 lines) and shallow nesting (no more than four levels).
- Remove dead or commented-out code before submitting.

### Comments and Docs

- Comment to explain *why* a choice was made, not what the code already states.
- Update documentation and examples whenever behaviour changes.
- Follow the Language Guide structure when adding new docs; place new book chapters under `docs/language-guide/`.

## Testing Expectations

- Run `opam exec -- dune test` before every push.
- Add or update targeted tests for every bug fix or new feature.
- Mention any skipped or flaky tests in the pull request so reviewers know the risk.

## Using AI Assistants

No AI configuration files live in this repository yet, but you may use your own tools. You remain responsible for code quality:

- Understand the surrounding code before pasting AI suggestions.
- Review and test generated code carefully.
- Never merge output you do not fully understand.

## Pull Request Checklist

- [ ] Tests pass locally with `opam exec -- dune test`.
- [ ] Docs and comments updated if behaviour changed.
- [ ] Commit messages explain the intent.
- [ ] PR description covers motivation, approach, and testing.

## Reporting Issues

When filing a bug, include:

- Steps to reproduce.
- Expected versus actual behaviour.
- Output snippets or logs when helpful.
- Musi commit hash, OCaml version, and platform.

Feature requests should describe the use case and why Musi needs it now.

## Development Setup

### Prerequisites

- OCaml 5.3.0 or newer
- opam
- Dune 3.20.2 or newer
- Git

### Typical Commands

```bash
opam exec -- dune pkg lock   # lock dependencies
opam exec -- dune build      # compile everything
opam exec -- dune test       # run all tests
opam exec -- dune exec lib/parse/test_lexer.exe   # run a specific test
```

## Repository Layout

- `lib/basic/` -- spans, diagnostics, interner, and helpers
- `lib/parse/` -- lexer, parser, checker, resolver, metadata
- `lib/codegen/` -- bytecode instructions and emitter (work in progress)
- `lib/runtime/` -- VM, GC, and builtins
- `bin/` -- CLI entry point
- `docs/` -- language guide, reference, design notes
- `test/` -- integration and regression suites

## Questions and Support

Open an issue if you need clarification on direction, architecture, or roadmap priorities. Discussions stay public so future contributors benefit from the context.

## Code of Conduct

All contributors must follow the [Code of Conduct](CODE_OF_CONDUCT.md).
