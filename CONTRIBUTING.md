# Contributing to Musi

Thanks for helping Musi grow. This guide keeps expectations clear and simple so you can move fast without guessing.

## Before Starting

- Read language guide in `docs/` to understand current syntax and roadmap.
- Set up toolchain listed in [Development Setup](#development-setup).
- Check open issues to avoid duplicating work.
- Keep British spelling in docs and commit messages.

## Standard Workflow

1. Fork repository.
2. Clone fork: `git clone https://github.com/<your-user>/musi.git`.
3. Create topic branch: `git checkout -b feat/<short-description>`.
4. Make focused changes.
5. Run `opam exec -- dune test` (and any extra commands relevant to change).
6. Commit with clear message explaining *why* change exists.
7. Push and open pull request describing behaviour changes and tests.

## Coding Guidelines

### Core Principles

- **KISS** -- prefer simplest solution that works today.
- **DRY** -- extract shared behaviour quickly to keep one source of truth.
- **YAGNI** -- do not build future features until roadmap calls for them.

### Naming

- Modules and variant constructors use `PascalCase` (`Expr`, `Token`, `Some`).
- Functions and values use `snake_case`.
- Use `t` for primary types inside modules.
- Use `_opt` suffix for functions that return `option`.
- Use `try_` prefix for functions that return `result`.
- Prefix intentionally unused bindings with `_`.

### OCaml Style

- Add type annotations when inference is unclear, especially in `.mli` files.
- Pattern match exhaustively instead of relying on `_` fallbacks.
- Keep data immutable unless mutable field is required; mark with `mutable`.
- Prefer small functions (under 50 lines) and shallow nesting (no more than four levels).
- Remove dead or commented-out code before submitting.

### Comments and Docs

- Comment to explain *why* choice was made, not what code already states.
- Update documentation and examples whenever behaviour changes.
- Follow language guide structure when adding new docs; place new book chapters under `docs/language-guide/`.

## Testing Expectations

- Run `opam exec -- dune test` before every push.
- Add or update targeted tests for every bug fix or new feature.
- Mention any skipped or flaky tests in pull request so reviewers know risk.

## Using AI Assistants

No AI configuration files live in repository yet, but you may use own tools. You remain responsible for code quality:

- Understand surrounding code before pasting AI suggestions.
- Review and test generated code carefully.
- Never merge output you do not fully understand.

## Pull Request Checklist

- [ ] Tests pass locally with `opam exec -- dune test`.
- [ ] Docs and comments updated if behaviour changed.
- [ ] Commit messages explain intent.
- [ ] PR description covers motivation, approach, and testing.

## Reporting Issues

When filing bug, include:

- Steps to reproduce.
- Expected versus actual behaviour.
- Output snippets or logs when helpful.
- Musi commit hash, OCaml version, and platform.

Feature requests should describe use case and why Musi needs it now.

## Development Setup

### Prerequisites

- OCaml 5.4.0 or newer
- opam
- Dune 3.20.2 or newer
- Git

### Typical Commands

```bash
# lock dependencies
opam exec -- dune pkg lock

# compile everything
opam exec -- dune build

# run all tests
opam exec -- dune test
```

## Questions and Support

Open issue if you need clarification on direction, architecture, or roadmap priorities. Discussions stay public so future contributors benefit from context.

## Code of Conduct

All contributors must follow [Code of Conduct](CODE_OF_CONDUCT.md).
