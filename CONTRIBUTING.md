# Contributing to Musi

This repo moves fast. Keep changes small, current, and well-tested.

## Read first

Before large changes, read:

- `AGENTS.md`
- `README.md`
- `docs/where/workspace-map.md`
- `docs/reference/public-api.md`
- `grammar/MusiParser.g4`
- `grammar/MusiLexer.g4`
- `grammar/Musi.abnf`

## Workflow

1. Fork the repo.
2. Clone your fork.
3. Create a focused branch.
4. Make one coherent change set.
5. Run targeted validation.
6. Commit with a message that explains why.
7. Open a pull request with motivation, approach, and test evidence.

## Ground rules

- Prefer surgical diffs over broad rewrites.
- Keep public docs aligned with current behavior.
- Do not add placeholders such as `todo!()` or `unimplemented!()` in `crates/`.
- Keep tests next to owned code. Musi tests live in `*.test.ms` files and export `test`.
- Grammar changes must keep parser structure intentional and update grammar tests or reference material when syntax meaning changes.

## Validation

Pick commands that match touched areas.

### Core repo

```bash
make lint
make check
```

### Rust crates

```bash
cargo test -p music_syntax
cargo test -p music_resolve
cargo test -p music_sema
cargo test -p music_ir
cargo test -p musi_project
cargo test -p musi_rt
```

Use targeted crate tests first. Avoid `cargo test --workspace` on low-memory machines.

## Pull request checklist

- [ ] Scope is focused.
- [ ] `make lint` passes.
- [ ] `make check` passes.
- [ ] Touched crates or surfaces have targeted tests/build checks.
- [ ] Reference docs or tests changed with behavior changes.
- [ ] Commit messages explain intent.
- [ ] PR description states what changed and how it was validated.

## Reporting bugs

Include:

- reproduction steps
- expected vs actual behavior
- relevant source snippet or diagnostic output
- commit hash
- platform details

## Using AI tools

AI assistance is allowed. You own the result.

- read surrounding code first
- verify generated changes
- run real tests
- do not merge code you do not understand

## Code of Conduct

All contributors must follow [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md).
