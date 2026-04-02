# Rewrite Notes

Follow `AGENTS.md` (source of truth).

Fast reminders:

- Edit `crates_new/` only; `crates/` is legacy reference-only.
- `grammar/Musi.g4` is canonical; `grammar/Musi.abnf` is strict RFC 5234 ABNF (aligned).
- No stubs: new crates/modules must be real and tested.
- Use longform naming (`syntax`, `sema`, `lexer.rs`, `parser.rs`).
- Do not introduce new macros.
