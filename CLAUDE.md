# Rewrite Notes

Follow `AGENTS.md` (source of truth).

Fast reminders:

- `grammar/Musi.g4` is canonical; `grammar/Musi.abnf` is strict RFC 5234 ABNF (aligned).
- No stubs: new crates/modules must be real and tested.
- Use longform naming (`syntax`, `sema`, `lexer.rs`, `parser.rs`).
- Do not introduce new macros.
