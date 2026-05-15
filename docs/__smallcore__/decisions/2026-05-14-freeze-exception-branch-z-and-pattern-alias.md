# Freeze-Exception RFC — `br.z` Branch Mnemonic And Pattern Alias Canon

Status: `approved`
Date: `2026-05-14`
Policy source: `docs/__smallcore__/freeze-policy.md`
Process source: `docs/__smallcore__/freeze-exception-rfc.md`

## Change summary

This exception updates frozen canonical wording and mnemonic spelling to keep SEAM branch semantics Bit-native and assembly-shaped: conditional branch mnemonic is `br.z` (branch when top `Bit` is `0`) instead of `br.false`. The same exception also closes a canonical grammar/spec gap by explicitly freezing pattern alias/destructure forms (`as` and `or` patterns, plus record/tuple/array/variant destructuring details) so `as` is unambiguously pattern aliasing and never cast syntax.

## Canonical diff map

- `crates/music_seam/src/opcode/table/branch.rs`
  - mnemonic spelling: `br.false` -> `br.z`
  - opcode slot unchanged: `0x44`
- `specs/seam/bytecode.md`
  - branch mnemonic spelling and Bit/Word terminology updates
- `specs/seam/format.md`
  - disasm mnemonic reference updated to `br.z`
- `docs/__smallcore__/seam-00-index-and-principles.md`
- `docs/__smallcore__/seam-01-bytecode-and-stack-effects.md`
- `docs/__smallcore__/seam-04-external-artifacts-decomp-mar.md`
- `docs/reference/seam-il.md`
- `docs/where/bootstrap-bytecode-ledger.md`
- `docs/__smallcore__/reconciliation.md`
- `crates/music_session/src/tests.rs`
  - expected disasm mnemonic updated to `br.z`
- `grammar/MusiParser.g4`
- `grammar/Musi.abnf`
- `specs/language/syntax.md`
- `specs/language/module-boundaries.md`
- `docs/__smallcore__/musi-small-core-frozen-system.md`
- `crates/music_syntax/src/parser/tests.rs`
  - pattern alias test naming cleanup and explicit trailing-comma record-destructure parse coverage

## Compatibility impact

- Bytecode semantic behavior does not change: branch still consumes `Bit` and branches on `0`.
- Public disasm spelling changes from `br.false` to `br.z`.
- Canonical grammar/spec text now explicitly matches parser behavior for pattern aliasing/destructure forms.

## Risk and rollback

- Risk: downstream tools expecting `br.false` text need a coordinated update.
- Rollback: revert this RFC file and affected canonical files, restore prior hashes in `docs/__smallcore__/freeze-manifest.toml`, re-run freeze and compiler gates.

## Validation evidence

- `rtk cargo test -p musi --test freeze_contract`
- `rtk cargo test -p music_syntax --lib`
- `rtk cargo test -p music_seam --lib`
- `rtk cargo test -p music_session --lib`
- `rtk cargo test -p music_ir_lower --lib`
- `rtk cargo test -p musi --test cli`
- `rtk cargo test -p musi_vm --lib`

## Approval block

- Roadmap owner: `athena` — approved `2026-05-14`
- Syntax reviewer: `hermes` — approved `2026-05-14`
- Lowering reviewer: `hephaestus` — approved `2026-05-14`
- Runtime reviewer: `asclepius` — approved `2026-05-14`
- Artifacts reviewer: `artemis` — approved `2026-05-14`
- Tooling reviewer: `daedalus` — approved `2026-05-14`
