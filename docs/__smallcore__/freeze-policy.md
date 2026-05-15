# Small-Core Freeze Fingerprint Policy

Status: `active`
Scope: frozen syntax and bytecode canonical surfaces
Gate test: `rtk cargo test -p musi --test freeze_contract`
Manifest: `docs/__smallcore__/freeze-manifest.toml`
Checkpoint log: `docs/__smallcore__/checkpoint-log.md`
Freeze-exception RFC: `docs/__smallcore__/freeze-exception-rfc.md`

## Canonical Surfaces

The freeze fingerprint gate tracks these canonical files:

- `grammar/MusiParser.g4`
- `grammar/MusiLexer.g4`
- `grammar/Musi.abnf`
- `specs/language/first-class-everything.md`
- `specs/language/items-and-attributes.md`
- `specs/language/syntax.md`
- `specs/language/type-core.md`
- `specs/language/contextual-capabilities.md`
- `specs/language/module-boundaries.md`
- `specs/language/yield-and-capabilities.md`
- `specs/seam/bytecode.md`
- `specs/seam/lowering.md`
- `specs/seam/domains.md`
- `specs/seam/format.md`
- `docs/__smallcore__/musi-small-core-frozen-system.md`
- `docs/__smallcore__/seam-00-index-and-principles.md`
- `docs/__smallcore__/seam-01-bytecode-and-stack-effects.md`
- `docs/__smallcore__/seam-02-calls-objects-and-layouts.md`
- `docs/__smallcore__/seam-03-runtime-gc-pinning-yield-defer.md`
- `docs/__smallcore__/seam-04-external-artifacts-decomp-mar.md`

Each file is pinned by SHA-256 in the manifest.

## Remediation Workflow

Use this workflow whenever `freeze_contract` fails.

1. Confirm whether the canonical-file change is intentional and approved.
2. If the change alters frozen scope, complete `docs/__smallcore__/freeze-exception-rfc.md` and secure required approvals before merge.
3. Recompute SHA-256 values from the repo root:
   - `rtk proxy -- shasum -a 256 <canonical-file-list>`
4. Update matching entries in `docs/__smallcore__/freeze-manifest.toml`.
5. Append a checkpoint entry in `docs/__smallcore__/checkpoint-log.md` with:
   - intent summary,
   - executed command(s),
   - evidence log path,
   - freeze-exception RFC reference (or `not required`).
6. Re-run the gate:
   - `rtk cargo test -p musi --test freeze_contract`

## Freeze-Exception RFC Process

Use `docs/__smallcore__/freeze-exception-rfc.md` whenever a change touches frozen scope semantics or canonical file ownership. Keep the RFC linked from PR notes and checkpoint entries until decision status is `approved` or `rejected`.

## Evidence Path Convention

Record command output in a local evidence file and reference that path in the checkpoint entry.

Suggested path template:

- `/private/tmp/musi-freeze-gate-YYYYMMDD.log`
