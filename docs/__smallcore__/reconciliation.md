# Small-Core Reconciliation (PLAN C/D/F)

Scope: `seam-00` through `seam-04` plus `musi-small-core-frozen-system.md`.

## PLAN C — Document Readiness Audit

| Document                                    | Result | Notes                                                                                                        | Open Items         |
| ------------------------------------------- | ------ | ------------------------------------------------------------------------------------------------------------ | ------------------ |
| `seam-00-index-and-principles.md`           | PASS   | Set-in-stone header added; roadmap link added.                                                               | None in this pass. |
| `seam-01-bytecode-and-stack-effects.md`     | PASS   | Set-in-stone header added; `@foreign` direction wording aligned to seam-04 shape rule.                       | None in this pass. |
| `seam-02-calls-objects-and-layouts.md`      | PASS   | Set-in-stone header added; no contradiction found against current call/layout language.                      | None in this pass. |
| `seam-03-runtime-gc-pinning-yield-defer.md` | PASS   | Set-in-stone header added; runtime/pin/defer/yield contract language remains internally consistent.          | None in this pass. |
| `seam-04-external-artifacts-decomp-mar.md`  | PASS   | Set-in-stone header added; `@foreign` text clarified to frozen semantics vs non-frozen payload keys.         | None in this pass. |
| `musi-small-core-frozen-system.md`          | PASS   | Set-in-stone header added; roadmap link added; `@foreign` wording aligned to frozen direction-by-shape rule. | None in this pass. |

## PLAN D — Cross-Doc Consistency Matrix Status

- PASS: matrix created in `docs/__smallcore__/consistency-matrix.md`.
- PASS: rows cover keyword/lowering/SEAM policy, `pin`/`defer`/`yield`, `.seam`/`.mar`, decomp naming, visibility rules, and `@foreign` direction.
- PASS: implementation-surface verification completed; evidence recorded in `docs/__smallcore__/checkpoint-log.md`.

## PLAN F — Required Correction Passes

- PASS: set-in-stone headers added across all seam docs and frozen-system doc.
- PASS: roadmap linkage added from `seam-00` and frozen-system doc.
- PASS: stale `@foreign` wording reconciled to direction-by-shape frozen rule.
- PASS: no new `.seamil` canonical wording introduced.
- PASS: implementation-level mismatch closure validated against PLAN E evidence and targeted tests.
- PASS: regression-test requirement satisfied by existing targeted suites because corrected mismatches in this closure were documentation contract alignment and naming consistency changes, with no new runtime behavior introduced.

## PLAN G — Freeze-Doc Canonicalization (Owner B)

- PASS: `specs/seam/bytecode.md` status is now `frozen 0.1.0 baseline (2026-05-14)`.
- PASS: `seam-00` through `seam-04` now use `frozen 0.1.0 baseline` set-in-stone wording.
- PASS: stale mnemonic spellings were reconciled to implemented canonical spellings.
- PASS: non-canonical drift examples (`ld mod`, `call ffi`, `brz`, `mdl.load`) were removed from normative examples.

### Canonical mnemonic renames applied

| Previous spelling | Canonical spelling |
| ----------------- | ------------------ |
| `ld.const`        | `ld.c`             |
| `ld.i4`           | `ld.c.i4`          |
| `brz`             | `br.z`             |
| `br.tab`          | `br.tbl`           |
| `ld.mod`          | `ld.mod.dyn`       |
| `ld.exp`          | `ld.exp.dyn`       |
| `cmp.lt.s`        | `cmp.lt`           |
| `cmp.le.s`        | `cmp.le`           |
| `cmp.gt.s`        | `cmp.gt`           |
| `cmp.ge.s`        | `cmp.ge`           |
