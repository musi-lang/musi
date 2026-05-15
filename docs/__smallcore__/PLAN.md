# Musi Small-Core Long-Term Roadmap

Status legend:

- `[ ]` not started
- `[-]` in progress
- `[x]` complete
- `[!]` blocked / decision required

This plan tracks the full path from freeze-candidate documents to a set-in-stone small-core and SEAM contract freeze.

Roadmap status: `[x] complete` (sections A–J closed).

Bootstrap planning entrypoint: `docs/__smallcore__/bootstrap-planning.md`
Decision log: `docs/__smallcore__/decisions/2026-05-14-roadmap-kickoff.md`
Checkpoint cadence/log: `biweekly` in `docs/__smallcore__/checkpoint-log.md`
Risk register: `docs/__smallcore__/freeze-risk-register.md`
Changelog: `docs/__smallcore__/CHANGELOG.md`

Role assignments:

- Roadmap owner: `athena`
- Syntax reviewer: `hermes`
- Lowering reviewer: `hephaestus`
- Runtime reviewer: `asclepius`
- Artifacts reviewer: `artemis`
- Tooling reviewer: `daedalus`

---

## A. Governance and Tracking

- [x] Assign single owner for this roadmap cycle.
- [x] Assign per-domain reviewers: syntax, lowering, runtime, artifacts, tooling.
- [x] Define review cadence (weekly/biweekly) and checkpoint notes location.
- [x] Add dated decision log file under `docs/__smallcore__/decisions/`.
- [x] Add freeze-risk register file under `docs/__smallcore__/`.
- [x] Link this roadmap from `docs/__smallcore__/seam-00-index-and-principles.md`.
- [x] Link this roadmap from `docs/__smallcore__/musi-small-core-frozen-system.md`.

## B. Source-of-Truth Freeze Inputs

- [x] Reconfirm syntax canon alignment with `grammar/MusiParser.g4`.
- [x] Reconfirm syntax canon alignment with `grammar/MusiLexer.g4`.
- [x] Reconfirm syntax canon alignment with `grammar/Musi.abnf`.
- [x] Reconfirm crate ownership boundaries against `docs/where/workspace-map.md`.
- [x] Reconfirm public API scope against `docs/reference/public-api.md`.
- [x] Record all mismatches with file-level TODO ownership and due date.

## C. Document Readiness Audit (Reviewing Checkpoints)

- [x] Run full seam-doc review pass (`seam-00` to `seam-04`).
- [x] Reconfirm seam-00 freeze rules remain current and actionable.
- [x] Reconfirm seam-01 stack-effect/verifier rules remain internally consistent.
- [x] Reconfirm seam-02 call/frame/layout model remains internally consistent.
- [x] Reconfirm seam-03 runtime/GC/pin/defer/yield model remains internally consistent.
- [x] Reconfirm seam-04 artifact/interop/decomp/source-map model remains internally consistent.
- [x] Reconfirm `musi-small-core-frozen-system.md` sections remain normative and non-contradictory.
- [x] Verify no stale wording that implies removed syntax/features.
- [x] Verify each freeze checklist item maps to at least one implementation surface.
- [x] Produce one reconciliation note per doc with pass/fail and open items.

## D. Cross-Doc Consistency Matrix

- [x] Build matrix: source keyword rules vs lowering rules vs SEAM rules.
- [x] Build matrix: `pin`/`defer`/`yield` semantics across source, lowering, runtime.
- [x] Build matrix: `.seam`/`.mar`/map policy across seam-00 and seam-04.
- [x] Build matrix: decompilation naming policy across seam-02, seam-04, frozen-system.
- [x] Build matrix: visibility rules (`export`, `hidden`, `erased`) across docs.
- [x] Resolve all matrix conflicts with explicit accepted wording patches.

## E. Implementation Surface Mapping

- [x] Confirm artifact model coverage in `crates/music_seam/src/artifact.rs`.
- [x] Confirm descriptor model coverage in `crates/music_seam/src/descriptor/`.
- [x] Confirm `.mar` model coverage in `crates/music_seam/src/mar.rs`.
- [x] Confirm binary encode/decode alignment in `crates/music_seam/src/binary/`.
- [x] Confirm text format/parse alignment in `crates/music_seam/src/text/`.
- [x] Confirm CLI surface alignment in `crates/musi/src/commands/disasm.rs`.
- [x] Confirm CLI surface alignment in `crates/musi/src/commands/build.rs`.
- [x] Confirm CLI command wiring alignment in `crates/musi/src/commands/mod.rs`.
- [x] Confirm seam validation coverage in `crates/music_seam/src/tests.rs`.
- [x] Confirm seam assembly coverage in `crates/music_seam/src/assembly_tests.rs`.
- [x] Confirm CLI behavior coverage in `crates/musi/tests/cli.rs`.

## F. Required Correction Passes

- [x] Patch all doc/spec/impl mismatches found in checkpoints C–E.
- [x] Patch naming mismatches (`disasm`, `decomp`, `.seam`, `.mar`, map terms).
- [x] Patch any stale mention of `.seamil` as canonical artifact.
- [x] Patch any stale source-syntax wording that contradicts frozen system.
- [x] Patch any missing verifier or stack-effect contract text.
- [x] Patch any missing interop/domain descriptor requirements.

## G. Validation Gates

- [x] Run targeted seam library tests (`music_seam`).
- [x] Run targeted lowering tests (`music_ir_lower`).
- [x] Run targeted CLI tests (`musi` CLI surfaces).
- [x] Add regression tests for each corrected mismatch.
- [x] Verify no unchecked freeze checklist items in seam-01.
- [x] Verify no unchecked freeze checklist items in seam-02.
- [x] Verify no unchecked freeze checklist items in seam-03.
- [x] Verify no unchecked freeze checklist items in seam-04.
- [x] Record exact commands and outputs in checkpoint log.

## H. Set-in-Stone Decision Gate

- [x] Confirm all sections A–G are complete.
- [x] Confirm no open `[!]` blockers remain.
- [x] Confirm no unresolved cross-doc contradictions remain.
- [x] Confirm no unresolved doc-to-implementation mismatch remains.
- [x] Capture final freeze decision statement in `docs/__smallcore__/SET-IN-STONE.md`.
- [x] Record freeze decision date and owners.
- [x] Record explicit scope of what is frozen (language + SEAM contracts + artifact policy).

## I. Post-Freeze Lockdown

- [x] Add guardrail note: frozen small-core changes require explicit freeze-exception RFC.
- [x] Add changelog entry documenting frozen compiler state.
- [x] Mark all seam docs with set-in-stone header status.
- [x] Mark `musi-small-core-frozen-system.md` with set-in-stone header status.
- [x] Snapshot final consistency matrix and decision log for archive.

## J. Next User-Invoked Manual Phase Gate

- [x] Open new planning track titled `bootstrap planning`.
- [x] Define bootstrap scope boundaries from frozen contracts only.
- [x] Define bootstrap milestones and validation gates.
- [x] Link bootstrap plan entrypoint from `docs/__smallcore__/PLAN.md`.
- [x] Close this roadmap as complete once bootstrap plan is accepted.

---

## Completion Definition (for this roadmap)

This roadmap is complete when:

- [x] all sections A through J are marked `[x]`;
- [x] `docs/__smallcore__/SET-IN-STONE.md` exists with final freeze decision;
- [x] bootstrap planning track is created and linked;
- [x] this file status is updated to `complete`.
