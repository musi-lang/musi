# Frontend Stabilization Audit (`crates_new/`)

This document records the current stabilization audit for the pre-runtime frontend and service boundary.

Scope for this pass:

- `music_base`
- `music_names`
- `music_arena`
- `music_syntax`
- `music_module`
- `music_hir`
- `music_resolve`
- `music_sema`
- `music_ir`
- `music_emit`
- `music_session`

Out of scope for this pass:

- `music_bc`
- `music_assembly`
- `musi_project`
- runtime / VM
- native / JIT backend

Validation baseline at audit time:

- `cargo test -q` passes
- `cargo clippy -q --workspace --all-targets` passes
- `scripts/audit_god_crates.sh crates_new` passes
- `scripts/audit_paths.sh crates_new` passes

## Current Phase Boundary

Canonical phase DAG:

`music_base -> music_names -> music_syntax -> music_module -> music_resolve -> music_sema -> music_ir -> music_emit -> music_session`

Primary entrypoints:

- parse: `crates_new/music_session/src/session/cache.rs:26`
- resolve: `crates_new/music_session/src/session/cache.rs:66`
- sema: `crates_new/music_session/src/session/cache.rs:105`
- ir: `crates_new/music_session/src/session/cache.rs:143`
- emit: `crates_new/music_session/src/session/cache.rs:184`

Primary public stability boundary:

- crate-root `pub use ...` sets described in `docs/12-public-api.md`

## Freeze Decision

| Crate | Status | Reason |
| --- | --- | --- |
| `music_base` | Freeze now | Small public surface, clear ownership, diagnostics surface already accessor-based |
| `music_names` | Freeze now | Small, focused interning and name-resolution support surface |
| `music_arena` | Freeze now | Small, generic storage API with stable responsibility |
| `music_hir` | Freeze now | Internal semantic model crate with narrow root export role and no active instability signals in this audit |
| `music_syntax` | Freeze now | Typed syntax errors already convert into canonical diagnostics and session coverage now exercises syntax-failure propagation |
| `music_module` | Freeze now | `ImportError` now has stable typed identity plus explicit message access; display no longer depends on `Debug` formatting |
| `music_resolve` | Freeze now | Public surface is small and typed negative-path propagation is now covered through session integration tests |
| `music_emit` | Freeze now | `EmitDiagKind` extraction is now complete and code-based rather than message-based |
| `music_session` | Freeze now | Parse failures use one canonical `SessionSyntaxErrors` shape and session tests now cover typed parse/resolve/sema/IR/emit propagation |
| `music_sema` | Freeze now | Public reads are query-oriented, construction-only builders are internal, and downstream crates no longer rely on public storage layout |
| `music_ir` | Freeze now | Full executable IR ADT is now the explicit pre-runtime backend contract and lowering invariants return typed diagnostics instead of aborting |

## Ranked Blockers

| # | Severity | File | Issue | Required fix |
| --- | --- | --- | --- | --- |
| 1 | Medium | `crates_new/music_ir/src/lower/mod.rs:123` | `music_ir` lowering still lives in one very large coordinator module | Continue decomposition so lowering mechanics do not calcify into one file even though the public IR contract is now frozen |
| 2 | Medium | `crates_new/music_sema/src/checker/exprs.rs:1` | `music_sema` still carries a large checker hotspot even after API narrowing | Continue splitting semantic responsibilities so freeze does not imply one-file implementation ownership |
| 3 | Low | `crates_new/music_session/src/session/cache.rs:1` | `music_session` stage orchestration remains concentrated in one cache module | Keep splitting cache/orchestration responsibilities if new behavior lands there |

## Coverage Gaps

Boundary-focused gaps worth improving after freeze:

- quote/splice still have stronger positive backend coverage than explicit resolve/sema boundary-only assertions
- operator-family end-to-end coverage is thinner than core literal/call/control coverage in `music_ir`, `music_emit`, and `music_session`
- dynamic import has end-to-end opcode coverage, but `music_ir` tests still do not assert lowered `DynamicImport` shape in a dedicated direct IR body test

## Size / Responsibility Hotspots

Largest audited non-test hotspots:

- `music_sema`: `9177` LOC
- `music_ir`: `5243` LOC
- `music_syntax`: `4643` LOC
- `music_emit`: `3186` LOC
- `music_resolve`: `2146` LOC

Largest single-file hotspots from the current audit:

- `crates_new/music_ir/src/lower/mod.rs` — `2625` LOC
- `crates_new/music_syntax/src/parser/forms.rs` — `1158` LOC
- `crates_new/music_sema/src/checker/exprs.rs` — `985` LOC
- `crates_new/music_emit/src/emit/expr/control.rs` — `636` LOC
- `crates_new/music_session/src/session/cache.rs` — `304` LOC

These are not automatic freeze blockers by themselves, but they are the first places to inspect when behavior or API churn appears.

## Stabilization Order

Recommended order for the next cleanup wave:

1. keep splitting `music_ir/src/lower/mod.rs`
2. keep splitting `music_sema/src/checker/exprs.rs`
3. strengthen direct IR regression tests for dynamic import and remaining operator families
4. keep `music_session` integration coverage aligned with every newly frozen stage contract

## Audit Conclusion

Current state is **ready to freeze the full pre-runtime frontend/service public API**.

Most important conclusion:

- foundation/support crates are stable
- `music_sema`, `music_ir`, and `music_session` have now crossed the freeze gate
- remaining work is implementation decomposition and coverage deepening, not public-boundary uncertainty
