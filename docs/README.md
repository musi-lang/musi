# Docs

Musi docs use **WWHW**:

- **What** explains language and runtime concepts
- **Why** explains design rules and tradeoffs
- **How** explains operational flow and tooling use
- **Where** explains repo layout, crate ownership, and phase boundaries

Reference and status docs live separately.

## Start Here

If you need language surface:
- `docs/what/language/syntax.md`
- `docs/what/language/type-system.md`
- `docs/what/language/effect-system.md`

If you need runtime and SEAM:
- `docs/what/runtime/seam-vm.md`
- `docs/what/runtime/seam-bytecode.md`
- `docs/how/runtime/runtime-api.md`

If you need compiler and repo ownership:
- `docs/why/compiler-architecture.md`
- `docs/why/runtime-boundary.md`
- `docs/why/toolchain-split.md`
- `docs/where/workspace-map.md`
- `docs/where/stack-map.md`
- `docs/where/phase-boundaries.md`

If you need tool behavior:
- `docs/how/toolchain.md`

If you need inventories or status:
- `docs/reference/public-api.md`
- `docs/reference/grammar-ebnf.md`
- `docs/status/feature-matrix.md`
- `docs/status/stack-roadmap.md`
- `docs/status/frontend-stabilization-audit.md`

## Canon

`grammar/Musi.g4` is the canonical tool-supported grammar.

`grammar/Musi.abnf` is the strict RFC 5234 ABNF spec for the current surface syntax.

Implementation lives in `crates/`. First-party Musi packages live in `packages/`.
