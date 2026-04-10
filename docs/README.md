# Docs Index

`grammar/Musi.g4` is the canonical, tool-supported grammar for the current language surface.

`grammar/Musi.abnf` is a strict RFC 5234 ABNF specification of the Musi surface syntax (kept aligned with `grammar/Musi.g4`).

Canonical docs are ordered from language, to runtime, to tooling:

1. `00-syntax.md`
2. `01-type-system.md`
3. `02-effect-system.md`
4. `03-metaprogramming.md`
5. `04-compiler-attributes.md`
6. `05-ffi.md`
7. `06-seam-vm.md`
8. `07-seam-bytecode.md`
9. `08-runtime-api.md`
10. `09-architecture.md`
11. `10-toolchain.md`
12. `11-feature-matrix.md` (tracking)
13. `12-public-api.md` (tracking)
14. `14-antlr-grammar-tracker.md` (tracking)
15. `15-grammar-ebnf.md` (formal, compact)

The rewrite lives in `crates/` (canonical). The subsystem names are documented in `09-architecture.md`.
