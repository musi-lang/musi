# Public API inventory

Current public entrypoints by crate. Keep this list high-signal. Do not mirror every symbol.

## Binaries

- `music` — direct check/build/run lane for `.ms` and `.seam`
- `musi` — package-aware check/build/run/test lane

## Compiler libraries

- `music_syntax` — lex/parse entrypoints and syntax tree exports
- `music_builtin` — builtin type and intrinsic catalog used by compiler/runtime layers
- `music_resolve` — resolved module model
- `music_sema` — `check_module`, semantic surface, diagnostics, effect rows
- `music_ir` — IR model and diagnostics
- `music_ir_lower` — sema-to-IR lowering
- `music_emit` — IR-to-SEAM lowering
- `music_session` — `Session`, `CompiledOutput`, session diagnostics/options, CTFE host configuration

## Runtime libraries

- `music_seam` — SEAM artifact encode/decode, SEAM HIL model/verifier, lowered `.seam` text, instruction/types surface
- `musi_vm` — `Program`, `Vm`, `VmOptions`, `MvmMode`, `MvmFeatures`, `VmOptimizationLevel`, `MvmOptionsParseError`, runtime `Value`, bound call handles, value inspection views, host/loader traits, VM errors
- `musi_native` — `NativeHost`, native test report types
- `musi_rt` — `Runtime`, `RuntimeOptions`, runtime errors
- `musi_foundation` — compiler-owned foundation/runtime module registration helpers

## Project and tooling libraries

- `musi_project` — project load, workspace/package compile entrypoints
- `musi_fmt` — `FormatOptions`, source formatter, path formatter, formatter errors
- `musi_tooling` — diagnostics collection, hover, artifact helpers
- `musi_lsp` — `MusiLanguageServer`

## Internal/support libraries

- `music_base`, `music_arena`, `music_hir`, `music_module`, `music_names`, `music_term`

These crates are public in Cargo workspace terms, but mostly support higher layers rather than act as stable end-user APIs.
