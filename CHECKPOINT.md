# Musi Compiler -- Session Checkpoint (2026-03-07)

Read `CLAUDE.md` for grammar design rules (LL(1), mathematical purity).
Read `MEMORY.md` at `~/.claude/projects/-Users-krystian-CodeProjects-musi/memory/MEMORY.md` for full technical memory (workflow constraints, naming conventions, phase history, crate decisions).

---

## Original Goal

Implement three waves of features for the Musi language compiler:
- **Wave 1**: FFI via `extrin` keyword, error message rewrite, musi_sema audit
- **Wave 2**: Array values + opcodes, array intrinsics, `musi check` CLI subcommand, sema false-positive fixes
- **Wave 3**: Finalize `extrin` rename everywhere, wire FFI end-to-end, `#[link]` attribute extraction, array stdlib module, `Iterable` type class stub

Wave 4 is planned but NOT started.

---

## Completed Work

### Wave 1: FFI skeleton + error messages

- **`extrin` keyword** introduced (renamed from `native` -> `foreign` -> `extrin`, meaning "extrinsic" -- not part of the essential nature of the program)
- `crates/musi_vm/src/ffi.rs` -- NEW file. `FfiState` struct caches `libloading::Library` handles and resolved `FfiSymbol` pointers. `FfiState::call()` dispatches based on param count and value types. Supports `(f64)->f64`, `(f64,f64)->f64`, `(i64)->i64`, `(i64,i64)->i64`, `()->f64`, `()->i64`.
- `libloading = "0.9"` added to `crates/musi_vm/Cargo.toml`
- Platform-aware library loading: macOS loads `libSystem.B.dylib` for default namespace; Linux tries `libm.so.6` / `libm.so`
- `library_candidates()` generates platform-specific paths (`lib{name}.dylib`, `lib{name}.so`, etc.)
- Error messages across all 4 crates rewritten to Clang/Elm style (short, plain English, backtick-wrapped tokens, always says what was found)
- musi_sema audited: ~80% complete; grammar/AST/Pat for arrays already existed

### Wave 2: Arrays + `musi check`

- `Value::Array(Rc<RefCell<Vec<Value>>>)` added to `crates/musi_vm/src/value.rs` -- in-place mutation with `var`
- 6 array opcodes in `crates/musi_codegen/src/opcode.rs`:
  - `NewArr` (0x90), `ArrGet` (0x91), `ArrSet` (0x92), `ArrLen` (0x93), `ArrPush` (0x94), `ArrSlice` (0x95)
- Codegen in `crates/musi_codegen/src/emitter.rs` for:
  - Array literals `[1, 2, 3]` -> `NewArr`
  - Indexed access `arr.[i]` -> `ArrGet`
  - Indexed assignment `arr.[i] <- v` -> `ArrSet`
  - For-in loops `for x in arr loop (...)` -> uses `ArrLen` + `ArrGet` in a counted loop
- 6 array intrinsics (IDs 15-20) in `crates/musi_codegen/src/intrinsics.rs`:
  - `array_length` (15), `array_push` (16), `array_pop` (17), `array_get` (18), `array_set` (19), `array_slice` (20)
- Array intrinsic declarations added to `std/prelude.ms` (lines 141-146)
- VM dispatch for all array opcodes and intrinsics in `crates/musi_vm/src/vm.rs` and `crates/musi_vm/src/native.rs`
- `musi check <file.ms>` CLI subcommand added to `crates/musi/src/main.rs` -- runs parser + `musi_sema::analyze`, exits 1 on errors
- `musi_sema` dependency added to `crates/musi/Cargo.toml`
- musi_sema false-positive fixes in `crates/musi_sema/src/check.rs`:
  - `?T` (option sugar) now handled
  - `DotPrefix` patterns recognized
  - `ClassDef` / `GivenDef` no longer trigger errors
  - Anonymous records handled
  - 18 sema tests passing in `crates/musi_sema/src/tests.rs`

### Wave 3: `extrin` rename + FFI wiring + Array stdlib

- Token rename: `Foreign` -> `Extrin` in `crates/musi_lex/src/token.rs` (keyword `"extrin"`)
- Lexer test updated in `crates/musi_lex/src/lexer/tests.rs`
- AST: `Modifier::Foreign` -> `Modifier::Extrin` in `crates/musi_ast/src/lib.rs`
- Parser: `extrin` keyword handling in `crates/musi_parse/src/parser.rs`
- S-expression printer: `extrin` output in `crates/musi_parse/src/sexpr.rs`
- All `.ms` files updated: `std/prelude.ms`, `std/io/Io.ms`, `std/num/Float.ms`, `std/num/Nat.ms`, `std/string/String.ms`
- VS Code TextMate grammar: `extrin` keyword in `tools/vscode/syntaxes/musi.tmLanguage.json`
- `grammar.ebnf` updated with `extrin` keyword
- `ffi.rs` registered as `pub mod ffi` in `crates/musi_vm/src/lib.rs`
- `FfiState` field added to `Vm` struct; initialized in `Vm::new()`
- FFI dispatch wired in `exec_call`: when `intrinsic_id == 0xFFFF` (u16::MAX), calls `self.ffi.call(fn_idx, &args, &self.module)`
- `SymbolEntry` gained two new fields in `crates/musi_codegen/src/module.rs`:
  - `link_lib: Option<Box<str>>` -- from `#[link("libm")]`
  - `link_name: Option<Box<str>>` -- from `#[link(name := "sym")]`
  - Tag-encoded serialization: 0x00 = None, 0x01 = Some(len + bytes)
- `#[link]` attribute extraction in emitter: parses `#[link("libm")]` and `#[link(name := "sym")]` forms
- `workspace.unsafe_code` lint changed from `forbid` to `deny` in `Cargo.toml` (FFI requires unsafe)
- `std/core/Array.ms` -- NEW file with 7 pure-Musi array utilities:
  - `array_map`, `array_filter`, `array_fold`, `array_each`, `array_contains`, `array_reverse`, `array_concat`
- `Iterable['C, 'E]` type class stub added to `std/prelude.ms` (lines 148-151):
  - `fn iter_len(c: 'C): Int;`
  - `fn iter_get(c: 'C, i: Int): 'E;`

---

## Current State

- **Branch**: `worktree-agent-af06cf32` (all wave 1-3 changes are UNCOMMITTED in working tree)
- **31 files modified**, 2 new files (`crates/musi_vm/src/ffi.rs`, `std/core/Array.ms`)
- **Net diff**: +651 / -139 lines
- **All tests green**: musi_shared=34, musi_lex=57, musi_parse=51, musi_codegen=16, musi_vm=36, musi_sema=18
- **No partially-modified files** -- all changes are complete and consistent
- **No running processes**
- **FFI verified working**: `extrin fn sqrt(x: Float): Float;` with `#[link("m")]` calls libc sqrt, returns correct result
- **Arrays verified working**: `[1,2,3]`, `arr.[i]`, `arr.[i] <- v`, `for n in arr loop (...)`
- **`musi check` verified working**: catches type mismatches, exits 1 on errors

### Minor issue in module.rs

Line 126 has a missing `/` in the doc comment:
```
/ Symbol name override for extrin fns (from `#[link(name := "sym")]`). `None` = use `name`.
```
Should be `///` (triple slash). Cosmetic only, does not affect compilation.

---

## Remaining Work (Wave 4 Plan)

### Track A: `Iterable` type class wiring (HIGH PRIORITY)

1. Create `given Iterable[[]'T, 'T]` instance -- make arrays implement `Iterable` via `iter_len` = `array_length`, `iter_get` = `array_get`. Place in `std/prelude.ms` or `std/core/Array.ms`.
2. Generalize `for x in c loop (...)` codegen in `emitter.rs`: currently special-cases arrays with `ArrLen` / `ArrGet` opcodes. Should instead dispatch through `Iterable` class methods (`iter_len` / `iter_get`) so that any type with a `given Iterable` works in for-in loops.
3. Test with custom iterable types (e.g., a `Range` record that implements `Iterable`).

### Track B: Sema false-positive fixes (MEDIUM PRIORITY)

1. Record field access: `check.rs` currently returns a fresh type variable instead of the structural field type. Fix the record field access inference path.
2. Cross-module type propagation: only single-module analysis works today. Multi-file `musi check` needs import resolution wired into sema.
3. Goal: `musi check` should be reliable enough that real errors are caught without false positives on valid code.

### Track C: `std/core/Array.ms` import verification (LOW PRIORITY)

1. Verify `import { array_map } from "std/core/Array"` works end-to-end from a user `.ms` file.
2. The file exists and the functions are defined, but the import chain has not been fully tested.
3. May need `MUSI_STD` env var set correctly to resolve `"std/core/Array"` path.

### Track D: Deno-style CLI expansion (LOW PRIORITY)

1. `musi fmt` -- basic formatter (indentation normalization). Requires a pretty-printer for the AST.
2. `musi test` -- convention-based test runner (files ending in `_test.ms`).

---

## Important Context and Gotchas

### Build constraints (CRITICAL -- OOM risk)
- **32GB unified memory, ~12GB free** -- `cargo test --workspace` WILL crash the machine
- `.cargo/config.toml` sets `CARGO_BUILD_JOBS=3`, `CARGO_INCREMENTAL=0`, `RUSTFLAGS="-Cdebuginfo=1 -Ccodegen-units=1"`
- Correct test workflow: `cargo build --tests -p <crate>` then `timeout 5s ./target/debug/deps/<crate>-<hash>`
- For `musi_sema`: use `CARGO_BUILD_JOBS=1` and `timeout 120s`
- Agent teams: max 1-3 agents; never let agents run broad `cargo test`

### Architecture decisions (LOCKED)
- `extrin` keyword (not `extern`, not `foreign`) -- "extrinsic" = from outside the program
- Array mutation: `Rc<RefCell<Vec<Value>>>` -- in-place, consistent with `var` semantics
- Sema pipeline: fix false positives first, then expose via `musi check`; `musi run` NEVER runs sema (fast path)
- For-loop iteration target: `Iterable['C, 'E]` type class (not per-type special cases)
- CLI: Deno-style subcommands (`musi run`, `musi check`, future: `musi fmt`, `musi test`)
- `0xFFFF` (u16::MAX) sentinel in `SymbolEntry.intrinsic_id` means "not an intrinsic" -- used to distinguish extrin FFI functions from built-in intrinsics

### Things tried that DID NOT work
- `(...)` for type params -- violated LL(1); switched to `[...]`
- `::` for module qualification -- taken by `BinOp::Cons`
- `const Int := IntOps.{ max }` for primitive ops -- shadows type name; UFCS is the solution
- `cargo test -p musi_sema` from agents -- OOM crash; must use per-binary timeout approach
- `native` keyword -- too overloaded in Rust context; `foreign` -- too generic; settled on `extrin`

### Key syntax reminders
- Comments: `//` (C-style), NOT `--`
- No explicit `main` function -- root file IS the entrypoint
- Array type: `[]'T`; indexed access: `arr.[i]`; indexed assign: `arr.[i] <- v`
- Import: `import { name } from "path"`
- Bind: `const x := val;`; mutation: `var x := val; x <- newval;`
- Extrin: `extrin fn sqrt(x: Float): Float;` or `extrin "C" fn ...` or `extrin "C" { fn ...; }`

### Intrinsic ID table (locked, sequential)
```
writeln=0, write=1, int_to_string=2, float_to_string=3, string_length=4,
nat_to_string=5, string_concat=6, string_slice=7, string_to_int=8,
string_contains=9, float_sqrt=10, float_pow=11, float_floor=12,
float_ceil=13, read_line=14, array_length=15, array_push=16,
array_pop=17, array_get=18, array_set=19, array_slice=20
```

### Opcode map (array range)
```
NewArr=0x90, ArrGet=0x91, ArrSet=0x92, ArrLen=0x93, ArrPush=0x94, ArrSlice=0x95
```

---

## Key Files Reference

| File | Role |
|------|------|
| `grammar.ebnf` | Canonical LL(1) grammar (source of truth) |
| `CLAUDE.md` | Language design rationale and key grammar decisions |
| `~/.claude/projects/.../memory/MEMORY.md` | Persistent memory: workflow, conventions, all design decisions |
| `crates/musi_lex/src/token.rs` | Token enum (includes `Extrin` keyword) |
| `crates/musi_ast/src/lib.rs` | AST types (`Modifier::Extrin`, `Expr::ForeignBlock`) |
| `crates/musi_parse/src/parser.rs` | LL(1) parser |
| `crates/musi_parse/src/sexpr.rs` | S-expression debug printer |
| `crates/musi_codegen/src/emitter.rs` | Main codegen (very long -- array codegen, for-in loop, extrin handling) |
| `crates/musi_codegen/src/intrinsics.rs` | `Intrinsic` enum and name table (21 entries) |
| `crates/musi_codegen/src/opcode.rs` | Bytecode instruction set (0x00-0x95) |
| `crates/musi_codegen/src/module.rs` | `SymbolEntry` (has `abi`, `link_lib`, `link_name`) and `.mso` serialization |
| `crates/musi_vm/src/vm.rs` | VM execution loop (FFI dispatch at `intrinsic_id == 0xFFFF`) |
| `crates/musi_vm/src/ffi.rs` | NEW -- dlopen/dlsym FFI dispatch (`FfiState`) |
| `crates/musi_vm/src/value.rs` | `Value` enum (includes `Array(Rc<RefCell<Vec<Value>>>)`) |
| `crates/musi_vm/src/native.rs` | Built-in intrinsic dispatch (21 intrinsics including 6 array ones) |
| `crates/musi_vm/src/error.rs` | `VmError` enum (includes `FfiFailed`, `FunctionOutOfBounds`, `SymbolOutOfBounds`) |
| `crates/musi_sema/src/check.rs` | Hindley-Milner type checker (~1216+ lines) |
| `crates/musi_sema/src/tests.rs` | 18 sema tests |
| `crates/musi/src/main.rs` | CLI driver (`musi run` + `musi check` subcommands) |
| `std/prelude.ms` | Auto-imported prelude (primitive types, classes, `Iterable` stub) |
| `std/core/Array.ms` | NEW -- array stdlib (map/filter/fold/each/contains/reverse/concat) |
| `tools/vscode/syntaxes/musi.tmLanguage.json` | VS Code TextMate grammar (includes `extrin` keyword) |

---

## Suggested Next Steps (Wave 4)

1. **Commit all wave 1-3 work.** There are 31 modified + 2 new files, all uncommitted. Stage and commit before starting wave 4 to create a clean baseline. Suggested commit message: `feat: extrin FFI, arrays, musi check, array stdlib (waves 1-3)`.

2. **Wire `Iterable` for arrays.** Add `given Iterable[[]'T, 'T] { fn iter_len ... fn iter_get ... };` to `std/prelude.ms`. Then modify the for-in codegen in `emitter.rs` to dispatch through `CallMethod` for `iter_len`/`iter_get` instead of hard-coded `ArrLen`/`ArrGet`. Keep the array-specific fast path as an optimization if desired.

3. **Fix sema record field access.** In `crates/musi_sema/src/check.rs`, find where record field access (`expr.field`) returns a fresh type variable and instead look up the field type from the record's structural type. This is the biggest remaining false-positive source.
