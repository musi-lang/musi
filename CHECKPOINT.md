# Musi Compiler -- Session Checkpoint (2026-03-06)

Read `CLAUDE.md` for grammar design rules (LL(1), mathematical purity).
Read `MEMORY.md` at `~/.claude/projects/-Users-krystian-CodeProjects-musi/memory/MEMORY.md` for full technical memory (workflow constraints, naming conventions, phase history).

---

## Original Goal

Build the Musi programming language compiler end-to-end: lexer, parser, semantic analysis, bytecode codegen, and stack-based VM. The language is strictly LL(1), expression-oriented, with product/sum types, type classes, pattern matching, and a growing standard library.

This session added the functional List data structure, 9 new intrinsics (string/float/IO), and 3 new examples (12-list, 13-strings, 14-io).

---

## Completed Work

### Commits (newest first)

1. **`(pending)`** -- `std/core/List.ms` (recursive choice, fold, map, filter, etc.), 9 new intrinsics (string_concat/slice/to_int/contains, float_sqrt/pow/floor/ceil, read_line), updated string/float/io std modules, examples 12-14
2. **`d0542b3`** -- std intrinsics (`string_length`=4, `nat_to_string`=5), fix string literal quote stripping, `fn_return_types` tracking in `EmitState`, `Float.ms` dedup, example 11 (std-modules)
3. **`ee347bc`** -- dot-prefix patterns (`.Variant`), match guards (`if cond`), `if case` / `while case`, `var` in patterns (152 tests total)
4. **`94e4455`** -- type classes (`class`/`given`/`satisfies`), UFCS dispatch, operator overloading via `method_table`, UDN (`Value::Object` with `type_tag`, `NewObj` 5-byte opcode, `Question`/`QuestionDot`/`QuestionQuestion` tokens, `Expr::DotPrefix`, `Ty::Option`, `BinOp::NilCoalesce`, `PostfixOp::OptField`)

### Test Counts (all passing)

| Crate          | Tests |
|----------------|-------|
| musi_shared    | 34    |
| musi_lex       | 57    |
| musi_parse     | 51    |
| musi_codegen   | 16    |
| musi_vm        | 36    |

All 14 examples pass end-to-end (verified at checkpoint time; 14-io requires piped stdin).

### Key Implementation Details

**Type classes and UFCS:**
- `class Eq['T] { fn eq(a: 'T, b: 'T): Bool; };` defines a type class
- `given Eq[Int] { fn eq(a: Int, b: Int): Bool => a = b; };` provides an instance
- `satisfies` keyword for superclass constraints (parsed, not enforced at codegen)
- UFCS: `a.max(b)` desugars to `max(a, b)` -- sema/codegen rewrite, no grammar change
- `EmitState.class_method_names: HashSet<String>` tracks methods declared in any `class` body
- `Module.method_table: Vec<MethodEntry>` maps (method name, type_tag) to fn_idx at runtime
- VM `try_dispatch_binop()` checks if LHS is `Object` with `type_tag != 0` and dispatches to method_table; falls through to primitive ops otherwise
- Primitive type tags in `type_tag_of()`: Int=1, Float=2, String=4, Unit=5, Function=6, Object=from field

**Operator overloading:**
- Binary ops (`AddI64`, `SubI64`, `MulI64`, `DivI64`, `RemI64`, `EqI64`, `NeqI64`, `LtI64`, `GtI64`, `LeqI64`, `GeqI64`) all fall back to `try_dispatch_binop("add")` etc. when LHS is Object
- `CallMethod { method_idx: u16, arg_count: u16 }` opcode (0x83, 5 bytes) dispatches by receiver type_tag + method name from const pool

**User-Defined Nullables (UDN):**
- `NilCoalesce` opcode (0x84): pops rhs (default), pops lhs (Option); pushes inner if Some, else pushes rhs
- `OptField(u16)` opcode (0x85): optional chaining -- if None, propagate None; if Some(inner), access field and wrap in Some
- Tokens: `Question` (`?`), `QuestionDot` (`?.`), `QuestionQuestion` (`??`)

**Pattern matching improvements:**
- Dot-prefix patterns: `.Some(x)` -- leading dot disambiguates constructor vs variable binding
- Match guards: `| .Some(n) if n > 0 => ...`
- `if case .Some(v) := expr then ...` -- refutable pattern in if condition
- `while case .Some(v) := iter.next() loop ...` -- refutable pattern in while condition
- `var` in patterns binds mutable (same slot semantics, just a marker)

**String literal fix:** Emitter was storing string literals with surrounding quotes. Fixed in `emit_lit` to strip quotes before adding to const pool.

**`fn_return_types`:** `EmitState` now tracks declared return types so `const q := f(); q.field` can resolve field names when `f` returns a known named type.

**Intrinsic table (final):**

| ID | Name               | Signature                         |
|----|--------------------|-----------------------------------|
| 0  | `writeln`          | `(String) -> Unit`                |
| 1  | `write`            | `(String) -> Unit`                |
| 2  | `int_to_string`    | `(Int) -> String`                 |
| 3  | `float_to_string`  | `(Float) -> String`               |
| 4  | `string_length`    | `(String) -> Int`                 |
| 5  | `nat_to_string`    | `(Nat) -> String`                 |
| 6  | `string_concat`    | `(String, String) -> String`      |
| 7  | `string_slice`     | `(String, Int, Int) -> String`    |
| 8  | `string_to_int`    | `(String) -> Option[Int]`         |
| 9  | `string_contains`  | `(String, String) -> Bool`        |
| 10 | `float_sqrt`       | `(Float) -> Float`                |
| 11 | `float_pow`        | `(Float, Float) -> Float`         |
| 12 | `float_floor`      | `(Float) -> Float`                |
| 13 | `float_ceil`       | `(Float) -> Float`                |
| 14 | `read_line`        | `() -> String`                    |

**Import system:**
- BFS multi-file compilation in CLI (`main.rs`): collects import/export-from paths, resolves transitively
- Relative imports (`./foo`, `../bar`) resolve from importing file's directory
- Non-relative imports resolve via `MUSI_STD` env var, or `./std/` from cwd
- `collect_dep_paths()` strips surrounding quotes from interned path symbols

**Standard library modules (all working):**

| Path               | Contents                                           |
|--------------------|----------------------------------------------------|
| `std/prelude.ms`   | Primitive types, Bool, Option, Expect, type classes (Eq, Ord, Add, Sub, Mul, Div, Rem, Show), given instances for Int/Float/Bool/String |
| `std/core/Bool.ms` | Bool utilities                                     |
| `std/core/Option.ms` | `unwrap_or`, `map`, `is_some`, `is_none`         |
| `std/core/Expect.ms` | `unwrap_pass`, `unwrap_fail`, `map_pass`         |
| `std/core/Ordering.ms` | `choice Ordering { Lt \| Eq \| Gt }`, `ordering_reverse` |
| `std/core/Pair.ms` | `record Pair { first, second }`, `pair_new`        |
| `std/num/Int.ms`   | `int_abs`, `int_clamp`, `int_max`, `int_min`       |
| `std/num/Float.ms` | Float utilities                                    |
| `std/num/Nat.ms`   | Nat utilities                                      |
| `std/string/String.ms` | `string_length/concat/slice/to_int/contains`, `string_repeat`, `string_is_empty` |
| `std/io/Io.ms`     | `read_line` (intrinsic)                            |
| `std/core/List.ms` | `choice List['T] { Nil \| Cons('T, List['T]) }`, `list_length/append/reverse/map/filter/fold/head/tail/any/all/sum` |

---

## Current State

- **Branch:** `main`
- **Working tree:** Uncommitted changes (ready to commit)
- **Latest commit:** `d0542b3` (new work pending commit)
- **Build:** Compiles successfully. All tests pass. All 14 examples run correctly.
- **No failing tests, lint errors, or build issues.**
- **`musi_sema` exists but is NOT wired into the pipeline** -- the CLI goes straight from parse to codegen, skipping type checking. The sema crate has 18 tests of its own but is not invoked during `musi run`.

### File sizes of key source files

| File | Lines |
|------|-------|
| `crates/musi_codegen/src/emitter.rs` | 1970 |
| `crates/musi_parse/src/parser.rs` | 1838 |
| `crates/musi_sema/src/check.rs` | 1216 |
| `crates/musi_lex/src/lexer.rs` | 616 |
| `crates/musi_ast/src/lib.rs` | 567 |

---

## Remaining Work (prioritized)

### High Priority -- Functional Programming Foundations

1. ~~**Write `std/core/List.ms`**~~ -- DONE. Recursive choice, full set of operations.
2. ~~**Add string intrinsics**~~ -- DONE. `string_concat/slice/to_int/contains` added (IDs 6-9).
3. ~~**Add math intrinsics for Float**~~ -- DONE. `float_sqrt/pow/floor/ceil` added (IDs 10-13). `float_round` in Float.ms as pure Musi.
4. ~~**Add `read_line` IO intrinsic**~~ -- DONE. ID 14, declared in `std/io/Io.ms`.

**Remaining high priority:**

1. **`string_chars` / `string_from_runes`** -- Split a string into a `List[Rune]` or `List[String]` (char-by-char). Requires intrinsic that builds a List at the Rust level. Somewhat complex since List type_tag is user-defined. Alternative: add `string_get(s, i)` (char at index → String) and let Musi code loop.

2. **`string_get(s, i): String`** -- Get single character at index as a one-char String. Simple intrinsic, enables manual string iteration.

### Medium Priority -- Completing the Type System Story

5. **`Not` and `Xor` type class dispatch** -- `not` and `xor` are pure operators that should get `Not['T]` / `Xor['T]` classes + VM dispatch fallback (same pattern as Add/Sub). `and`/`or` intentionally NOT overloadable (short-circuit semantics).

6. **Bitwise operator type classes** -- `BitAnd['T]`, `BitOr['T]`, etc. Lower priority since bitwise ops on user types are rare.

7. **Wire `musi_sema` into the pipeline** -- Currently the CLI skips type checking entirely. Integrating sema would catch type errors before codegen. This is a significant integration task since sema needs to accept the same multi-module structure as codegen.

### Lower Priority -- Infrastructure

8. **`while case` end-to-end verification** -- Codegen for `while case .Some(v) := iter.next() loop` was implemented but no example exercises it. Write one.

9. **CLI subcommands** -- `musi fmt`, `musi test`, `musi bench`, `musi publish`. Schema fields exist in `mspackage.json` but CLI only has `musi run`.

10. **Remote dependency fetching** -- `dependencies` field in mspackage.json schema exists. Need: `~/.musi/packages/` cache, hash verification, tarball unpack. `msr:` registry prefix deferred.

11. **Mutable heap / Vec type** -- No mutable data structures beyond `var` locals. Need either VM-level array type or persistent immutable vector for practical programming.

12. **C interop / FFI** -- Needed for graphics (gl.h, glfw). Requires stable ABI + LLVM backend or WASM target. Architectural decision deferred.

13. **Compiler self-hosting** -- Writing Musi-to-bytecode in Musi. Needs file IO, string ops, mutable data structures first.

---

## Important Context and Gotchas

### Memory constraints (CRITICAL for development)
- **32GB unified memory, ~12GB free** -- OOM kills are real
- **Never run `cargo test --workspace`** -- will crash
- **musi_sema compilation is dangerous** -- 80GB swap + 27GB RAM crash observed. Use `CARGO_BUILD_JOBS=1` and `timeout 120s`
- **Correct test workflow:**
  1. `cargo build --tests -p <crate>`
  2. `ls -t target/debug/deps/<crate>-*` (no extension = executable, newest first)
  3. `timeout 5s ./target/debug/deps/<crate>-<hash>`
- `.cargo/config.toml` exists with `CARGO_INCREMENTAL=0`, `CARGO_BUILD_JOBS=3`, `RUSTFLAGS="-Cdebuginfo=1 -Ccodegen-units=1"`

### Grammar constraints (non-negotiable)
- **Strictly LL(1)** -- every parse decision requires exactly one token of lookahead, no exceptions
- **Mathematical purity** -- record uses `,` (conjunction, product), choice uses `|` (disjunction, sum), both use `{}`
- Type parameters use `[...]` not `<...>` (LL(1) -- angle brackets are ambiguous with comparison)
- See `CLAUDE.md` for full grammar design rationale

### Naming conventions
- Variant names: UpperCamelCase
- Tokens: named after symbol shape (`DotDot`, `Eq`, `SlashEq`, `LtDotDot`)
- Intrinsic attrs use string form: `#[intrinsic("writeln")]`
- Comment policy: comments are noise. Only where logic is non-obvious. Section dividers: `// -- Name ---`

### Architecture decisions
- `emit()` signature: `emit(prelude, deps, user, interner) -> Result<Module, CodegenError>` -- three-tier: prelude (built-in), deps (imported), user (main file)
- Main function is always the last entry in `function_table`
- `Value::Object { type_tag: u16, fields: Rc<Vec<Value>> }` -- type_tag 0 = anonymous/Bool, 1-9 reserved for primitives in `type_tag_of()`, 10+ = user-defined types
- Choices: field[0] = discriminant (Int), field[1..] = payload
- Bool is `Object { type_tag: 0, fields: [Int(0)] }` for False, `[Int(1)]` for True
- `method_table` is runtime-only, not serialized in `.mso` binary format
- `musi_sema` exists but is NOT in the pipeline -- codegen runs directly on parsed AST

### Things that were tried and did NOT work
- `(...)` for type parameters -- required peek2() to distinguish `('T)` from `(x)`, violating LL(1). Fixed to `[...]`.
- `::` for module qualification -- taken by `BinOp::Cons` (list cons). Cannot be repurposed.
- `const Int := IntOps.{ max }` for associating operations with primitives -- shadows the type name. UFCS is the solution.
- Running `cargo test -p musi_sema` or any broad test command from agents -- causes OOM. Must use binary directly with `timeout`.

### Planned grammar additions (tokens exist, not fully wired)
- `?'T` -- sugar for `Option['T]` (needs `Question` token -- token EXISTS)
- `expr?.field` -- optional chaining (needs `QuestionDot` -- token EXISTS, `OptField` opcode EXISTS)
- `expr ?? dflt` -- nil coalescing (needs `QuestionQuestion` -- token EXISTS, `NilCoalesce` opcode EXISTS)
- `for case` -- decided NOT to implement (use `for` + `if case` inside body)

---

## Key Files Reference

| File | Role |
|------|------|
| `CLAUDE.md` | Project instructions for AI assistants (grammar constraints, design rationale) |
| `grammar.ebnf` | Canonical language grammar (source of truth) |
| `std/prelude.ms` | Standard prelude (auto-imported, embedded via `include_str!`) |
| `crates/musi/src/main.rs` | CLI binary (`musi run <file.ms>`), BFS import resolution |
| `crates/musi_ast/src/lib.rs` | AST node types (Expr, Ty, Pat, etc.) |
| `crates/musi_lex/src/lexer.rs` | Lexer (616 lines) |
| `crates/musi_lex/src/token.rs` | Token kinds (33 keywords + compounds) |
| `crates/musi_parse/src/parser.rs` | LL(1) recursive-descent parser (1838 lines) |
| `crates/musi_sema/src/check.rs` | Type checker (1216 lines, NOT wired into pipeline) |
| `crates/musi_codegen/src/emitter.rs` | Bytecode emitter (1970 lines) |
| `crates/musi_codegen/src/intrinsics.rs` | Intrinsic enum (6 variants, IDs 0-5) |
| `crates/musi_codegen/src/opcode.rs` | Opcode enum (42 variants) + encode/decode |
| `crates/musi_codegen/src/module.rs` | Module container (const pool, symbols, functions, code, methods) |
| `crates/musi_vm/src/vm.rs` | Stack-based VM (674 lines) |
| `crates/musi_vm/src/native.rs` | Native intrinsic dispatch |
| `crates/musi_vm/src/value.rs` | Runtime value type (Int, Float, String, Unit, Function, Object) |
| `crates/musi_shared/src/` | Shared infrastructure (Span, Source, Interner, Diagnostics, Arena) |
| `.claude/plans/` | Phase-by-phase implementation plans (01-12) |
| `tools/vscode/syntaxes/musi.tmLanguage.json` | VS Code TextMate syntax highlighting |
| `mspackage.json` | Package manifest (project root) |

---

## Suggested Next Steps

1. **Write `std/core/List.ms`** -- Define `choice List['T] { Nil | Cons('T, List['T]) }` and helper functions (`map`, `filter`, `fold`, `length`, `append`, `reverse`). Create `examples/12-list/main.ms` to exercise it. This is purely Musi code -- no Rust changes needed.

2. **Add `string_slice` and `string_to_int` intrinsics** -- Add variants to `Intrinsic` enum (IDs 6, 7), add dispatch in `native.rs`, declare in `std/string/String.ms`. Pattern: copy how `string_length` was added in commit `d0542b3`.

3. **Add `read_line` IO intrinsic** -- ID 8, reads a line from stdin, returns `String`. Declare in `std/io/Io.ms`. This unblocks interactive programs and is the first step toward a REPL.
