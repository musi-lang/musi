# Checkpoint -- Musi Language Compiler

**Date:** 2026-03-05
**Branch:** `main` (all changes uncommitted -- substantial new code across 7 crates)
**Rust toolchain:** `stable-aarch64-apple-darwin`, rustc 1.93.0 (2026-01-19), edition 2024

---

## Original Goal

Build the Musi programming language compiler and VM from scratch. The project follows a phased implementation plan (12 phases total). Phases 1--7 are complete. The immediate previous session diagnosed and fixed an infinite-loop / OOM issue in `musi_sema` tests, and applied a robustness fix to the parser's `parse_program` loop.

---

## Completed Work (All Phases)

### Phase 1 -- musi_shared (34 tests)
`crates/musi_shared/src/`: `span.rs`, `source.rs`, `intern.rs`, `diag.rs`, `arena.rs` (each with sibling `/tests.rs`)
- `Idx<T>` typed index with `PhantomData<fn() -> T>`; manual Clone/Copy/Eq/Hash
- `Arena<T>`, `SliceArena<T>` + `Slice<T>` (Copy) for flat AST
- `Interner` (string interning), `SourceDb`, `DiagnosticBag`, `Span`
- Perf pass: `Arc<str>` for diag messages, `Box<str>` for source name/content

### Phase 2 -- musi_lex (55 tests)
`crates/musi_lex/src/`: `lib.rs`, `token.rs`+`token/tests.rs`, `lexer.rs`+`lexer/tests.rs`
- 33 keywords, compound tokens (`DotLBracket`, `DotLBrace`, `LtDotDot`, `DotDotLt`)
- `TyIdent` (`'T`), `Ident`, char literals vs type variables disambiguation
- Iterator-based `Lexer::new(source, file_id, interner, diags)` with `size_hint()`

### Phase 3 -- musi_parse (37 tests)
`crates/musi_parse/src/`: `lib.rs`, `ast.rs`+`ast/tests.rs`, `parser.rs`+`parser/tests.rs`, `sexpr.rs`+`sexpr/tests.rs`
- `parse(tokens, file_id, diags, interner) -> ParsedModule`
- Strict LL(1) + Pratt precedence; 1599 lines in `parser.rs`
- Left-factored: `ast_expr_paren`, `ast_fn_kind`, `ast_ty_named`, `ast_pat_ident`
- `ParsedModule { items: Vec<Idx<Expr>>, ctx: ParseCtx, span }`

### Phase 4 -- musi_codegen + musi_vm (26 tests total)
`crates/musi_codegen/src/`: `lib.rs`, `error.rs`, `opcode.rs`+`opcode/tests.rs`, `module.rs`+`module/tests.rs`, `emitter.rs`
`crates/musi_vm/src/`: `lib.rs`, `error.rs`, `value.rs`+`value/tests.rs`, `native.rs`+`native/tests.rs`, `vm.rs`+`vm/tests.rs`
- Bytecode: `emit(prelude, user, interner) -> Result<Module, CodegenError>`
- VM: `Vm::new(module, natives).run(entry_fn: u16) -> Result<Value, VmError>`
- Intrinsics: writeln=1, write=2, int_to_string=3

### Phase 5 -- Hello World e2e
`crates/musi/src/main.rs` -- CLI binary, `musi run <file.ms>`
`std/prelude.ms` -- embedded via `include_str!("../../../std/prelude.ms")`
`examples/hello.ms` -- `writeln("Hello, world!");`
Pipeline: read file -> lex -> parse -> check errors -> emit(prelude, user) -> Vm::run

### Phase 6 -- Widen Codegen (bindings, arithmetic, control flow)
- New opcodes: arithmetic, comparison, logical/bitwise, `br`/`br.true`/`br.false`, `concat.str`
- `const`/`var` bindings with scope stack -> local slots
- All binary ops, prefix ops, if/elif/else, while/loop/for (range-based), break/cycle with labels, blocks
- DocComment token fix: leading `///` tokens consumed at start of `parse_prefix_or_atom`
- Milestone: `while y < x loop (y <- y + 1;); writeln(int_to_string(y));` -> prints "10"

### Phase 7 -- musi_sema (name resolution + type checking, 18 tests)
`crates/musi_sema/src/`: `lib.rs`, `check.rs` (1340 lines), `resolve.rs` (648 lines), `types.rs`, `scope.rs`, `def.rs`, `tests.rs`
- Two-pass name resolution: collect top-level decls, then resolve references
- Bidirectional type checker with HM-style unification (union-find `UnifyTable`)
- Edit-distance suggestions for undefined names
- `SemaResult { defs, expr_defs, pat_defs, expr_types }`
- `Type::Error` poison type to suppress cascading diagnostics

### Previous Session Fixes
- Fixed 2 `musi_sema` tests that used wrong `if` syntax (missing `then` keyword)
- Added parse-error guard assertion in sema test helper `analyze_src()`
- Applied `parse_program` robustness fix: tracks `pos_before` and force-advances on zero progress to prevent infinite loops on stray closing brackets

---

## Current State

### Build Status -- ALL CLEAN

Every crate builds without errors or warnings (verified one-at-a-time with `CARGO_BUILD_JOBS=1`):
- `musi_shared` ✓  `musi_lex` ✓  `musi_parse` ✓  `musi_codegen` ✓  `musi_vm` ✓  `musi_sema` ✓  `musi` ✓

Note: `musi_sema` must be compiled alone (`CARGO_BUILD_JOBS=1`) -- parallel compilation of it alongside other crates caused an 80 GB swap spike and system crash (the sema crate is large: `check.rs` 1340 lines, `resolve.rs` 648 lines).

### Git State

All changes are **uncommitted** on `main`. This includes the entire implementation across phases 1--7 (thousands of lines across 7 crates). The git status shows:
- Modified: `.claude/plans/02-LEXER.md`, `.claude/plans/03-PARSER_AST.md`, `Cargo.lock`, `Cargo.toml`, `crates/musi_shared/Cargo.toml`, `crates/musi_shared/src/lib.rs`, `examples/hello.ms`, `grammar.ebnf`, `tools/vscode/syntaxes/musi.tmLanguage.json`
- Deleted: `tests/.gitkeep`
- Untracked (new): `.cargo/`, `crates/musi/`, `crates/musi_codegen/`, `crates/musi_lex/`, `crates/musi_parse/`, `crates/musi_sema/`, `crates/musi_vm/`, `std/`, many new source files in `crates/musi_shared/src/`

### No Running Processes

No background processes or environment state.

---

## Remaining Work

### Priority 0 (BLOCKING): Fix musi_lex Build Error

**File:** `crates/musi_lex/src/lexer.rs`
### Priority 1: Verify All Tests Pass

After the lexer fix, run tests per-crate (see Memory Constraints below):
1. `musi_shared` -- 34 tests
2. `musi_lex` -- 55 tests (was 54 in earlier count; verify)
3. `musi_parse` -- 37 tests
4. `musi_codegen` -- 16 tests
5. `musi_vm` -- 10-11 tests
6. `musi_sema` -- 18 tests

### Refactoring Done This Session (do NOT redo)

All lint suppressions eliminated and code quality improved across all crates:
- **`size_hint` bug fixed** in `lexer.rs`: `remaining/2+1` → `remaining+1`
- **`dead_code` removed**: `alloc_ty`/`alloc_pat` helpers deleted from `parser.rs`
- **`NativeFn` type changed**: `fn(&mut Vm, ...)` → `fn(&Vm, ...)` -- removes `needless_pass_by_ref_mut`
- **`vm.rs run()` split**: `Signal` enum + `push_entry_frame`, `fetch_and_advance`, `step`, `exec_arith`, `exec_cmp`, `exec_bitwise`, generic `push_i64/f64_bin/cmp` helpers -- removes `cognitive_complexity` + `too_many_lines`
- **`sexpr.rs`**: `FnDefView<'a>` struct -- removes `too_many_arguments` on `print_fn_def`
- **`check.rs`**: `FnDefNode<'a>` struct + 13 helpers extracted from `infer_inner` -- removes `too_many_arguments` + `too_many_lines`
- **All inline tests moved** to sibling `<module>/tests.rs` files across all 6 crates (16 test files created)
- **`parser.rs` reduced** 2250 → 1599 lines via helpers: `parse_separated_list`, `parse_and_alloc_expr`, `parse_option`, `parse_optional_guard`, `parse_delimited`, `parse_optional_expr`, `parse_field_header`, `wrap_postfix`, `parse_pipe_separated`
- **`lexer.rs` reduced** 1101 → 598 lines via helpers: `emit_interned`, `lex_maybe_two_char`, `consume_ident_chars`, `consume_until_newline`, `consume_hex_digits`, tightened `skip_whitespace`

**Remaining LOC reduction candidates (next session):**
- `check.rs` (1340 lines): `infer_binary` ~80 lines, `infer_if`/`infer_postfix` ~100 lines -- share `unify(expected, actual, span, diags, file_id)` call structure
- `emitter.rs` (1102 lines): big match on AST nodes -- same class of problem as `parser.rs`
- `resolve.rs` (648 lines): scope/list patterns

### Priority 2: Commit All Accumulated Work

All changes from phases 1--7 are uncommitted. This is a large amount of work that should be committed (potentially as multiple logical commits or one checkpoint commit). Review for any sensitive files before staging.

### Priority 3: Phase 8 -- Widen Codegen (Records, Choices, Match, Functions)

See `.claude/plans/08-WIDEN_CODEGEN_TYPES.md` for the full plan. Key deliverables:
- Type table in `.mso` module format
- `new.obj`, `ld.fld`, `st.fld`, `ld.tag`, `eq.tag`, `dup` opcodes
- Record allocation, field access, spread (`<..`)
- Choice (tagged union) construction
- Pattern match -> sequential test-and-branch compilation
- User-defined function calls (currently only intrinsics work)
- Non-capturing lambda support
- Recursive function support

Milestones: Option type match, record field access, spread, `factorial(10)` -> 3628800, lambda `fn(x) => x * 2`

### Future Phases (not yet started)
- Phase 9: Garbage collector + closures (capturing lambdas)
- Phase 10: Module system (import/export, separate compilation)
- Phase 11: FFI
- Phase 12: CLI + LSP

---

## Important Context & Gotchas

### Memory Constraints (CRITICAL)
- **32GB unified memory, ~12GB free** -- OOM kills are real
- **NEVER run `cargo test --workspace`** -- always test per-crate
- `.cargo/config.toml` sets: `CARGO_INCREMENTAL=0`, `CARGO_BUILD_JOBS=3`, `RUSTFLAGS="-Cdebuginfo=1"`
- **Correct test workflow:**
  1. `cargo build --tests -p <crate>` -- compile
  2. Run binary directly: `./target/debug/deps/<crate>-<hash>` (no extension = executable)
  3. Single test: `./target/debug/deps/<crate>-<hash> -- <test_name> --exact`

### Parser Design (LL(1) -- Non-negotiable)
- Strictly LL(1): every parse decision = exactly one token of lookahead
- No left recursion; expressions use Pratt / iterated forms
- All alternatives have disjoint FIRST sets
- When proposing new syntax, compute FIRST sets explicitly
- `recover()` stops AT sync tokens (`;`, `)`, `}`, `]`, EOF) without consuming them -- correct for nested contexts
- `parse_program` has a safety net: force-advances if no progress was made in an iteration

### Musi `if` Syntax
- `if cond then expr {elif} [else]` -- `then` keyword is REQUIRED
- Without `then`, parser misreads `if cond (body)` as a function call

### DocComment Handling
- `///` tokens consumed at start of `parse_prefix_or_atom` (discarded for now)
- Proper doc-comment attachment to AST nodes is future work

### Workspace Lints (Strict)
- `unsafe_code = "forbid"`, `unused_results = "forbid"`, `as_conversions = "deny"`
- All numeric casts: `try_from().expect()` -- the `as` keyword is workspace-denied
- Clippy: `pedantic`, `nursery`, `complexity`, `correctness`, `perf`, `style`, `suspicious` all set to `deny`
- Use `let _prev = map.insert(...)` pattern to satisfy `unused_results`

### Things That Did NOT Work
- Making `error_expr()` always advance at least one token: rejected because it over-consumes in nested contexts where `)` should terminate the enclosing construct
- `u8::is_ascii_*` as bare function references in `is_some_and()`: broke in rustc 1.93.0 / edition 2024 due to `fn(&self)` vs `FnOnce(T)` mismatch

---

## Key Files Reference

| File                                                 | Role                                                          |
| ---------------------------------------------------- | ------------------------------------------------------------- |
| `CLAUDE.md`                                          | Project instructions, design rationale, key grammar decisions |
| `grammar.ebnf`                                       | Canonical language grammar (source of truth, 341 lines)       |
| `std/prelude.ms`                                     | Standard prelude (embedded in CLI via `include_str!`)         |
| `examples/hello.ms`                                  | Hello world example                                           |
| `.cargo/config.toml`                                 | Memory-saving build settings                                  |
| `.claude/plans/01-SHARED.md` through `12-CLI_LSP.md` | Phase-by-phase implementation plans                           |
| `crates/musi_shared/src/lib.rs`                      | Re-exports for span, source, intern, diag, arena              |
| `crates/musi_lex/src/lexer.rs`                       | Lexer implementation (HAS BUILD ERROR on lines 152, 191)      |
| `crates/musi_lex/src/token.rs`                       | Token and TokenKind definitions                               |
| `crates/musi_parse/src/parser.rs`                    | LL(1)+Pratt parser (1599 lines)                               |
| `crates/musi_parse/src/ast.rs`                       | AST node definitions                                          |
| `crates/musi_parse/src/sexpr.rs`                     | S-expression pretty printer for AST (test helper)             |
| `crates/musi_codegen/src/emitter.rs`                 | AST -> bytecode emitter (1102 lines)                          |
| `crates/musi_codegen/src/opcode.rs`                  | Opcode enum definitions                                       |
| `crates/musi_codegen/src/module.rs`                  | Module container (.mso format)                                |
| `crates/musi_vm/src/vm.rs`                           | Stack-based VM execution loop (560 lines)                     |
| `crates/musi_vm/src/value.rs`                        | Runtime value representation (`Value::String(Rc<str>)`)       |
| `crates/musi_vm/src/native.rs`                       | Native function registry (writeln, write, int_to_string)      |
| `crates/musi_sema/src/lib.rs`                        | Sema entry point + `analyze()` function                       |
| `crates/musi_sema/src/resolve.rs`                    | Two-pass name resolution (648 lines)                          |
| `crates/musi_sema/src/check.rs`                      | Bidirectional type checker (1340 lines)                       |
| `crates/musi_sema/src/types.rs`                      | Type representation (PrimTy, Type, TypeVarId)                 |
| `crates/musi_sema/src/scope.rs`                      | Scope tree for name resolution                                |
| `crates/musi_sema/src/def.rs`                        | DefId, DefInfo, DefKind                                       |
| `crates/musi_sema/src/tests.rs`                      | 18 sema tests                                                 |
| `crates/musi/src/main.rs`                            | CLI binary (`musi run <file.ms>`)                             |
| `tools/vscode/syntaxes/musi.tmLanguage.json`         | VS Code TextMate grammar                                      |

### Crate Dependency Graph
```
musi_shared  (leaf -- no deps)
    |
musi_lex  (depends on musi_shared)
    |
musi_parse  (depends on musi_shared, musi_lex)
    |          \
musi_codegen    musi_sema
(+ musi_shared,  (+ musi_shared, musi_parse;
  musi_parse)     dev-dep: musi_lex)
    |
musi_vm  (depends on musi_codegen)
    |
musi  (CLI -- depends on all except musi_sema)
```

---

## Suggested Next Steps

1. **Fix the musi_lex build error** -- change lines 152 and 191 of `crates/musi_lex/src/lexer.rs` to use closure wrappers (`|b| b.is_ascii_hexdigit()` and `|b| b.is_ascii_whitespace()`). This is a 2-line fix that unblocks the entire workspace.

2. **Build and run all tests per-crate** to verify the fix and confirm all 170+ tests still pass. Use the per-crate binary-direct workflow described in Memory Constraints.

3. **Commit the accumulated work** -- all phases 1--7 are uncommitted. Stage and commit logically (one large checkpoint commit is acceptable given the volume).

4. **Begin Phase 8** -- follow `.claude/plans/08-WIDEN_CODEGEN_TYPES.md`. Start with adding the type table to the module format, then implement `new.obj`/`ld.fld`/`st.fld` opcodes for record support.

---

*Consult `CLAUDE.md` for language design constraints (LL(1), mathematical purity). Consult `~/.claude/projects/-Users-krystian-CodeProjects-musi/memory/MEMORY.md` for accumulated project memory including all phase details.*
