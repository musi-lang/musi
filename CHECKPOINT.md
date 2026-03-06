# Musi Compiler -- Session Checkpoint (2026-03-06)

Read `CLAUDE.md` for grammar design rules (LL(1), mathematical purity).
Read `MEMORY.md` at `~/.claude/projects/-Users-krystian-CodeProjects-musi/memory/MEMORY.md` for full technical memory (workflow constraints, naming conventions, phase history).

---

## Original Goal

Implement type classes for the Musi programming language compiler: design keywords, add lexer/parser/AST support, implement runtime dispatch via a new `CallMethod` opcode, wire type class methods into UFCS, and build out the standard library with class/given definitions in the prelude.

---

## Completed Work

### 1. Type Class Language Design (finalized)

Five new keywords: `class`, `given`, `satisfies`, `where`, `law`.

```
class Eq['T] { fn eq(a: 'T, b: 'T): Bool; };
given Eq[Int] { fn eq(a: Int, b: Int): Bool => a = b; };
given Eq[Option['T]] where 'T satisfies Eq { ... };
fn sort['T](arr: ['T]): ['T] where 'T satisfies Ord => ...;
class Ord['T] satisfies Eq['T] { ... };
```

- Comma-separated multiple constraints and superclasses
- `class`/`given` are NOT exported (coherence rule)
- `law` has syntax but no runtime enforcement

### 2. Lexer (musi_lex) -- 55/55 tests pass

Added `Class`, `Given`, `Satisfies`, `Where`, `Law` to `TokenKind`, `keyword_from_str`, `is_keyword`, `fixed_text`. All keyword-count test assertions updated.

Files modified:
- `crates/musi_lex/src/token.rs`
- `crates/musi_lex/src/token/tests.rs` (keyword count assertions)
- `crates/musi_lex/src/lexer/tests.rs`

### 3. AST (musi_ast) -- new nodes

- `Constraint { ty_var: Symbol, bound: Ty, span: Span }` -- a single `where` constraint
- `ClassMember` enum -- `Method(Idx<Expr>)` or `Law { name, params, body, span }`
- `Expr::ClassDef { name, ty_params, supers, members, span }`
- `Expr::GivenDef { class_app: Ty, constraints, members, span }`
- `Expr::FnDef` and `Expr::Lambda` gained `where_clause: Vec<Constraint>`

File: `crates/musi_ast/src/lib.rs`

### 4. Parser (musi_parse) -- 43/43 tests pass (was 37; 6 new tests)

Full parsing of `class`, `given`, `where`, `satisfies`, `law`. Grammar rules added to `grammar.ebnf` (lines 234-289).

Files modified:
- `crates/musi_parse/src/parser.rs` -- parsing logic
- `crates/musi_parse/src/parser/tests.rs` -- 6 new tests
- `crates/musi_parse/src/sexpr.rs` -- display for new nodes
- `crates/musi_parse/src/sexpr/tests.rs` -- s-expr tests

### 5. Codegen (musi_codegen) -- 16/16 tests pass

**New opcode**: `CallMethod { method_idx: u16, arg_count: u16 }` (byte 0x83, 5 bytes total).
- `method_idx` = const-pool index for method name string
- `arg_count` = total args including receiver as arg0

**New module field**: `Module.method_table: Vec<MethodEntry>` (runtime-only, not serialized).
- `MethodEntry { name: Box<str>, type_tag: u8, fn_idx: u16 }`

**EmitState.class_method_names**: `HashSet<String>` populated from ClassDef/GivenDef members. Used in UFCS routing.

**UFCS dispatch priority** (in `emit_ufcs_call`):
1. Field access on record -> load field + CallDynamic
2. Free function in fn_map -> Call(fn_idx)
3. Class method name in class_method_names -> CallMethod

**Critical bug fixed**: `emit_module_fn_bodies(prelude, ...)` was missing from `emit()`, so prelude given methods had `code_length=0` and caused `UnexpectedEof` at runtime.

**New intrinsic**: `FloatToString` (id=3). IntToString is id=2. (Previously int_to_string was id=3.)

Files modified:
- `crates/musi_codegen/src/opcode.rs` -- CallMethod opcode
- `crates/musi_codegen/src/opcode/tests.rs`
- `crates/musi_codegen/src/module.rs` -- MethodEntry, method_table
- `crates/musi_codegen/src/module/tests.rs`
- `crates/musi_codegen/src/emitter.rs` -- ClassDef/GivenDef emission, UFCS routing, emit_module_fn_bodies fix
- `crates/musi_codegen/src/error.rs`
- `crates/musi_codegen/src/intrinsics.rs` -- FloatToString added

### 6. VM (musi_vm) -- 24/24 tests pass (was 21; 3 new tests)

`exec_call_method`: inspects receiver's `type_tag_of()`, looks up `(name, tag)` in `module.method_table`, calls that `fn_idx`. Error if no matching entry.

Files modified:
- `crates/musi_vm/src/vm.rs` -- CallMethod dispatch
- `crates/musi_vm/src/vm/tests.rs` -- 3 new tests
- `crates/musi_vm/src/error.rs` -- new error variant
- `crates/musi_vm/src/native.rs` -- FloatToString dispatch
- `crates/musi_vm/src/value.rs` + `value/tests.rs`

### 7. Standard Library Restructured

New files:
- `std/core/Ordering.ms` -- `choice Ordering { Lt | Eq | Gt }` + helper functions
- `std/core/Pair.ms` -- `record Pair['A, 'B]` + helper functions
- `std/mspackage.json` -- module exports

Updated:
- `std/prelude.ms` -- type classes (Eq, Ord, Add, Sub, Mul, Show) and given instances for Int, Float, String
- `std/string/String.ms` -- fixed `==` to `=`
- `std/num/Float.ms` -- added float_abs/min/max/clamp + short alias `fabs`
- `std/num/Int.ms` -- added short aliases `abs`, `clamp`
- `std/core/Option.ms` -- added short aliases

### 8. Examples Reorganized

`examples/01-hello/` through `examples/09-type-classes/` -- each with `mspackage.json`. All 9 verified working. Original flat files preserved alongside.

`examples/09-type-classes/type_classes.ms` demonstrates `a.max(b)`, `a.min(b)`, `a.eq(a)` via type class dispatch.

### 9. Grammar Updated

`grammar.ebnf` -- added `class`/`given`/`where`/`law`/`satisfies` rules (lines 234-289). Keywords list updated (line 36).

---

## Current State

- **Branch**: `main` -- all changes are unstaged working-tree modifications (nothing committed this session)
- **Last action**: verified all 9 example programs run correctly
- **All tests pass**: musi_lex=55, musi_parse=43, musi_codegen=16, musi_vm=24
- **musi_sema**: NOT tested or compiled this session. Has text-level patches for `where_clause` fields but unverified. Compiling musi_sema is dangerous (80GB swap risk).
- **No build errors** in musi_lex, musi_parse, musi_codegen, musi_vm, musi (CLI)
- **e2e**: `cargo run -- run examples/09-type-classes/type_classes.ms` prints `7`, `3`, `1`, `0`

---

## Remaining Work

### High Priority

1. **musi_sema type class support** -- sema currently ignores ClassDef/GivenDef/where clauses entirely. Needs:
   - Class registry: store class name -> method signatures
   - Given registry: store concrete implementations per type
   - Constraint checking: verify `where 'T satisfies Eq` constraints at call sites
   - WARNING: compiling musi_sema is dangerous. Always use `CARGO_BUILD_JOBS=1 timeout 120s cargo build -p musi_sema`

2. **Object type dispatch** -- `given Eq[Point]` for user-defined record/choice types needs a real `type_id` in `Value::Object`. Currently `type_tag_of(Object) = 255` (unsupported). The method_table lookup will fail for user types.

3. **Default methods in classes** -- `class Ord['T] { fn gt(a,b) => not lt(b,a); }` -- parser accepts default bodies, but codegen ignores them. Need to emit default implementations when a `given` block omits them.

### Medium Priority

4. **musi_sema pattern matching for new AST fields** -- FnDef/Lambda now have `where_clause: Vec<Constraint>`. Text patches were applied to musi_sema but never compiled. May have match-arm exhaustiveness issues.

5. **String/Record as fn parameters** -- passing String or Record values as function parameters does not work correctly in all cases. Mentioned in examples notes.

6. **Optional chaining `?.`** and **nil coalescing `??`** -- planned in MEMORY.md, not started.

7. **`?'T` sugar for `Option['T]`** -- planned, not started.

8. **Keyword-operator conflicts** -- `not`/`and`/`or` are reserved keywords and cannot be used as class method names. No workaround implemented.

### Low Priority

9. **`law` enforcement** -- property testing / quickcheck integration for law declarations.

10. **Parameterized given instances** -- `given Eq[Option['T]] where 'T satisfies Eq` parses but codegen does not handle the type-variable dispatch.

11. **Commit all changes** -- everything is in the working tree, nothing committed.

---

## Important Context and Gotchas

### Workflow (CRITICAL -- read before doing anything)
- **32GB unified memory, ~12GB free**. OOM kills are real.
- **musi_sema compilation can crash the system** -- 80GB swap spike. Always: `CARGO_BUILD_JOBS=1 timeout 120s cargo build -p musi_sema`
- **Test workflow**: `cargo build --tests -p <crate>` then find binary with `ls -t target/debug/deps/<crate>-* | grep -v '\.' | head -1` then `timeout 5s ./<binary>`
- **Never**: `cargo test --workspace` or `cargo test -p musi_sema` directly
- **Max 3 agent threads** at once

### Architecture Decisions
- `CallMethod` opcode uses const-pool string lookup for method name, not a numeric method ID. This is simple but O(n) in method_table size.
- `class_method_names` is a flat `HashSet<String>` -- no per-class scoping. If two classes define a method with the same name, UFCS will route both to CallMethod and the VM will pick based on receiver type tag.
- `method_table` is runtime-only (not serialized to .mso). This means .mso files cannot be loaded and run without re-emitting.
- Intrinsic IDs changed: `Writeln=0, Write=1, IntToString=2, FloatToString=3`. Previous sessions had different numbering.
- `emit_module_fn_bodies` is called three times in `emit()`: once for prelude, once per dep, once for user module. Order matters.

### Things That Were Tried and Worked
- UFCS for primitives (`10.double()`, `3.add(4)`) works via free-function lookup in fn_map.
- UFCS for type class methods (`a.max(b)`) works via CallMethod + method_table.
- Type tag mapping: Int=1, Float=2, Bool=3, String=4, Unit=5, Object=255.

### Comment Policy
- Comments are noise. Only write comments where logic is non-obvious.
- No doc comments repeating what code says.
- Section dividers: `// -- Name ---`

---

## Key Files Reference

| File | Role |
|------|------|
| `grammar.ebnf` | Canonical LL(1) grammar -- source of truth |
| `CLAUDE.md` | AI assistant instructions (LL(1) rules, design constraints) |
| `std/prelude.ms` | Standard prelude with type classes + given instances |
| `crates/musi_ast/src/lib.rs` | All AST node definitions |
| `crates/musi_lex/src/token.rs` | Token kinds including 5 new keywords |
| `crates/musi_parse/src/parser.rs` | LL(1) recursive descent parser |
| `crates/musi_codegen/src/emitter.rs` | Bytecode emitter (ClassDef/GivenDef/UFCS) |
| `crates/musi_codegen/src/opcode.rs` | Instruction set (CallMethod = 0x83) |
| `crates/musi_codegen/src/module.rs` | Module struct with method_table |
| `crates/musi_codegen/src/intrinsics.rs` | Intrinsic enum (4 entries) |
| `crates/musi_vm/src/vm.rs` | Stack-based VM with CallMethod dispatch |
| `crates/musi_vm/src/native.rs` | Native function dispatch |
| `crates/musi_sema/src/check.rs` | Type checker (~1300 lines, dangerous to compile) |
| `crates/musi_sema/src/resolve.rs` | Name resolver (~640 lines) |
| `crates/musi/src/main.rs` | CLI binary |
| `examples/09-type-classes/type_classes.ms` | Working type class demo |
| `.cargo/config.toml` | Memory-saving build settings |

---

## Suggested Next Steps

1. **Commit the working-tree changes.** Everything from this session is uncommitted. Review with `git diff` and create a commit covering the type class implementation.

2. **Tackle musi_sema ClassDef/GivenDef handling.** Start by reading `crates/musi_sema/src/check.rs` to find where `Expr::FnDef` is handled, then add arms for `Expr::ClassDef` and `Expr::GivenDef`. Build with `CARGO_BUILD_JOBS=1 timeout 120s cargo build -p musi_sema`. Do NOT compile alongside other crates.

3. **Implement default method emission** in `crates/musi_codegen/src/emitter.rs`. When a `given` block omits a method that the `class` defined with a default body, emit the default body as a fallback implementation.
