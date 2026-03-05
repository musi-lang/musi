# Phase 5 — Prelude + Hello World End-to-End

**Crate:** `musi_codegen`, `musi_vm`, `musi`
**Goal:** Wire source → execution. Parse a real `.ms` file, emit bytecode, run in VM.
**Dependencies:** Phase 3 (parser+AST), Phase 4 (bytecode+VM)

---

## Deliverables

### Prelude

A real `.ms` file declaring core builtins:

```
// std/prelude.ms
#[intrinsic(writeln)] native fn writeln(s: String): Unit;
#[intrinsic(write)]   native fn write(s: String): Unit;
```

- Compiler implicitly loads the prelude before the user's file.
- User can read it — not magic.
- Parser handles `native fn` (no body, terminated by `;`).
- Codegen reads `#[intrinsic(...)]` attr to assign intrinsic IDs.

### Codegen (Thin Slice)

Walks the AST and emits bytecode for a minimal subset:

**Supported constructs:**
- Top-level statements (sequential)
- String literals → const pool entries
- Integer literals → `ld.imm.i64`
- Function call expressions → `call` opcode
- `native fn` declarations → symbol table entries with native flag
- `#[intrinsic(name)]` → maps to intrinsic ID via a name→id table

**Codegen algorithm (pseudo-code):**
```
fn codegen(program: &[Expr]):
  // Pass 1: collect function declarations, assign indices
  for expr in program:
    if expr is FnDef or (native fn):
      register in function table

  // Pass 2: emit code for main body
  main_code = emit_block(non-declaration top-level exprs)
  main_code.push(halt)

  // Emit the module
  build .mso with const_pool, symbol_table, function_table, code
```

**Intrinsic ID mapping:**
```
intrinsic_name_to_id = {
  "writeln" → 1,
  "write"   → 2,
}
```

Read from `#[intrinsic(name)]` attribute on `native fn`.

### CLI

Binary crate `musi` with minimal subcommand:

```
musi run <file.ms>
```

Pipeline:
1. Read source file
2. Lex → token stream
3. Parse → AST
4. If parse errors: print diagnostics to stderr, exit 1
5. Codegen → .mso Module (in memory)
6. VM execute
7. Exit 0 on success

### Integration Test

```
# examples/hello.ms
writeln("Hello, world!");
```

```
$ cargo run -- run examples/hello.ms
Hello, world!
```

**Key:** Codegen does NOT type-check. It emits optimistically. Types come in Phase 7.

---

## Milestone

1. `musi run examples/hello.ms` prints "Hello, world!" with exit 0.
2. Syntax error in input → diagnostic on stderr with exit 1.
3. `cargo test --workspace` passes.
