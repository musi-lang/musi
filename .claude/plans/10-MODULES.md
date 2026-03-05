# Phase 10 — Modules

**Crate:** `musi_sema`, `musi_codegen`
**Goal:** `import`/`export`, separate compilation units, .mso linking.
**Dependencies:** Phase 8 (records, choices, match, functions)

---

## Deliverables

### Module System Design

**Source-level syntax:**
```
// Exporting
export fn add(a: Int, b: Int): Int = a + b;
export record Point { x: Int, y: Int };
export choice Color { Red | Green | Blue };
export const PI := 3;

// Importing
import { add, Point } from "math";
import { Color } from "colors";
```

- `export` modifier makes a declaration visible to other modules.
- `import` brings names into scope from another module.
- Module path resolution: `"math"` → `./math.ms` (relative to importing file).

### Name Resolution (Extended)

**Module-level symbol tables:**
```
ModuleScope = {
  id: ModuleId,
  path: String,
  exports: HashMap<Symbol, DefId>,    // externally visible
  locals: HashMap<Symbol, DefId>,      // all bindings (including non-exported)
}
```

**Import resolution algorithm:**
```
fn resolve_import(importing_module, import_stmt):
  target_path = resolve_path(importing_module.path, import_stmt.path)
  target_module = compile_module(target_path)  // recursive, with cycle detection

  for item in import_stmt.items:
    if item.name not in target_module.exports:
      error: "name not exported from module"
    def_id = target_module.exports[item.name]
    alias = item.alias or item.name
    importing_module.locals.insert(alias, def_id)
```

**Cycle detection:**
- Maintain a "currently compiling" set.
- If a module is encountered while already in the set → error: cyclic import.

### Compilation Pipeline (Extended)

```
fn compile_program(entry_path):
  modules = {}          // path → compiled module
  order = []            // topological compilation order

  fn compile_module(path):
    if path in modules: return modules[path]
    if path in currently_compiling: error("cyclic import")

    currently_compiling.add(path)
    source = read_file(path)
    ast = parse(source)

    // Recursively compile dependencies first
    for import_stmt in ast.imports:
      dep_path = resolve_path(path, import_stmt.path)
      compile_module(dep_path)

    // Name resolution + type checking
    resolve_names(ast, modules)
    type_check(ast)

    modules[path] = compiled_module
    order.push(path)
    currently_compiling.remove(path)

  compile_module(entry_path)
  return link(order, modules)
```

### .mso Linking

**Per-module .mso compilation:**
- Each module produces its own const pool, symbol table, function table, code.
- Exported symbols have the `export` flag in symbol table.

**Linking (in-memory):**
```
fn link(modules: [Module]) → LinkedModule:
  // 1. Merge const pools (dedup identical entries)
  // 2. Merge symbol tables, reindex
  // 3. Merge function tables, reindex
  // 4. Merge code sections, fix up indices
  // 5. Resolve inter-module references:
  //    import ref → find matching export → rewrite to direct index
```

**Cross-module calls:**
- Import of a function → resolves to the function's index in the linked module.
- Import of a type → resolves to the type's ID in the linked type table.

### Prelude as a Proper Module

```
// std/prelude.ms
export #[intrinsic(writeln)] native fn writeln(s: String): Unit;
export #[intrinsic(write)] native fn write(s: String): Unit;
export #[intrinsic(int_to_string)] native fn int_to_string(n: Int): String;
```

- Implicitly imported by every module (unless explicitly overridden).
- Can be explicitly imported: `import { writeln } from "std/prelude";`

### Acyclic Dependency Validation

- Imports form a DAG.
- Detected at compile time during recursive compilation.
- Error message: "cyclic import detected: A → B → C → A".

---

## Milestone

1. Two-file program: `main.ms` imports `math.ms`, calls exported function, runs correctly.
2. Cyclic import → clear error message.
3. Import non-exported name → error.
4. Prelude functions available without explicit import.
5. `cargo test --workspace` passes.

### Milestone test program:

```
// math.ms
export fn square(x: Int): Int = x * x;
export const MAGIC := 42;

// main.ms
import { square, MAGIC } from "math";
writeln(int_to_string(square(MAGIC)));  // 1764
```
