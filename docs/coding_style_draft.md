# Musi Coding Style Guide (Draft)

This document defines the recommended coding style for Musi. The goal is to encourage consistency, readability, and a "community standard" that feels modern and approachable.

**Philosophy**:

- **Readability first**: Code is read more often than written.
- **Consistency**: Defaults are better than arguments.
- **Modernity**: Inherit best practices from ecosystems like Rust, TypeScript (Deno/Biome), and Go.

---

## Formatting

### Indentation

- **Style**: Spaces.
- **Width**: 2 spaces. (**2 = DEFAULT**)
- **Tabs**: No.
- *Rationale*: 2 spaces is standard in modern JS/TS/OCaml/Reason ecosystems and reduces horizontal scroll in deeply nested code (e.g., match expressions).

### Line Length

- **Soft Limit**: 80 characters.
- **Hard Limit**: 100 or 120 characters. (**100 = DEFAULT**)
- *Rationale*: Keeps side-by-side diffs readable.

### Braces

- **Style**: Same line (Egyptian brackets/K&R).
- **Control Flow**:

    ```musi
    if condition {
      // body
    } else {
      // body
    };
    ```

- *Rationale*: Consistency with Rust, Go, TypeScript.

### Semicolons (Language Rule)

- **Rule**: Semicolons are **required** for statements (expressions wrapped in `StmtExpr`). They are **not used** for expressions (e.g., the final expression in a block).
- **Example**:

    ```musi
    import "std/io";    // Gets 'writeln(...)' from here

    val x := 10;        // StmtExpr -> ';' required
    if x > 5 {
      writeln("big");
    } else {
      writeln("small");  // StmtExpr inside block -> ';' required
    };                   // StmtExpr (if-expr as stat) -> ';' required

    // Final expr in block generally has no ';' if returned
    fn get_value(): Int { x };  // Expr (final return) -> No ';' (you'd need 'return' keyword if you use ';')
    ```

- *Note*: This is a strict syntax rule, not a style choice.

### Trailing Commas

- **Preference**: Multi-line lists/records should have trailing commas.
- *Rationale*: Cleaner diffs when adding items.

---

## Naming Conventions

### General

- **Variables**: `snake_case` (e.g., `user_id`, `is_valid`).
- **Functions**: `snake_case`.
- **Types/Classes**: `PascalCase` (e.g., `HttpRequest`, `Result`).
- **Constants**: `SCREAMING_SNAKE_CASE` or `snake_case` (depending on usage).

### Enums/Sum Types

- **Variants**: `PascalCase`.

    ```musi
    sum Status {
      case Ok,
      case Error(String)
    };
    ```

---

## Best Practices (Linting Rules)

### 1. No Unused Variables

Variables that are declared but not used should be removed or prefixed with `_`.

### 2. Explicit Returns

(To be decided: Implicit return of last expression vs `return` keyword).

- *Current*: `x` supported. `parser.ml` shows `ExprBlock` has an optional final expression.
- *Recommendation*: Prefer implicit return for short lambdas/blocks.

### 3. Imports

- Group imports by module.
- Use explicit names over wildcards where possible.

---

## Configuration (`msconfig.json`)

The formatter should respect `msconfig.json`.
Suggested defaults:

```json
{
  "fmt": {
    "useTabs": false,
    "indentWidth": 2,
    "lineWidth": 80,
  }
}
```
