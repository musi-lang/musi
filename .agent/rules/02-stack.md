---
trigger: always_on
---

# PROJECT STACK CONTEXT

## 1. Rust Environment

- **Build System**: Cargo
  - **MANDATORY**: Use `cargo` commands for all build/test/run tasks (e.g., `cargo build`, `cargo test`, `cargo run`).
- **Function Ordering (Public-First)**:
  - In `impl` blocks, maintain a "Public First" reading order.
  - **Order**:
    1. Constructors (`new`, `Default`, `From`).
    2. Main Public Entry Points (the core API of the struct).
    3. High-level orchestrators/dispatchers.
    4. Specialized implementation methods (e.g., specific scanners).
    5. Internal private helpers and state-modification logic.
    6. Low-level navigation (`peek`, `advance`).
    7. Character predicates or small unit-helpers at the very bottom.

- **Borrow Checker & Closures**:
  - **Closure Pitfall**: Avoid using functional methods like `map_or_else` or `and_then` when the closures need to capture `&mut self` if `self` is already partially borrowed.
  - **Preference**: Use `match` or `if let` blocks. They provide clearer lifetime boundaries and are more robust against "cannot borrow as mutable" errors in complex structs like Lexers or Parsers.

## 2. JavaScript/TypeScript Environment

- **Runtime & Tooling**: Bun
- **Strict Constraint**:
  - **ALWAYS** use `bun install` or `bun i`, `bun test`, `bun run`.
  - **FORBIDDEN**: `npm`, `yarn`, `pnpm`, or `node`.
  - If you see a `package.json`, assume it is for **Bun**.

## 3. Testing Protocols

- **JS/TS**: Use `bun:test`.

## 4. Paradigm Conflicts

- **Scripting**: If writing quick scripts, verify if I want a `cargo run --bin script` or a `bun` script before generating code.

## 5. Musi Language Development

- **Project Type**: Language Compiler (Rust/Cargo).
- **No Parser Generators**: The lexer and parser are hand-written for maximum control and performance. Do NOT suggest using `pest`, `lalrpop`, or similar unless explicitly asked.
- **Core Definitions (`grammar.ebnf`)**:
  - This file is **HUMAN-AUTHORED** and is the absolute Source of Truth.
  - **Ambiguity Protocol**: If you encounter a design decision in the grammar that looks "questionable," "inefficient," or ambiguous (e.g., "should this be X or Y?"):
    1. **STOP**. Do not assume it is a typo.
    2. **DO NOT** "auto-correct" or optimize the grammar file silently.
    3. **ASK**: "I noticed [pattern] in the grammar. Is this intentional behavior, or a bug that needs fixing?"
- **Extension `.ms`**:
  - These are source files for the Musi language.
  - **DO NOT** try to run them with Python/Node.
  - **DO NOT** "fix" syntax in `.ms` files to look like TypeScript/Python.
  - If a `.ms` file fails parsing, check `grammar.ebnf` first.
