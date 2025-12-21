---
trigger: always_on
---

# PROJECT STACK CONTEXT

## 1. OCaml Environment

- **Language Version**: OCaml 5.4.0
- **Build System**: Dune 2.30.2
  - **MANDATORY**: Use `dune` commands for all build/test/run tasks (e.g., `dune build`, `dune runtest`, `dune exec`).
  - **FORBIDDEN**: Do not generate `Makefiles` or call `ocamlc`/`ocamlopt` directly.
- **Package Manager**: Opam
  - Dependencies are managed via `dune` files and `dune-project`.

## 2. JavaScript/TypeScript Environment

- **Runtime & Tooling**: Bun
- **Strict Constraint**:
  - **ALWAYS** use `bun install` or `bun i`, `bun test`, `bun run`.
  - **FORBIDDEN**: `npm`, `yarn`, `pnpm`, or `node`.
  - If you see a `package.json`, assume it is for **Bun**.

## 3. Testing Protocols

- **OCaml**: Use `Alcotest` (via `dune runtest`) unless existing files show otherwise.
- **JS/TS**: Use `bun:test`.

## 4. Paradigm Conflicts

- **OCaml Files (`.ml`, `.mli`)**: Functional programming first. Immutable by default.
- **Scripting**: If writing quick scripts, verify if I want a `dune exec` script or a `bun` script before generating code.

## 5. Musi Language Development

- **Project Type**: Language Compiler (OCaml/Dune).
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
