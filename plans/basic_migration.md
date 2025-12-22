# Implementation Plan - Basic Module Migration

We are migrating the `basic` module from OCaml (`lib/basic`) to Rust (`src/basic`).
The goal is to provide equivalent functionality with concise, idiomatic Rust code, adhering to the project's lints.

## 1. Span (`src/basic/span.rs`)

- Port `Span.t` to `struct Span`.
- Fields: `file` (u32), `start` (u32), `end` (u32).
- Methods: `new`, `merge`, accessor methods.
- Implement `Copy`, `Clone`, `Debug`, `PartialEq`, `Eq`.

## 2. Interner (`src/basic/interner.rs`)

- Port `Interner.t` to `struct Interner`.
- Use `std::collections::HashMap<String, u32>` for the table.
- Use `Vec<String>` for storage.
- Methods: `new`, `intern`, `lookup` (equivalent to `lookup_opt`).

## 3. Source (`src/basic/source.rs`)

- Port `Source.t` to `struct SourceFile` (renaming from `Source` to avoid confusion or just `Source`).
- Fields: `filename` (String), `lines` (Vec<String>).
- Methods: `new`, `line_col`, `line_text`, `path`.
- Note: `Source.line_col` in OCaml iterates to find lines. In Rust, we might pre-compute line starts for O(1) lookups if "compressed" allows, or stick to OCaml's logic. OCaml code splits by newline on creation, so it stores lines. I will stick to storing lines.

## 4. Errors (`src/basic/errors.rs`)

- Port types `level`, `numeric_error`, `lexical_error`, `t`, `info`.
- Use Rust `enum` for variants.
- Implement `impl fmt::Display` or a method `info()` to return the metadata message/hint/level.

## 5. Reporter (`src/basic/reporter.rs`)

- Port `Reporter` logic.
- Struct `Diagnostic` (equivalent to `t`), `Bag`.
- Methods to emit diagnostics to Stderr.
- Will use minimal ANSI escape codes for colors if detected.

## 6. Module exposure (`src/basic/mod.rs`)

- `pub mod span;`
- `pub mod interner;`
- `pub mod source;`
- `pub mod errors;`
- `pub mod reporter;`

## 7. Verification

- Validate the code compiles via `cargo check`.
