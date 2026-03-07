# Changelog

All notable changes to Musi are documented here.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).
Musi uses [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

## [0.1.0-alpha1] — unreleased

First public alpha. The language, toolchain, and standard library are functional
but under active development. Breaking changes may occur before 0.1.0.

### Language

- **Algebraic types** — `record` (product) and `choice` (sum) with LL(1) grammar
- **Generics** — type parameters in `[...]` on records, choices, and functions
- **Pattern matching** — `match`/`case`, or-patterns, destructuring, guard expressions
- **Optional chaining** — `expr?.field`, `expr ?? default` (`??` nil-coalesce)
- **Inline case conditions** — `if case .Some(v) := expr then ...` / `while case`
- **Type classes** — `class`, `given`, `satisfies`; operator overloading via `Add`, `Eq`, etc.
- **Lambdas** — `(params) => body`; closures capture by reference
- **Mutable bindings** — `var x := 0; x <- x + 1` (`<-` assign, `:=` bind)
- **Arrays** — `[1, 2, 3]`, `arr.[i]`, `arr.[i] <- v`, spread `[<..arr, extra]`
- **Tuples** — `(a, b)`, field access `t.0`, `t.1`
- **String interop** — UTF-8 strings, `string_concat`, slice, split, trim, join, etc.
- **FFI** — `extrin fn` with `#[link(...)]` for dlopen/dlsym bindings
- **`defer`** — deferred expression execution
- **`using`** — resource acquisition (semantics TBD)
- **`break value`** — break with a value from a loop
- **`cycle [guard]`** — continue with optional guard expression
- **`#[test("name")]`** — inline unit tests wired into `musi test`

### Toolchain

- `musi run <file>` — compile and execute
- `musi check <file>` — type-check via `musi_sema`
- `musi test <file>` — run `#[test]`-annotated functions
- `musi build <file>` — emit `.mso` bytecode binary
- `musi init` — scaffold a new package (cargo-new style)
- Rich diagnostic output with source context and ANSI colour
- Namespace imports: `import * as Name from "path"`
- Multi-file compilation with BFS dependency resolution

### Standard Library

- `std/prelude` — auto-imported: `Bool`, `Option`, `Expect`, `Map['K,'V]`, array/string/map intrinsics, type classes
- `std/math` — `gcd`, `lcm`, `pow_int`, `clamp`, trig (`sin`, `cos`, `tan`, `atan2`), `log`, `exp`, `pi`, `tau`, `degrees`, `radians`
- `std/string` — `string_is_empty`, `string_pad_start/end`, `string_join`, `string_lines`, `string_words`, `string_replace`
- `std/core/{bool,option,ordering,pair,result}` — standard algebraic types
- `std/collections/array` — `array_map`, `filter`, `fold`, `find`, `sort_by`, `flatten`, `zip`, `dedup`, and more
- `std/collections/map` — `map_get_or`, `map_update`, `map_from_pairs`, `map_to_pairs`, `map_count_by`, `map_merge`
- `std/collections/set` — set operations backed by `Map[String, String]`
- `std/encoding/csv` — `csv_parse`, `csv_serialize`

### VM

- Stack-based MSIL bytecode interpreter
- `Value::Map` — hash map with `HashKey` (Int or String keys)
- Float arithmetic/comparison dispatched at runtime from integer opcodes
- FFI via `libloading` (dlopen/dlsym); macOS and Linux

### Known Limitations

- `musi_sema` type checker is partially complete; `musi check` may miss some errors
- Named functions inside blocks (`Expr::FnDef` in non-module scope) are not yet supported; use lambdas
- `for case` pattern is not yet implemented in codegen
- Map key type is constrained to `Int | String` at runtime (not enforced by the type system yet)
- No GC; heap values use reference counting (`Rc`); cycles will leak
- No LSP; VS Code extension provides syntax highlighting only
