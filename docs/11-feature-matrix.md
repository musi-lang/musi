# Feature Matrix (crates_new)

This document tracks reduced-core language feature coverage across the clean-room crates in `crates_new/`.

The canonical feature definitions live in:

- `grammar.abnf`
- `docs/00-syntax.md`
- `docs/01-type-system.md`
- `docs/02-effect-system.md`
- `docs/03-metaprogramming.md`
- `docs/04-compiler-attributes.md`
- `docs/05-ffi.md`

Status legend:

- `done`: implemented and covered by tests/fixtures
- `partial`: implemented but missing edge cases or cross-layer integration
- `missing`: not implemented in `crates_new/`

## Surface Syntax

| Feature                                  | Lex  | Parse/AST | Resolve/HIR | Sema    | Notes                                                                 |
| ---------------------------------------- | ---- | --------- | ----------- | ------- | --------------------------------------------------------------------- |
| Identifiers (plain/escaped)              | done | done      | done        | done    |                                                                       |
| Symbolic operators (`SymOp`)             | done | done      | done        | partial | Sema treats most ops as unknown callable surface                      |
| Integer literals (bases, `_`)            | done | done      | done        | partial | Type is `Int`; numeric-range validation is not modeled yet            |
| Float literals (incl. exponent)          | done | done      | done        | partial | Type is `Float`; no numeric-range validation yet                      |
| Strings                                  | done | done      | done        | done    | `music_basic::string_lit` supports decoding                           |
| F-strings (syntax + interpolation exprs) | done | done      | done        | done    | Interpolations parsed into syntax subtree and typechecked as `String` |
| Runes                                    | done | done      | done        | partial | Represented as `Int` today                                            |
| Comments + trivia                        | done | done      | done        | n/a     | Trivia preserved in tokens/syntax tree                                |

## Expressions

| Feature                                        | Lex  | Parse/AST | Resolve/HIR | Sema    | Notes                                                         |
| ---------------------------------------------- | ---- | --------- | ----------- | ------- | ------------------------------------------------------------- |
| Sequences (`;`)                                | done | done      | done        | done    | Statement wrappers lowered as a top-level sequence            |
| `let` bindings                                 | done | done      | done        | partial | Polymorphism and full generalization not complete             |
| Mut bindings (`let mut`)                       | done | done      | done        | partial | Mutation model is not fully enforced                          |
| Assignment (`<-`)                              | done | done      | done        | partial | Treated as expression; effect model is still evolving         |
| Calls                                          | done | done      | done        | partial | Callable inference is present, dispatch not fully modeled     |
| Field/index/update access (`.`, `.[`, `.{`)    | done | done      | done        | partial | Optional/forced chain validated via lang item `Option`        |
| `case ... of` with guards                      | done | done      | done        | partial | Pattern/type interactions incomplete                          |
| `data`, `effect`, `class`, `instance` as exprs | done | done      | done        | partial | Registered into type environment with reduced rules           |
| `perform`, `handle`, `resume`                  | done | done      | done        | partial | Effect-row computation exists; full handler typing incomplete |
| `quote` and splice forms                       | done | done      | done        | partial | Quote/splice are typed as `Any` today                         |

## Types

| Feature                             | Parse/AST | Resolve/HIR | Sema    | Notes                                                       |
| ----------------------------------- | --------- | ----------- | ------- | ----------------------------------------------------------- |
| Named types + application (`T[A]`)  | done      | done        | partial | Generic instantiation exists but is limited                 |
| Functions (`->`, `~>`)              | done      | done        | partial | Effect rows live on signatures (`with {}`)                  |
| Products (`*`, tuples)              | done      | done        | partial | Sema models tuples; product types are still evolving        |
| Sums (`+`)                          | done      | done        | partial | Sema represents sum types but does not fully lower/dispatch |
| Arrays (`[]T`, `[n]T`)              | done      | done        | partial | Shape/dimension checking not complete                       |
| `mut T`                             | done      | done        | partial | Treated as prefix type op; enforcement incomplete           |
| `where` constraints (`T <:`, `T :`) | done      | done        | partial | Stored and partially checked                                |
| Effect rows (`with { ... }`)        | done      | done        | partial | Row openness modeled as `EffectRow::is_open`                |

## Attributes + FFI

| Feature                     | Parse/AST | Resolve/HIR | Sema    | Notes                                                             |
| --------------------------- | --------- | ----------- | ------- | ----------------------------------------------------------------- |
| Attribute syntax + args     | done      | done        | partial | Argument-model validation for compiler-known attrs is in progress |
| `foreign` decl surface      | done      | done        | done    | `foreign (...)` block attrs copied to inner decls                 |
| `export` / `opaque` surface | done      | done        | partial | Export collection exists; representation hiding not implemented   |

## SEAM Boundary

| Feature                     | IL (`music_il`) | Assembly (`music_assembly`) | Notes                          |
| --------------------------- | --------------- | --------------------------- | ------------------------------ |
| SEAM descriptor + ISA model | done            | n/a                         | Contract defined in `music_il` |
| Text encode/decode          | n/a             | done                        |                                |
| Binary encode/decode        | n/a             | done                        |                                |

## Missing Clean-Room Crates (Locked In 09)

- `music_codegen`: missing
- `music_fe`: missing
- `music_session`: missing

