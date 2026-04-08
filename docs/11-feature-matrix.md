# Feature Matrix (`crates_new/`)

This document tracks language and toolchain coverage across the canonical compiler rewrite in `crates_new/`.

The canonical feature definitions live in:

- `grammar/Musi.abnf`
- `grammar/Musi.g4`
- `docs/00-syntax.md`
- `docs/01-type-system.md`
- `docs/02-effect-system.md`
- `docs/03-metaprogramming.md`
- `docs/04-compiler-attributes.md`
- `docs/05-ffi.md`

Primary implementation truth comes from:

- `Cargo.toml`
- `docs/09-architecture.md`
- `docs/12-public-api.md`
- `docs/13-crates_new-rewrite-tracker.md`
- current tests in `music_syntax`, `music_resolve`, `music_sema`, `music_emit`, `music_session`, and `musi_project`

Status legend:

- `done`: implemented in the canonical `crates_new` workspace and backed by current tests
- `partial`: implemented, but intentionally limited, incomplete across phases, or not fully lowered/executable yet
- `missing`: not implemented in the canonical `crates_new` stack

This matrix is language-first. It does not claim runtime or JIT completion.

Notes are descriptive: they reflect current `crates_new` behavior (accepted vs diagnosed), not language decisions.

## Surface Syntax And Literals

| Feature                                  | Lex  | Parse/AST | Resolve/HIR | Sema | Notes                                                             |
| ---------------------------------------- | ---- | --------- | ----------- | ---- | ----------------------------------------------------------------- |
| Identifiers (plain/escaped)              | done | done      | done        | done |                                                                   |
| Symbolic operators (`SymOp`)             | done | done      | done        | done | Symbolic infix surface resolves and typechecks like callable use  |
| Integer literals (bases, `_`)            | done | done      | done        | done | Type is `Int`; numeric range modeling is not implemented in sema  |
| Float literals (incl. exponent)          | done | done      | done        | done | Type is `Float`; no range/NaN-policy modeling in sema             |
| Strings                                  | done | done      | done        | done | Escape decoding is covered in `music_syntax::string_lit`          |
| Template strings with interpolation      | done | done      | done        | done | Interpolations are ordinary expressions and typecheck as `String` |
| Runes                                    | done | done      | done        | done | Typed as `Int` codepoints                                         |
| Comments and trivia                      | done | done      | done        | n/a  | Trivia is preserved in the syntax tree                            |
| Quote and splice surface                 | done | done      | done        | done | `Syntax`-typed metaprogramming surface exists in frontend + sema  |

## Core Expressions And Modules

| Feature                                           | Parse/AST | Resolve/HIR | Sema | Backend/Toolchain | Notes                                                                |
| ------------------------------------------------- | --------- | ----------- | ---- | ----------------- | -------------------------------------------------------------------- |
| Sequences (`;`)                                   | done      | done        | done | done              | Top-level statements lower as sequence expressions                   |
| `let` bindings                                    | done      | done        | done | done              | Destructuring patterns (`(x, y)`, `[x, y]`, `{x, y}`) compile end-to-end; generic `let` uses explicit type params, not implicit HM-style generalization |
| Mutable bindings (`let mut`)                      | done      | done        | done | done              | Mutable local value binds compile end-to-end in the non-runtime backend |
| Assignment (`<-`)                                 | done      | done        | done | done              | Local names, globals, and indexed sequence elements compile end-to-end |
| Calls                                             | done      | done        | done | done              | Direct named, imported, generic, foreign, and higher-order closure calls compile end-to-end in the non-runtime backend |
| Field/index/update access (`.`, `.[`, `.{`)       | done      | done        | done | done              | Imported module members, indexed sequence get/set, record field projection, and record update now compile end-to-end; SEAM text transport supports quoted symbolic names for structural record types |
| Variant constructors (`.Tag(...)`)                | done      | done        | done | done              | Constructor type is inferred from unique matching `data` variant tag in scope; ambiguous tags require disambiguation via annotation |
| `case ... of` with guards                         | done      | done        | done | done              | Literal, wildcard, bind, tuple, array, and structural variant/or/as patterns with guards compile end-to-end through SEAM (`data.tag`, `br.tbl`, `data.get`) |
| `data`, `effect`, `class`, `instance` declarations | done     | done        | done | done            | Declaration forms are supported at module scope; using them as value expressions is a semantic error (not a runtime feature) |
| `perform`, `handle`, `resume`                     | done      | done        | done | done              | Handler clauses bind params as written (`op(args, k) => ...`), `value => ...` has an implicit `value` binder, and `k` is typed as `op_result ~> handled_result`; lowering emits handler objects plus `hdl.push/hdl.pop`, `eff.invk`, and `eff.resume` |
| Static imports (`import "..."`)                   | done      | done        | done | done              | Static import discovery, module keys, and session/project integration exist |
| Dynamic imports (`import expr`)                   | done      | done        | partial | partial         | Expression form exists; static graph participation is intentionally limited |
| Export and opaque module surface                  | done      | done        | done | done            | Export collection, opaque marking, and SEAM metadata emission exist; runtime hiding implications remain out of scope |
| Imported module member typing                     | n/a       | done        | done | done              | Imported globals and generic callables compile through semantic module surfaces |
| Destructured module imports and aliases           | done      | done        | done | done              | Destructured imported value aliases and imported class/effect alias hydration are covered end-to-end for non-runtime compilation |
| Class and effect laws                             | done      | done        | done | done         | Law surface is parsed, typechecked, and emitted as metadata; no runtime/property-check execution |

## Types, Constraints, And Effects

| Feature                              | Parse/AST | Resolve/HIR | Sema | Backend/Toolchain | Notes                                                                |
| ------------------------------------ | --------- | ----------- | ---- | ----------------- | -------------------------------------------------------------------- |
| Named types and application (`T[A]`) | done      | done        | done | done              | Explicit generic type application works and is emitted in exported signature metadata |
| Functions (`->`, `~>`)               | done      | done        | done | done              | Function kinds exist; exported signature types and effect rows are emitted as metadata |
| Tuples and products                  | done      | done        | done | done              | Tuple types and tuple expressions compile end-to-end in the non-runtime backend |
| Anonymous sums (`+`)                 | done      | done        | done | done              | Sum types are represented semantically and emitted in exported signature metadata |
| Arrays (`[]T`, `[n]T`)               | done      | done        | done | done              | Array literals and fixed-dimension annotations typecheck |
| Array spread (`...expr` in `[...]`)  | done      | done        | missing | missing         | Spread surface is parsed and resolved but currently diagnosed as not implemented in sema/IR/emit |
| `mut T`                              | done      | done        | done | done              | Writable types are enforced for write-through assignment (`base.[i] <-`, `base.field <-`) |
| `where` constraints (`T :`, `T <:`)  | done      | done        | done | done              | Constraint lowering and solving exist; constraints are emitted in exported signature metadata |
| Open effect rows (`with { ... }`)    | done      | done        | done | done              | Open rows and declared-effect checks exist; effect rows are emitted in exported signature metadata |
| Imported generic exports             | n/a       | done        | done | done              | Imported generic callable uses now compile through `music_session` end-to-end |
| Instance coherence across imports    | n/a       | done        | done | n/a              | Reachable exported instances participate in sema coherence           |

## Attributes, FFI, And Metaprogramming

| Feature                            | Parse/AST | Resolve/HIR | Sema | Backend/Toolchain | Notes                                                                  |
| ---------------------------------- | --------- | ----------- | ---- | ----------------- | ---------------------------------------------------------------------- |
| Attribute syntax and data-only args | done     | done        | done | n/a               | `@link/@when/@repr/@layout/@diag.*` argument-model validation exists   |
| `foreign` declaration surface      | done      | done        | done | done              | Foreign declarations and direct foreign calls lower into IR and SEAM metadata end-to-end |
| `export foreign` surface           | done      | done        | done | done              | `export foreign "abi" ( ... )` is carried into SEAM artifact foreign descriptors and text/binary transports |
| `@link` validation                 | done      | done        | done | done              | Invalid targets are diagnosed; foreign descriptors carry `link` + `symbol` metadata (runtime linking is out-of-scope) |
| `@when` target gating              | done      | done        | done | done              | Gated foreign declarations are excluded from IR lowering and SEAM emission for the active target |
| `@repr` and `@layout` surface      | done      | done        | done | done              | Layout-sensitive metadata is carried into SEAM artifacts (`.data` descriptors); runtime ABI execution remains out of scope |
| Compiler-only `@musi.*` attrs      | done      | done        | done | done         | Reserved surface exists and is emitted as metadata; consumers are toolchain-defined |
| Inert metadata attrs               | done      | done        | done | done             | Preserved and emitted as metadata; downstream consumers are optional  |
| Quote as first-class syntax        | done      | done        | done | partial           | Frontend and sema support exist; backend/runtime execution path is not complete |
| Splice forms `#name/#()/#[]`       | done      | done        | done | partial           | Valid inside quote; backend/runtime execution path is not complete     |

## SEAM, Session, And Project Integration

| Feature                                        | Status | Notes                                                                       |
| ---------------------------------------------- | ------ | --------------------------------------------------------------------------- |
| SEAM contract (`music_bc`)                     | done   | Artifact tables, descriptors, opcode families, and structural validation exist; `data.new` uses `type_len`, `data.get/data.set` exist, and `hdl.push` takes an effect id (handler object is a stack value) |
| SEAM text transport (`music_assembly`)         | done   | Text format, parser, formatter, and validation exist                        |
| SEAM binary transport (`music_assembly`)       | done   | Binary encode/decode and validation exist                                   |
| Sema-to-IR lowering (`music_ir`)               | done   | Codegen-facing facts and owned executable IR exist                          |
| IR-to-SEAM emission (`music_emit`)             | partial | Current emission includes records, variants, and effects (`data.*`, `br.tbl`, `hdl.*`, `eff.*`); remaining gaps include full sums, richer data-field op surface, and runtime execution |
| Module compilation through `music_session`     | done   | Artifact, bytes, and text outputs exist                                     |
| Reachable entry-graph compilation              | done   | `music_session` compiles the static-import closure                          |
| Parse/resolve/sema/IR/emit session caching     | done   | Cached phase products and edit invalidation exist                           |
| Package/workspace loading (`musi_project`)     | done   | `musi.json`, workspaces, lockfiles, registry cache, and package-aware compile exist |
| Package import remapping and registry resolution | done | `musi_project` builds the session view used for package-aware compilation   |

## Planned Or Missing

| Feature                  | Status  | Notes                                                      |
| ------------------------ | ------- | ---------------------------------------------------------- |
| Native/JIT backend       | missing | `music_jit` is planned but not implemented                 |
| Runtime execution/VM     | missing | Outside the current non-runtime `crates_new` completion bar |
| Full-language backend parity | partial | The compiler path exists, but emission is still incomplete overall |
