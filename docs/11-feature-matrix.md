# Feature Matrix (`crates_new/`)

This document tracks reduced-core language and toolchain coverage across the canonical compiler rewrite in `crates_new/`.

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
- `partial`: implemented, but intentionally reduced, incomplete across phases, or not fully lowered/executable yet
- `missing`: not implemented in the canonical `crates_new` stack

This matrix is language-first. It does not claim runtime or JIT completion.

## Surface Syntax And Literals

| Feature                                  | Lex  | Parse/AST | Resolve/HIR | Sema | Notes                                                             |
| ---------------------------------------- | ---- | --------- | ----------- | ---- | ----------------------------------------------------------------- |
| Identifiers (plain/escaped)              | done | done      | done        | done |                                                                   |
| Symbolic operators (`SymOp`)             | done | done      | done        | done | Symbolic infix surface resolves and typechecks like callable use  |
| Integer literals (bases, `_`)            | done | done      | done        | done | Type is `Int`; numeric range modeling is still reduced-core       |
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
| `let` bindings                                    | done      | done        | partial | partial         | Top-level lets and local bind/wildcard value lets compile; implicit HM-style generalization and destructuring lets remain reduced |
| Mutable bindings (`let mut`)                      | done      | done        | done | done              | Mutable local value binds compile end-to-end in the non-runtime backend |
| Assignment (`<-`)                                 | done      | done        | done | done              | Local names, globals, and indexed sequence elements compile end-to-end |
| Calls                                             | done      | done        | done | done              | Direct named, imported, generic, foreign, and higher-order closure calls compile end-to-end in the non-runtime backend |
| Field/index/update access (`.`, `.[`, `.{`)       | done      | done        | done | done              | Imported module members, indexed sequence get/set, record field projection, and record update now compile end-to-end; SEAM text transport supports quoted symbolic names for structural record types |
| Variant constructors (`.Tag(...)`)                | done      | done        | done | done              | Constructor type is inferred from unique matching `data` variant tag in scope; ambiguous tags require disambiguation via annotation |
| `case ... of` with guards                         | done      | done        | done | done              | Literal, wildcard, bind, tuple, array, and structural variant/or/as patterns with guards compile end-to-end through SEAM (`data.tag`, `br.tbl`, `data.get`) |
| `data`, `effect`, `class`, `instance` as exprs    | done      | done        | partial | partial         | Strong semantic support exists, but whole-language lowering/runtime is not complete |
| `perform`, `handle`, `resume`                     | done      | done        | done | done              | Handler clauses bind params as written (`op(args, k) => ...`), `value => ...` has an implicit `value` binder, and `k` is typed as `op_result ~> handled_result`; lowering emits handler objects plus `hdl.push/hdl.pop`, `eff.invk`, and `eff.resume` |
| Static imports (`import "..."`)                   | done      | done        | done | done              | Static import discovery, module keys, and session/project integration exist |
| Dynamic imports (`import expr`)                   | done      | done        | partial | partial         | Expression form exists; static graph participation is intentionally limited |
| Export and opaque module surface                  | done      | done        | partial | partial         | Export collection and semantic module surfaces exist; full hiding/runtime implications remain reduced |
| Imported module member typing                     | n/a       | done        | done | done              | Imported globals and generic callables compile through semantic module surfaces |
| Destructured module imports and aliases           | done      | done        | done | done              | Destructured imported value aliases and imported class/effect alias hydration are covered end-to-end for non-runtime compilation |
| Class and effect laws                             | done      | done        | partial | missing         | Law surface is parsed, lowered, and tracked semantically; no runtime/property-check execution |

## Types, Constraints, And Effects

| Feature                              | Parse/AST | Resolve/HIR | Sema | Backend/Toolchain | Notes                                                                |
| ------------------------------------ | --------- | ----------- | ---- | ----------------- | -------------------------------------------------------------------- |
| Named types and application (`T[A]`) | done      | done        | partial | partial         | Explicit generic type application works; type system remains reduced-core overall |
| Functions (`->`, `~>`)               | done      | done        | partial | partial         | Function kinds and signature-side effect rows exist                  |
| Tuples and products                  | done      | done        | partial | partial         | Tuple checking exists; broader product-system completeness is still reduced |
| Anonymous sums (`+`)                 | done      | done        | partial | partial         | Represented semantically, but not fully lowered through the whole backend |
| Arrays (`[]T`, `[n]T`)               | done      | done        | partial | partial         | Core shape exists; shape/dimension completeness remains reduced      |
| `mut T`                              | done      | done        | partial | partial         | Writable-type surface exists; enforcement is not a full ownership system |
| `where` constraints (`T :`, `T <:`)  | done      | done        | partial | partial         | Constraint lowering and solving exist; the overall type system is still reduced-core |
| Open effect rows (`with { ... }`)    | done      | done        | partial | partial         | Named open remainders and declared-effect checks exist; backend/runtime story is still reduced |
| Imported generic exports             | n/a       | done        | done | done              | Imported generic callable uses now compile through `music_session` end-to-end |
| Instance coherence across imports    | n/a       | done        | done | n/a              | Reachable exported instances participate in sema coherence           |

## Attributes, FFI, And Metaprogramming

| Feature                            | Parse/AST | Resolve/HIR | Sema | Backend/Toolchain | Notes                                                                  |
| ---------------------------------- | --------- | ----------- | ---- | ----------------- | ---------------------------------------------------------------------- |
| Attribute syntax and data-only args | done     | done        | done | n/a               | `@link/@when/@repr/@layout/@diag.*` argument-model validation exists   |
| `foreign` declaration surface      | done      | done        | done | done              | Foreign declarations and direct foreign calls lower into IR and SEAM metadata end-to-end |
| `export foreign` surface           | done      | done        | partial | partial         | Declared surface exists in docs/grammar; backend/runtime completeness is still reduced |
| `@link` validation                 | done      | done        | done | partial           | Invalid targets are diagnosed; runtime linking is outside the current non-runtime stack |
| `@when` target gating              | done      | done        | done | partial           | Target metadata is modeled semantically; end-to-end target selection remains reduced |
| `@repr` and `@layout` surface      | done      | done        | done | partial           | Layout-sensitive metadata exists; full runtime ABI contract is still reduced |
| Compiler-only `@musi.*` attrs      | done      | done        | partial | partial         | Reserved surface exists; backend-specific meaning remains selective    |
| Inert metadata attrs               | done      | done        | partial | n/a             | Preserved as metadata; not all downstream consumers exist yet          |
| Quote as first-class syntax        | done      | done        | done | partial           | Frontend and sema support exist; emitter/runtime support is not the reduced-core focus |
| Splice forms `#name/#()/#[]`       | done      | done        | done | partial           | Valid inside quote; backend/runtime execution path is not complete     |

## SEAM, Session, And Project Integration

| Feature                                        | Status | Notes                                                                       |
| ---------------------------------------------- | ------ | --------------------------------------------------------------------------- |
| SEAM contract (`music_bc`)                     | done   | Artifact tables, descriptors, opcode families, and structural validation exist; `data.new` uses `type_len`, `data.get/data.set` exist, and `hdl.push` takes an effect id (handler object is a stack value) |
| SEAM text transport (`music_assembly`)         | done   | Text format, parser, formatter, and validation exist                        |
| SEAM binary transport (`music_assembly`)       | done   | Binary encode/decode and validation exist                                   |
| Sema-to-IR lowering (`music_ir`)               | done   | Codegen-facing facts and owned executable IR exist                          |
| IR-to-SEAM emission (`music_emit`)             | partial | Reduced-core emission includes records, variants, and effects (`data.*`, `br.tbl`, `hdl.*`, `eff.*`); remaining gaps are outside the current non-runtime completion bar (for example, full sums, richer data-field op surface, and runtime execution) |
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
| Full-language backend parity | partial | The compiler path exists, but emission is still reduced-core overall |
