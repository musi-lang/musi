# Feature Matrix (`crates/`)

This document tracks language and toolchain coverage across the canonical compiler rewrite in `crates/`.

The canonical feature definitions live in:

- `grammar/Musi.abnf`
- `grammar/Musi.g4`
- `docs/what/language/syntax.md`
- `docs/what/language/type-system.md`
- `docs/what/language/effect-system.md`
- `docs/what/language/metaprogramming.md`
- `docs/what/language/compiler-attributes.md`
- `docs/what/language/ffi.md`

Primary implementation truth comes from:

- `Cargo.toml`
- `docs/why/compiler-architecture.md`
- `docs/reference/public-api.md`
- current tests in `music_syntax`, `music_resolve`, `music_sema`, `music_emit`, `music_session`, and `musi_project`

Legend:

| Emoji | State   | Meaning                                                             |
| ----- | ------- | ------------------------------------------------------------------- |
| ✅     | done    | Implemented in the canonical `crates` workspace and backed by tests |
| 🟡     | partial | Implemented but incomplete/limited across phases                    |
| ❌     | missing | Not implemented in the canonical `crates` stack                     |
| ➖     | n/a     | Not applicable to this layer                                        |

This matrix is current implementation truth. It does not claim architectural completion beyond the implemented and tested `crates` stack.

Notes are descriptive: they reflect current `crates` behavior (accepted vs diagnosed), not language decisions.

## Syntax And Literals

| Feature                             | Lex | Parse/AST | Resolve/HIR | Sema | Notes                                                                      |
| ----------------------------------- | --- | --------- | ----------- | ---- | -------------------------------------------------------------------------- |
| Identifiers (plain/escaped)         | ✅   | ✅         | ✅           | ✅    |                                                                            |
| Symbolic operators (`SymOp`)        | ✅   | ✅         | ✅           | ✅    | Symbolic infix forms resolve and typecheck like callable use               |
| Integer literals (bases, `_`)       | ✅   | ✅         | ✅           | ✅    | Type is `Int`; numeric range modeling is not implemented in sema           |
| Float literals (incl. exponent)     | ✅   | ✅         | ✅           | ✅    | Type is `Float`; no range/NaN-policy modeling in sema                      |
| Strings                             | ✅   | ✅         | ✅           | ✅    | Escape decoding is covered in `music_syntax::string_lit`                   |
| Template strings with interpolation | ✅   | ✅         | ✅           | ✅    | Interpolations are ordinary expressions and typecheck as `String`          |
| Runes                               | ✅   | ✅         | ✅           | ✅    | Typed as `Int` codepoints                                                  |
| Comments and trivia                 | ✅   | ✅         | ✅           | ➖    | Trivia is preserved in the syntax tree                                     |
| Quote and splice                    | ✅   | ✅         | ✅           | ✅    | Parser diagnoses splice outside quote; sema types quote/splice as `Syntax` |

## Core Expressions And Modules

| Feature                                            | Parse/AST | Resolve/HIR | Sema | IR  | Emit/SEAM | Notes                                                                                                                                                                                                                                         |
| -------------------------------------------------- | --------- | ----------- | ---- | --- | --------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Sequences (`;`)                                    | ✅         | ✅           | ✅    | ✅   | ✅         | Top-level statements lower as sequence expressions                                                                                                                                                                                            |
| `let` bindings                                     | ✅         | ✅           | ✅    | ✅   | ✅         | Destructuring patterns (`(x, y)`, `[x, y]`, `{x, y}`) compile end-to-end; generic `let` uses explicit type params, not implicit HM-style generalization                                                                                       |
| `let rec`                                          | ✅         | ✅           | ✅    | ✅   | ✅         | Local recursive callable bindings, including captured local recursion, now typecheck, lower, and emit end-to-end                                                                                                                              |
| Writable locations (`mut expr`)                    | ✅         | ✅           | ✅    | ✅   | ✅         | Names bind immutably; mutation is performed by writing into `mut` locations via `:=`                                                                                                                                                          |
| Assignment (`:=`)                                  | ✅         | ✅           | ✅    | ✅   | ✅         | Local names, globals, indexed sequence elements, and record fields compile end-to-end                                                                                                                                                         |
| Calls                                              | ✅         | ✅           | ✅    | ✅   | ✅         | Direct named, imported, generic, foreign, and higher-order closure calls compile end-to-end in the non-runtime backend                                                                                                                        |
| Field/index/update access (`.`, `.[`, `.{`)        | ✅         | ✅           | ✅    | ✅   | ✅         | Imported module members, single-index and multi-index sequence get/set, record field projection, and record update compile end-to-end (`seq.get`, `seq.getn`, `seq.set`, `seq.setn`)                                                          |
| Variant constructors (`.Tag(...)`)                 | ✅         | ✅           | ✅    | ✅   | ✅         | Constructor type is inferred from unique matching `data` variant tag in scope; ambiguous tags require disambiguation via annotation                                                                                                           |
| `case ... of` with guards                          | ✅         | ✅           | ✅    | ✅   | ✅         | Literal, wildcard, bind, tuple, array, and structural variant patterns with guards compile end-to-end through SEAM (`data.tag`, `br.tbl`, `data.get`)                                                                                         |
| `data`, `effect`, `class`, `instance` declarations | ✅         | ✅           | ✅    | ✅   | ✅         | Declaration forms are supported at module scope; using them as value expressions is a semantic error (not a runtime feature)                                                                                                                  |
| `perform`, `handle`, `resume`                      | ✅         | ✅           | ✅    | ✅   | ✅         | Handler clauses bind params as written (`op(args, k) => ...`), `value => ...` has an implicit `value` binder, and `k` is typed as `op_result ~> handled_result`; lowering emits `hdl.push/hdl.pop`, `eff.invk`, and `eff.resume`              |
| Static imports (`import "..."`)                    | ✅         | ✅           | ✅    | ✅   | ✅         | Static import discovery, module keys, and session/project integration exist                                                                                                                                                                   |
| Dynamic imports (`import expr`)                    | ✅         | ✅           | ✅    | ✅   | ✅         | Static `import "..."` stays compile-time-only; non-static `import expr` preserves an explicit IR node and emits `mod.load`. Dynamic import sites do not add static graph edges, while runtime loading now runs through `VmLoader` / `musi_rt` |
| Exports and opaque modules                         | ✅         | ✅           | ✅    | ✅   | ✅         | Export collection, opaque marking, and SEAM metadata emission exist; runtime export hiding is not implemented                                                                                                                                  |
| Imported module member typing                      | ➖         | ✅           | ✅    | ✅   | ✅         | Imported globals and generic callables compile through semantic module surfaces                                                                                                                                                               |
| Destructured module imports and aliases            | ✅         | ✅           | ✅    | ✅   | ✅         | Destructured imported value aliases and imported class/effect alias hydration are covered end-to-end for non-runtime compilation                                                                                                              |
| Class and effect laws                              | ✅         | ✅           | ✅    | ✅   | ✅         | Laws are parsed, typechecked, and emitted as metadata; no runtime/property-check execution                                                                                                                                                    |

## Types, Constraints, And Effects

| Feature                              | Parse/AST | Resolve/HIR | Sema | IR  | Emit/SEAM | Notes                                                                                                               |
| ------------------------------------ | --------- | ----------- | ---- | --- | --------- | ------------------------------------------------------------------------------------------------------------------- |
| Named types and application (`T[A]`) | ✅         | ✅           | ✅    | ✅   | ✅         | Explicit generic type application works and is emitted in exported signature metadata                               |
| Functions (`->`, `~>`)               | ✅         | ✅           | ✅    | ✅   | ✅         | Function kinds exist; exported signature types and effect rows are emitted in exported signature metadata           |
| Tuples and products                  | ✅         | ✅           | ✅    | ✅   | ✅         | Tuple types and tuple expressions compile end-to-end in the non-runtime backend                                     |
| Anonymous sums (`+`)                 | ✅         | ✅           | ✅    | ✅   | ✅         | Sum types are represented semantically and emitted in exported signature metadata                                   |
| Arrays (`Array[T, n]`)               | ✅         | ✅           | ✅    | ✅   | ✅         | Array literals and dimension-argument forms typecheck                                                               |
| Array spread (`...expr` in `[...]`)  | ✅         | ✅           | ✅    | ✅   | ✅         | Tuple and fixed-dimension array spreads expand into indexed reads; 1D spreads with unknown dims lower via `seq.cat` |
| `mut T`                              | ✅         | ✅           | ✅    | ✅   | ✅         | Writable types are enforced for write-through assignment (`base.[i] :=`, `base.field :=`)                           |
| Type test / cast (`:?`, `:?>`)       | ✅         | ✅           | ✅    | ✅   | ✅         | Lowers into `TyTest` / `TyCast` IR and emits `TyChk` / `TyCast` opcodes                                             |
| `where` constraints (`T :`, `T <:`)  | ✅         | ✅           | ✅    | ✅   | ✅         | Constraint lowering and solving exist; constraints are emitted in exported signature metadata                       |
| Open effect rows (`with { ... }`)    | ✅         | ✅           | ✅    | ✅   | ✅         | Open rows and declared-effect checks exist; effect rows are emitted in exported signature metadata                  |
| Imported generic exports             | ➖         | ✅           | ✅    | ✅   | ✅         | Imported generic callable uses now compile through `music_session` end-to-end                                       |
| Instance coherence across imports    | ➖         | ✅           | ✅    | ➖   | ➖         | Reachable exported instances participate in sema coherence                                                          |

## Attributes, FFI, And Metaprogramming

| Feature                             | Parse/AST | Resolve/HIR | Sema | IR  | Emit/SEAM | Notes                                                                                                                                                  |
| ----------------------------------- | --------- | ----------- | ---- | --- | --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Attribute syntax and data-only args | ✅         | ✅           | ✅    | ➖   | ➖         | `@link/@when/@repr/@layout/@diag.*` argument-model validation exists                                                                                   |
| `foreign` declarations              | ✅         | ✅           | ✅    | ✅   | ✅         | Foreign declarations and direct foreign calls lower into IR and SEAM metadata end-to-end                                                               |
| `export foreign`                    | ✅         | ✅           | ✅    | ✅   | ✅         | `export foreign "abi" ( ... )` is carried into SEAM artifact foreign descriptors and text/binary transports                                            |
| `@link` validation                  | ✅         | ✅           | ✅    | ✅   | ✅         | Invalid targets are diagnosed; foreign descriptors carry `link` + `symbol` metadata (runtime linking is out-of-scope)                                  |
| `@when` target gating               | ✅         | ✅           | ✅    | ✅   | ✅         | Gated foreign declarations are excluded from IR lowering and SEAM emission for the active target                                                       |
| `@repr` and `@layout` attrs         | ✅         | ✅           | ✅    | ✅   | ✅         | Layout-sensitive metadata is carried into SEAM artifacts (`.data` descriptors); runtime ABI execution is not implemented                                 |
| Compiler-only `@musi.*` attrs       | ✅         | ✅           | ✅    | ✅   | ✅         | Reserved attrs exist and are emitted as metadata; consumers are toolchain-defined                                                                      |
| Inert metadata attrs                | ✅         | ✅           | ✅    | ✅   | ✅         | Preserved and emitted as metadata; downstream consumers are optional                                                                                   |
| Quote as first-class syntax         | ✅         | ✅           | ✅    | ✅   | ✅         | Parser enforces splice placement and sema types quote as `Syntax`; lowering preserves quote as emitted syntax data rather than staged executable code   |
| Splice forms `#name/#()/#[]`        | ✅         | ✅           | ✅    | ✅   | ✅         | Valid only inside quote at parse level; splice survives lowering and emission as syntax data inside quoted code                                        |

## SEAM, Session, And Project Integration

| Feature                                          | Status | Notes                                                                                                                                                                                                      |
| ------------------------------------------------ | ------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| SEAM contract (`music_bc`)                       | ✅      | Artifact tables, descriptors, opcode families, and structural validation exist; `data.new` uses `type_len`, `data.get/data.set` exist, and `hdl.push` takes an effect id (handler object is a stack value) |
| SEAM text transport (`music_assembly`)           | ✅      | Text format, parser, formatter, and validation exist                                                                                                                                                       |
| SEAM binary transport (`music_assembly`)         | ✅      | Binary encode/decode and validation exist                                                                                                                                                                  |
| Sema-to-IR lowering (`music_ir`)                 | ✅      | IR covers multi-index access, dynamic imports, syntax values, first-class type values, record case patterns, local recursive callables, and dynamic module export lookup                                 |
| IR-to-SEAM emission (`music_emit`)               | ✅      | Emits the current IR into SEAM artifacts and opcode streams (`data.*`, `br.tbl`, `hdl.*`, `eff.*`, `seq.*`, `mod.load`, `ty.id`) without generic unsupported-expression fallback                           |
| Module compilation through `music_session`       | ✅      | Artifact, bytes, and text outputs exist                                                                                                                                                                    |
| Reachable entry-graph compilation                | ✅      | `music_session` compiles the static-import closure                                                                                                                                                         |
| Parse/resolve/sema/IR/emit session caching       | ✅      | Cached phase products and edit invalidation exist                                                                                                                                                          |
| Package/workspace loading (`musi_project`)       | ✅      | `musi.json`, workspaces, lockfiles, registry cache, and package-aware compile exist                                                                                                                        |
| Package import remapping and registry resolution | ✅      | `musi_project` builds the session view used for package-aware compilation                                                                                                                                  |

## Runtime / VM (Planned Or Missing)

| Feature                                  | Status | Notes                                                                                                                                                                                                                                                                                             |
| ---------------------------------------- | ------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `musi_vm` crate                          | ✅      | `crates/musi_vm` now loads `.seam` bytes into `Program`, executes core SEAM through `Vm`, and exposes host-owned embedding hooks                                                                                                                                                                  |
| `musi_native` crate                      | ✅      | `crates/musi_native` owns the repo-owned host/world boundary behind the runtime path, including registered foreign/effect handlers, `musi:test` report collection, cfg-selected desktop dispatch for macOS/Linux/Windows, explicit unsupported-target rejection, and embedding fallback delegation |
| `musi_rt` crate                          | ✅      | `crates/musi_rt` now composes `music_session` and `musi_vm` into one source-aware runtime service                                                                                                                                                                                                 |
| SEAM runtime execution / VM              | ✅      | SEAM runs end-to-end through `musi_vm` and `musi_rt`, including dynamic module loading, handled effects, `resume`, and default foreign/effect handler registration; perf/GC maturity is tracked separately                                                                                        |
| Runtime dynamic imports (module loading) | ✅      | `mod.load` now executes through `VmLoader`, initializes one cached module instance once per spec, and supports runtime export lookup/call through module handles                                                                                                                                  |
| Runtime execution for quote/splice       | ✅      | `musi_vm` keeps syntax as runtime data. `musi_rt` executes syntax through expression/module compilation services rather than VM-native staged execution                                                                                                                             |

## First-Party Packages

| Feature                       | Status | Notes                                                                                                                                                                                                                                              |
| ----------------------------- | ------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `@std/*` package family       | ✅      | First-party packages live in `packages/std` with family imports, a root `@std` hub, rebuilt `assert`/`bytes`/`math`/`option`/`result`/`testing` families, and co-located tests compiled through project/runtime tooling                            |
| `musi:*` foundation namespace | ✅      | `musi_foundation` owns the canonical Rust-side registry for the foundation inventory: `musi:test` for package-test events and `musi:syntax` for source-visible syntax evaluation/module registration without hidden compiler/package magic         |

## Backends (Planned Or Missing)

| Feature                      | Status | Notes                                                                                                                                                                                                                               |
| ---------------------------- | ------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Native/JIT backend           | ❌      | `music_jit` is planned but not implemented                                                                                                                                                                                          |
| Full-language backend parity | ✅      | Canonical SEAM backend path now spans frontend, IR, emission, and runtime execution, including dynamic imports, syntax values, first-class type values, and captured local `let rec`; planned native/JIT work is tracked separately |
