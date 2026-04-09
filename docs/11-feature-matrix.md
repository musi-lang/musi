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

Legend:

| Emoji | State   | Meaning                                                                 |
| ----- | ------- | ----------------------------------------------------------------------- |
| ✅    | done    | Implemented in the canonical `crates_new` workspace and backed by tests |
| 🟡    | partial | Implemented but incomplete/limited across phases                         |
| ❌    | missing | Not implemented in the canonical `crates_new` stack                      |
| ➖    | n/a     | Not applicable to this layer                                             |

This matrix is language-first. It does not claim runtime or JIT completion.

Notes are descriptive: they reflect current `crates_new` behavior (accepted vs diagnosed), not language decisions.

## Surface Syntax And Literals

| Feature                                  | Lex  | Parse/AST | Resolve/HIR | Sema | Notes                                                                      |
| ---------------------------------------- | ---- | --------- | ----------- | ---- | -------------------------------------------------------------------------- |
| Identifiers (plain/escaped)              | ✅   | ✅        | ✅          | ✅   |                                                                            |
| Symbolic operators (`SymOp`)             | ✅   | ✅        | ✅          | ✅   | Symbolic infix surface resolves and typechecks like callable use           |
| Integer literals (bases, `_`)            | ✅   | ✅        | ✅          | ✅   | Type is `Int`; numeric range modeling is not implemented in sema           |
| Float literals (incl. exponent)          | ✅   | ✅        | ✅          | ✅   | Type is `Float`; no range/NaN-policy modeling in sema                      |
| Strings                                  | ✅   | ✅        | ✅          | ✅   | Escape decoding is covered in `music_syntax::string_lit`                   |
| Template strings with interpolation      | ✅   | ✅        | ✅          | ✅   | Interpolations are ordinary expressions and typecheck as `String`          |
| Runes                                    | ✅   | ✅        | ✅          | ✅   | Typed as `Int` codepoints                                                  |
| Comments and trivia                      | ✅   | ✅        | ✅          | ➖   | Trivia is preserved in the syntax tree                                     |
| Quote and splice surface                 | ✅   | ✅        | ✅          | ✅   | Parser diagnoses splice outside quote; sema types quote/splice as `Syntax` |

## Core Expressions And Modules

| Feature                                            | Parse/AST | Resolve/HIR | Sema | IR | Emit/SEAM | Notes |
| -------------------------------------------------- | --------- | ----------- | ---- | -- | --------- | ----- |
| Sequences (`;`)                                    | ✅        | ✅          | ✅   | ✅ | ✅        | Top-level statements lower as sequence expressions |
| `let` bindings                                     | ✅        | ✅          | ✅   | ✅ | ✅        | Destructuring patterns (`(x, y)`, `[x, y]`, `{x, y}`) compile end-to-end; generic `let` uses explicit type params, not implicit HM-style generalization |
| `let rec`                                          | ✅        | ✅          | ✅   | ✅ | ✅        | Local recursive callable bindings, including captured local recursion, now typecheck, lower, and emit end-to-end |
| Writable locations (`mut expr`)                    | ✅        | ✅          | ✅   | ✅ | ✅        | Names bind immutably; mutation is performed by writing into `mut` locations via `:=` |
| Assignment (`:=`)                                  | ✅        | ✅          | ✅   | ✅ | ✅        | Local names, globals, indexed sequence elements, and record fields compile end-to-end |
| Calls                                              | ✅        | ✅          | ✅   | ✅ | ✅        | Direct named, imported, generic, foreign, and higher-order closure calls compile end-to-end in the non-runtime backend |
| Field/index/update access (`.`, `.[`, `.{`)        | ✅        | ✅          | ✅   | ✅ | ✅        | Imported module members, single-index and multi-index sequence get/set, record field projection, and record update compile end-to-end (`seq.get`, `seq.getn`, `seq.set`, `seq.setn`) |
| Variant constructors (`.Tag(...)`)                 | ✅        | ✅          | ✅   | ✅ | ✅        | Constructor type is inferred from unique matching `data` variant tag in scope; ambiguous tags require disambiguation via annotation |
| `case ... of` with guards                          | ✅        | ✅          | ✅   | ✅ | ✅        | Literal, wildcard, bind, tuple, array, and structural variant patterns with guards compile end-to-end through SEAM (`data.tag`, `br.tbl`, `data.get`) |
| `data`, `effect`, `class`, `instance` declarations | ✅        | ✅          | ✅   | ✅ | ✅        | Declaration forms are supported at module scope; using them as value expressions is a semantic error (not a runtime feature) |
| `perform`, `handle`, `resume`                      | ✅        | ✅          | ✅   | ✅ | ✅        | Handler clauses bind params as written (`op(args, k) => ...`), `value => ...` has an implicit `value` binder, and `k` is typed as `op_result ~> handled_result`; lowering emits `hdl.push/hdl.pop`, `eff.invk`, and `eff.resume` |
| Static imports (`import "..."`)                    | ✅        | ✅          | ✅   | ✅ | ✅        | Static import discovery, module keys, and session/project integration exist |
| Dynamic imports (`import expr`)                    | ✅        | ✅          | ✅   | ✅ | ✅        | Static `import "..."` stays compile-time-only; non-static `import expr` preserves an explicit IR node and emits `mod.load`. Dynamic import sites still do not add static graph edges and there is no runtime module loading yet |
| Export and opaque module surface                   | ✅        | ✅          | ✅   | ✅ | ✅        | Export collection, opaque marking, and SEAM metadata emission exist; runtime hiding implications remain out of scope |
| Imported module member typing                      | ➖        | ✅          | ✅   | ✅ | ✅        | Imported globals and generic callables compile through semantic module surfaces |
| Destructured module imports and aliases            | ✅        | ✅          | ✅   | ✅ | ✅        | Destructured imported value aliases and imported class/effect alias hydration are covered end-to-end for non-runtime compilation |
| Class and effect laws                              | ✅        | ✅          | ✅   | ✅ | ✅        | Law surface is parsed, typechecked, and emitted as metadata; no runtime/property-check execution |

## Types, Constraints, And Effects

| Feature                              | Parse/AST | Resolve/HIR | Sema | IR | Emit/SEAM | Notes |
| ------------------------------------ | --------- | ----------- | ---- | -- | --------- | ----- |
| Named types and application (`T[A]`) | ✅        | ✅          | ✅   | ✅ | ✅        | Explicit generic type application works and is emitted in exported signature metadata |
| Functions (`->`, `~>`)               | ✅        | ✅          | ✅   | ✅ | ✅        | Function kinds exist; exported signature types and effect rows are emitted in exported signature metadata |
| Tuples and products                  | ✅        | ✅          | ✅   | ✅ | ✅        | Tuple types and tuple expressions compile end-to-end in the non-runtime backend |
| Anonymous sums (`+`)                 | ✅        | ✅          | ✅   | ✅ | ✅        | Sum types are represented semantically and emitted in exported signature metadata |
| Arrays (`Array[T, n]`)               | ✅        | ✅          | ✅   | ✅ | ✅        | Array literals and dimension-argument forms typecheck |
| Array spread (`...expr` in `[...]`)  | ✅        | ✅          | ✅   | ✅ | ✅        | Tuple and fixed-dimension array spreads expand into indexed reads; 1D spreads with unknown dims lower via `seq.cat` |
| `mut T`                              | ✅        | ✅          | ✅   | ✅ | ✅        | Writable types are enforced for write-through assignment (`base.[i] :=`, `base.field :=`) |
| Type test / cast (`:?`, `:?>`)       | ✅        | ✅          | ✅   | ✅ | ✅        | Lowers into `TyTest` / `TyCast` IR and emits `TyChk` / `TyCast` opcodes |
| `where` constraints (`T :`, `T <:`)  | ✅        | ✅          | ✅   | ✅ | ✅        | Constraint lowering and solving exist; constraints are emitted in exported signature metadata |
| Open effect rows (`with { ... }`)    | ✅        | ✅          | ✅   | ✅ | ✅        | Open rows and declared-effect checks exist; effect rows are emitted in exported signature metadata |
| Imported generic exports             | ➖        | ✅          | ✅   | ✅ | ✅        | Imported generic callable uses now compile through `music_session` end-to-end |
| Instance coherence across imports    | ➖        | ✅          | ✅   | ➖ | ➖        | Reachable exported instances participate in sema coherence |

## Attributes, FFI, And Metaprogramming

| Feature                             | Parse/AST | Resolve/HIR | Sema | IR | Emit/SEAM | Notes |
| ----------------------------------- | --------- | ----------- | ---- | -- | --------- | ----- |
| Attribute syntax and data-only args | ✅        | ✅          | ✅   | ➖ | ➖        | `@link/@when/@repr/@layout/@diag.*` argument-model validation exists |
| `foreign` declaration surface       | ✅        | ✅          | ✅   | ✅ | ✅        | Foreign declarations and direct foreign calls lower into IR and SEAM metadata end-to-end |
| `export foreign` surface            | ✅        | ✅          | ✅   | ✅ | ✅        | `export foreign "abi" ( ... )` is carried into SEAM artifact foreign descriptors and text/binary transports |
| `@link` validation                  | ✅        | ✅          | ✅   | ✅ | ✅        | Invalid targets are diagnosed; foreign descriptors carry `link` + `symbol` metadata (runtime linking is out-of-scope) |
| `@when` target gating               | ✅        | ✅          | ✅   | ✅ | ✅        | Gated foreign declarations are excluded from IR lowering and SEAM emission for the active target |
| `@repr` and `@layout` surface       | ✅        | ✅          | ✅   | ✅ | ✅        | Layout-sensitive metadata is carried into SEAM artifacts (`.data` descriptors); runtime ABI execution remains out of scope |
| Compiler-only `@musi.*` attrs       | ✅        | ✅          | ✅   | ✅ | ✅        | Reserved surface exists and is emitted as metadata; consumers are toolchain-defined |
| Inert metadata attrs                | ✅        | ✅          | ✅   | ✅ | ✅        | Preserved and emitted as metadata; downstream consumers are optional |
| Quote as first-class syntax         | ✅        | ✅          | ✅   | ✅ | ✅        | Parser enforces splice placement and sema types quote as `Syntax`; current lowering preserves quote as emitted syntax data, not executable staged code |
| Splice forms `#name/#()/#[]`        | ✅        | ✅          | ✅   | ✅ | ✅        | Valid only inside quote at parse level; splice now survives lowering and emission as syntax data inside the quoted surface |

## SEAM, Session, And Project Integration

| Feature                                          | Status | Notes |
| ------------------------------------------------ | ------ | ----- |
| SEAM contract (`music_bc`)                       | ✅     | Artifact tables, descriptors, opcode families, and structural validation exist; `data.new` uses `type_len`, `data.get/data.set` exist, and `hdl.push` takes an effect id (handler object is a stack value) |
| SEAM text transport (`music_assembly`)           | ✅     | Text format, parser, formatter, and validation exist |
| SEAM binary transport (`music_assembly`)         | ✅     | Binary encode/decode and validation exist |
| Sema-to-IR lowering (`music_ir`)                 | ✅     | Codegen-facing IR now covers multi-index access, dynamic imports, syntax values, first-class type values, record case patterns, and local recursive callables without routing sema-valid programs through unsupported placeholder IR |
| IR-to-SEAM emission (`music_emit`)               | ✅     | Emits the current valid IR surface into SEAM artifacts and opcode streams (`data.*`, `br.tbl`, `hdl.*`, `eff.*`, `seq.*`, `mod.load`, `ty.id`) without a generic unsupported-expression fallback |
| Module compilation through `music_session`       | ✅     | Artifact, bytes, and text outputs exist |
| Reachable entry-graph compilation                | ✅     | `music_session` compiles the static-import closure |
| Parse/resolve/sema/IR/emit session caching       | ✅     | Cached phase products and edit invalidation exist |
| Package/workspace loading (`musi_project`)       | ✅     | `musi.json`, workspaces, lockfiles, registry cache, and package-aware compile exist |
| Package import remapping and registry resolution | ✅     | `musi_project` builds the session view used for package-aware compilation |

## Runtime / VM (Planned Or Missing)

| Feature                                  | Status | Notes |
| ---------------------------------------- | ------ | ----- |
| SEAM runtime execution / VM              | ❌     | Interpreter/runtime for executing emitted opcodes does not exist |
| Runtime dynamic imports (module loading) | ❌     | `import expr` now lowers and emits `mod.load`, but there is still no runtime loader/VM support for executing it |
| Runtime execution for quote/splice       | ❌     | Quote/splice now emit as syntax data, but there is no runtime syntax evaluation/execution model yet |

## Backends (Planned Or Missing)

| Feature                       | Status | Notes |
| ----------------------------- | ------ | ----- |
| Native/JIT backend            | ❌     | `music_jit` is planned but not implemented |
| Full-language backend parity  | 🟡     | Frontend-backed constructs now lower and emit through IR and SEAM, including multi-index access, dynamic imports, syntax values, first-class type values, and captured local `let rec`; remaining gap is runtime/VM execution plus planned native/JIT backend work |
