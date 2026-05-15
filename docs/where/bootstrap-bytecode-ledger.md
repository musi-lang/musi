# Bootstrap and Bytecode Ledger

This ledger records the frozen `0.1.0` host-language baseline for Musi syntax,
SEAM bytecode, and compiler bootstrapping boundaries.

Freeze governance artifacts:

- policy: `docs/__smallcore__/freeze-policy.md`
- manifest: `docs/__smallcore__/freeze-manifest.toml`
- checkpoint log: `docs/__smallcore__/checkpoint-log.md`
- freeze-exception RFC process: `docs/__smallcore__/freeze-exception-rfc.md`

## 0.1.0 Canon

- Source syntax lives in `grammar/MusiParser.g4`, `grammar/MusiLexer.g4`,
  `grammar/Musi.abnf`, and `specs/language/syntax.md`.
- SEAM bytecode lives in `specs/seam/bytecode.md`, with implementation tables in
  `crates/music_seam/src/opcode/table/`.
- SEAM lowering boundaries live in `specs/seam/lowering.md`.
- SEAM domain features live in `specs/seam/domains.md`.
- Rust compiler/runtime crate ownership lives in `docs/where/workspace-map.md`.

## 0.1.0 Frozen Surfaces

The `0.1.0` freeze anchors these surfaces:

- Source delimiters, expression sequencing, binding, assignment, equality,
  function syntax, fixed operator spellings, and source attributes.
- SEAM dotted mnemonic grammar, mnemonic roots, opcode families, numeric opcode
  positions, operand kinds, stack type lists, and branch stack rules.
- SEAM domain names: `managed`, `native`, `link`, and `introspect`.
- Lowering rule that source-level constructs break before `.seam`
  emission.

## Bytecode Contract Baseline

The `0.1.0` line tracks format, descriptor, verifier, root-map, source-map, and
archive contracts from `docs/__smallcore__` as host-language-owned contracts.
Mnemonic display uses action-first order for current public opcodes.

## Bootstrapping Boundary

Musi compiler bootstrapping moves compiler-owned source into Musi modules that
compile through the existing Rust host pipeline. The boundary is source-shaped
and acyclic:

1. Syntax data and parser-facing helper APIs.
2. Module/import graph helpers.
3. Name binding tables.
4. Typed surface and diagnostic data.
5. IR construction helpers.
6. SEAM emission helpers.

Each layer depends only on earlier layers and stable `musi:*` foundation
modules. Compiler-in-Musi modules avoid cycles between source packages so the
graph stays compatible with the current package resolver and self-hosting path.

Bootstrap phase tracking for compiler-side preparation lives in
`docs/where/compiler-bootstrap-roadmap.md`.

## Package Placement

`packages/` is the Musi-source analog of Rust `crates/` and Node.js
`packages/`. The `0.1.0` split is:

- Rust-owned runtime and host substrate: VM, runtime, native host, binary
  transport, and embedding boundary.
- Musi-owned compiler frontend: syntax-facing data, module records, name
  binding, typed surface, diagnostics, IR construction, and SEAM emission
  helpers.

Compiler-in-Musi source belongs under `packages/`, with the exact package names
and layer graph chosen from the acyclic bootstrap boundary.

This mirrors the `javac` / `java` split: the compiler frontend can be written in
the language it compiles, while the runtime VM stays in the host implementation
language.

The `packages/` graph stays intentionally undecided in this ledger. Package
names, dependency layers, and ownership boundaries are recorded only through
explicit design decisions.

## Language Shape

The `0.1.0` syntax canon uses these source rules:

- Modules are records.
- Everything is an expression.
- A statement is an expression at top level with a mandatory semicolon.
- The language stays small, powerful, embeddable, and scriptable.

## Bytecode Completeness Bar

For `0.1.0`, SEAM bytecode completeness means supported source operations lower
without emergency redesigns. Reserved opcode space stays available for
extension room, and the core design includes the primitive operations needed for
current language semantics, embedding, scripting, module records, calls, data
layout, control flow, native boundaries, and runtime introspection.

The primitive set stays grounded in what CPUs and efficient interpreters
fundamentally care about:

- move data between constants, locals, globals, fields, elements, and the stack
- compute scalar arithmetic, boolean operations, and comparisons
- branch, return, and preserve exact control-flow stack contracts
- call direct targets, indirect function values, tail targets, and foreign ABI
  edges
- allocate or construct runtime values through explicit layouts
- cross module, metadata, native, and embedding boundaries through declared
  descriptors
- keep source conveniences in lowering, library helpers, or domain contracts
  when they are not primitive machine/VM transitions

## Source Operation Coverage

Current source operations map to SEAM primitives like this:

| Source operation group                               | SEAM coverage                                                    |
| ---------------------------------------------------- | ---------------------------------------------------------------- |
| Top-level statements, final expressions, sequencing  | procedures, locals, globals, `ret`                               |
| `let`, reassignment, mutable places                  | `ld.loc`, `st.loc`, `ld.glob`, `st.glob`                         |
| Integer literals and constant payloads               | `ld.c.i4`, `ld.c`, constant table                                |
| Arithmetic and strict boolean operators              | `add`, `sub`, `mul`, `div.s`, `rem.s`, `and`, `or`, `xor`, `not` |
| Equality and ordering                                | `cmp.eq`, `cmp.ne`, `cmp.lt`, `cmp.gt`, `cmp.le`, `cmp.ge`       |
| `if`, `match`, refutable `let`, guards               | `br`, `br.z`, `br.tbl`, field loads, `cmp.*`                     |
| Functions, lambdas, closures, calls, pipelines       | `call`, `new.fn`, `call.ind`, locals                             |
| Records, modules-as-records, tuples, variants        | `new.obj`, `ld.fld`, `st.fld`, type/layout tables                |
| Arrays, indexing, length                             | `new.arr`, `ld.elem`, `st.elem`, `ld.len`                        |
| Imports, exports, dynamic module records             | artifact imports/exports, `ld.mod.dyn`, `ld.exp.dyn`             |
| `@foreign`, native boundaries, embedding             | foreign descriptors, `ld.ffi`, `call.ffi`, SEAM domains          |
| Runtime type checks and type values                  | `ld.type`, `is.inst`, `cast`                                     |
| `known`, templates, syntax services                  | syntax constants, metadata, `musi:syntax` host hooks             |
| `yield`, `defer`, `pin`, unsafe runtime consequences | explicit helper calls, native/runtime modules, domains           |

Completeness review proves each source operation either lowers to one of these
primitives or has a deliberate helper/runtime-module contract.

## Lowering Contracts

These source operations have lowering contracts in `specs/seam/lowering.md`:

- Modules are record-shaped values plus artifact metadata; dynamic lookup uses
  `ld.mod.dyn` and `ld.exp.dyn`.
- `in`, ranges, and `??` lower to comparisons, variant tests, branches, and
  typed library/runtime helpers.
- `known` is Musi's compile-time evaluation boundary, analogous to Zig
  `comptime` and C++ `constexpr` / `consteval`; it emits constants, syntax
  values, generated modules, or ordinary runtime values.
- `yield` lowers to explicit runtime suspension state and driver helper calls.
- `defer` lowers to explicit cleanup calls on every expression-block exit path.
- `pin` lowers to native-domain pin lease helpers with lexical release points.
- `unsafe` allows native-domain descriptors while emitted code stays ordinary
  SEAM calls, objects, and descriptors.
- Templates, runes, spreads, and destructuring lower through constants,
  helper/runtime contracts, field/element loads, branches, and stores.
- Export, hidden, and attributes are artifact metadata unless they affect a
  verifier-visible descriptor or lowering contract.

## Mnemonic Naming Baseline

Current public display names use action-first order:

- action-first: `ld.loc`, `st.loc`, `ld.fld`, `call.ind`, `ld.ffi`, `new.obj`
- bare action or predicate: `add`, `sub`, `ret`, `cast`, `is.inst`
- action-first: `ld.mod.dyn`, `ld.exp.dyn`

`0.1.0` naming policy uses these constraints:

- Module dynamic access uses `ld.mod.dyn` and `ld.exp.dyn`.
- Mnemonic roots describe CPU/VM primitives first, with domains carried by
  operands and descriptors when possible.
- Each primitive operation keeps one canonical spelling.

## Current Host Hooks

- `crates/musi_foundation/modules/syntax.ms` exposes `musi:syntax` helpers for
  syntax evaluation and module registration.
- `crates/musi_foundation/src/registry.rs` embeds foundation modules and exposes
  public `musi:*` specs.
- `crates/musi_project/src/project/session.rs` builds project sessions, extends
  foundation imports, and registers project module text.
- `crates/music_session/src/session/compile.rs` owns the current end-to-end
  host compile path from module keys to artifacts, bytes, and text.

## 0.1.0 Line Tracking

- Conformance coverage includes mnemonic/code positions, operand shapes, and
  stack effects from `specs/seam/bytecode.md`.
- Syntax and SEAM status fields stay aligned with implementation and specs.
- Compiler-in-Musi package graph additions follow the acyclic bootstrap boundary
  and explicit design decisions.
- Bytecode completeness stays tied to current source operation coverage.
- Opcode naming changes run through explicit mnemonic review before numeric
  table updates.
- Frozen-surface semantic changes require approved
  `docs/__smallcore__/freeze-exception-rfc.md` records before merge.
