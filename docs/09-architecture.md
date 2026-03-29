# Architecture

This document defines the semantic ownership map from Musi source to the runtime boundary.

It does not describe the current Rust crate graph. It describes the stable responsibilities the implementation must preserve.

## Design Principle

Each layer owns one transformation or one runtime responsibility.

The architecture stays honest when:

- surface syntax is defined by the language docs and grammar
- semantic analysis owns meaning, not ad hoc emitter/runtime recovery
- SEAM owns runtime contracts, not source sugar
- the runtime owns execution and embedding, not compile-time language decisions

## Locked Crate Structure

The clean-room workspace in `crates_new/` uses bounded-context crate names and this structure is locked.

Compiler and IL crates stay under `music_*`:

- `music_assembly`: text/binary IL encoding, decoding, printing, and validation
- `music_ast`: full-fidelity green/red syntax tree, syntax only, with top-level expression sequences
- `music_basic`: spans, sources, diagnostics, literals, and other non-binding foundation types
- `music_codegen`: lowering from typed IR into `music_il`
- `music_fe`: frontend orchestration over lex/parse/ast/names/sema/ir/codegen
- `music_hir`: typed high-level IR after semantic analysis
- `music_il`: VM-facing bytecode / intermediate language contract
- `music_known`: compiler-known builtins and intrinsic surface
- `music_lex`: lossless lexing and token/trivia production
- `music_names`: symbols, interning, identifiers, scopes, bindings, and name-resolution data
- `music_parse`: parsing token streams into syntax structures (`music_ast`)
- `music_sema`: type/effect/class semantic analysis and validation
- `music_session`: compiler session state, loaded sources, and shared compile context
- `music_storage`: arena/index storage and related typed storage mechanics

Project, tooling, and runtime crates stay under `musi_*`:

- `musi_project`: manifests, package/workspace model, dependency config, and task metadata
- `musi_rt`: runtime, VM, loading, and embedding boundary
- `musi_tooling`: operator, editor, and higher-level tooling integration over project/compiler crates

This naming is part of the architecture contract:

- domain nouns define the subsystem boundaries
- `music_*` owns the compiler and IL boundary
- `musi_*` owns project loading, tooling, and runtime embedding
- diagnostics stay inside `music_basic`
- symbols, interning, and identifiers stay inside `music_names`
- `music_names` and `music_sema` stay separate
- `music_il` and `music_assembly` are the locked SEAM pair

The clean-room crate layout is also locked:

- `src/` is production code only
- `benches/` is Criterion-only
- `module.rs` or `module/mod.rs` owns the module surface
- `module/tests.rs` owns unit tests and close test helpers
- `tests/` is for integration and e2e only
- no Rust file may exceed 2000 LOC, including tests and benches

The clean-room naming rule is also locked:

- public names must stay domain-specific when imported unqualified
- avoid generic names that read like std or UI vocabulary when a bounded-context name is clearer
- keep established compiler and IL terms such as `Span`, `Arena`, `Idx`, `TypeDescriptor`, and `ConstantPool`

## Ownership Chain

### 1. Source Language

The language layer owns:

- syntax
- type and effect surface
- attrs
- FFI declarations
- metaprogramming surface

These are defined by the language and reference docs, not by implementation accidents.

### 2. Parsing And Syntax

The syntax layer turns source text into a structured program representation.

It owns:

- tokenization
- grammar
- operator and guard structure
- syntax trees and source spans

It does not decide meaning beyond what is necessary to represent the parsed program correctly.

### 3. Name And Module Resolution

The resolution layer owns:

- import graph discovery
- module-level visibility
- binding and reference resolution
- compiler-owned surface seeding

It decides what names refer to. It does not decide whether the resulting program is well-typed.

### 4. Type And Effect Analysis

The semantic layer owns:

- type checking
- effect checking
- class and instance resolution
- law and attr validation where they affect language meaning
- FFI surface validation at the language boundary

Its output is the authoritative typed program representation used by lowering.

### 5. Lowering To SEAM

The lowering layer owns:

- translation from typed language constructs to SEAM
- closure, effect, typeclass, aggregate, and foreign-call lowering
- merged-program assembly where multiple modules become one executable artifact

This is where source sugar disappears and runtime contracts become explicit machine-level operations.

### 6. SEAM Contract

SEAM owns the binary and text runtime contract:

- bytecode families
- metadata tables
- runtime-visible descriptors
- loader-facing format invariants

SEAM is the handoff point between compilation and execution.

### 7. Runtime And Embedding Boundary

The runtime layer owns:

- loading `.seam` into a program object
- execution through the VM
- host effect handling
- native library and symbol resolution
- stable value inspection for embedding

The runtime boundary is documented in the runtime and SEAM docs, not redefined here.

## Compiler-Owned Surface

The compiler-owned surface stays intentionally small:

- compiler-owned language definitions
- intrinsic modules and bridges
- builtin runtime-visible type, effect, class, and variant metadata

Everything else is ordinary library or package surface:

- `@std/...` remains library surface
- user attrs outside reserved compiler namespaces remain metadata
- native and host interaction stays explicit through `foreign`, runtime host capabilities, and compiler-owned intrinsic boundaries

## Boundary Rules

To keep the architecture stable:

- parsing does not guess semantic meaning the type/effect layer should decide
- semantic analysis does not depend on emitter or VM recovery
- SEAM does not preserve removed source sugar
- the runtime does not invent a second startup model separate from top-level module execution
- embedding APIs expose runtime values and metadata, not compiler internals

## Tooling Boundary

The outer tool boundary also stays split by responsibility:

- `music` is the low-level compiler/runtime driver over source and `.seam`
- `musi` is the package-aware operator tool over manifests, entries, tasks, tests, and dependency workflows
- schema-backed tooling such as fmt/lint/bench/publish/lock belongs to the Musi framework without being confused with compiler-core behavior

That tooling split sits above the language-to-runtime ownership chain described here. It does not redefine the semantic layers underneath it.

## Reading Order

This architecture doc sits after the language, SEAM, and runtime API docs on purpose.

Read those first for:

- language surface
- SEAM VM and bytecode contract
- runtime embedding boundary

Then read this document as the ownership map across them.
